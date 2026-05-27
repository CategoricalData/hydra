-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for hydra.lib.regex primitives

module Hydra.Test.Lib.Regex where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for hydra.lib.regex primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.regex primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "matches",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "exact match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "pattern match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[a-z]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partial content does not match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[a-z]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello123"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "digit pattern",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "12345"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed pattern",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[a-z]+[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello123"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty pattern matches empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty pattern does not match non-empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "star matches empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a*"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "alternation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "cat|dog"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "cat"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "alternation second",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "cat|dog"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "dog"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "alternation no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "cat|dog"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bird"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "quantifier",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ab?c"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ac"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "quantifier with optional",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.matches")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ab?c"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "find",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple find",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.find")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc123def"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "123")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.find")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abcdef"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find first",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.find")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[a-z]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "123abc456def"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "abc")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty input",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.find")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "full match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.find")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ".*"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "findAll",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple matches",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.findAll")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a1b2c3"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "1"),
                  (Core.TermLiteral (Core.LiteralString "2")),
                  (Core.TermLiteral (Core.LiteralString "3"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no matches",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.findAll")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "overlapping words",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.findAll")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[a-z]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc def ghi"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "abc"),
                  (Core.TermLiteral (Core.LiteralString "def")),
                  (Core.TermLiteral (Core.LiteralString "ghi"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.findAll")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "say hello world"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "hello")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "replace",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic replace",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replace")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "X"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc123def456"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "abcXdef456")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replace")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "X"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abcdef"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "abcdef")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replace at start",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replace")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "^[a-z]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "X"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc123"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "X123")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty replacement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replace")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc123def"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "abcdef")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "replaceAll",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replace all digits",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replaceAll")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "X"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a1b2c3"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "aXbXcX")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replaceAll")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "X"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "abc")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replace all words",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replaceAll")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[a-z]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "X"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc 123 def"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "X 123 X")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty replacement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.replaceAll")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a1b2c3"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralString "abc")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "split",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "split on comma",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.split")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a,b,c"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "split on spaces",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.split")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " +"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a b  c"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.split")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "abc")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "split on digits",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.split")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "[0-9]+"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a1b2c"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "trailing delimiter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.regex.split")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a,b,"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString ""))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
