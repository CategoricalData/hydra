-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.text primitives

module Hydra.Test.Lib.Text where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals

-- | Test cases for hydra.lib.text primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.text primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "decodeUtf8",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty bytes",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.decodeUtf8")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "")))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ASCII text",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.decodeUtf8")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "aGVsbG8=")))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "hello")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multi-byte UTF-8 text",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.decodeUtf8")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "w6l2w6luZW1lbnQ=")))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "\233v\233nement")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "invalid lone continuation byte",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.isLeft")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.decodeUtf8")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "gA==")))}))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "encodeUtf8",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.encodeUtf8")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary ""))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ASCII text",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.encodeUtf8")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "aGVsbG8="))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multi-byte UTF-8 text",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.text.encodeUtf8")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\233v\233nement"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.term (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "w6l2w6luZW1lbnQ="))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
