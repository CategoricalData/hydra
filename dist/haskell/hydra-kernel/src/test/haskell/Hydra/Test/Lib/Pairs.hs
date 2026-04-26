-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for hydra.lib.pairs primitives

module Hydra.Test.Lib.Pairs where
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for hydra.lib.pairs primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.pairs primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "bimap",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "transform both elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\p -> Strings.cat [
                  "(",
                  (Literals.showInt32 (Pairs.first p)),
                  ", ",
                  (Literals.showInt32 (Pairs.second p)),
                  ")"]) (Pairs.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (5, "ab"))),
                Testing.universalTestCaseExpected = ((\p -> Strings.cat [
                  "(",
                  (Literals.showInt32 (Pairs.first p)),
                  ", ",
                  (Literals.showInt32 (Pairs.second p)),
                  ")"]) (10, 2))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "with zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\p -> Strings.cat [
                  "(",
                  (Literals.showInt32 (Pairs.first p)),
                  ", ",
                  (Literals.showInt32 (Pairs.second p)),
                  ")"]) (Pairs.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (0, "hello"))),
                Testing.universalTestCaseExpected = ((\p -> Strings.cat [
                  "(",
                  (Literals.showInt32 (Pairs.first p)),
                  ", ",
                  (Literals.showInt32 (Pairs.second p)),
                  ")"]) (0, 5))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "first",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "extract first element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Pairs.first (42, "hello"))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 42)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "with zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Pairs.first (0, "world"))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 0)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative number",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Pairs.first ((-5), "test"))),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) (-5))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "second",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "extract second element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Pairs.second (42, "hello"))),
                Testing.universalTestCaseExpected = ((\s -> s) "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Pairs.second (0, ""))),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "long string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Pairs.second (123, "testing"))),
                Testing.universalTestCaseExpected = ((\s -> s) "testing")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
