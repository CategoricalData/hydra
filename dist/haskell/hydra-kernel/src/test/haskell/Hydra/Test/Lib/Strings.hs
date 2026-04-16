-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.strings primitives

module Hydra.Test.Lib.Strings where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Test cases for hydra.lib.strings primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.strings primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "cat",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic concatenation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "one",
                  "two",
                  "three"])),
                Testing.universalTestCaseExpected = ((\s -> s) "onetwothree")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "hello"])),
                Testing.universalTestCaseExpected = ((\s -> s) "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [])),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "with empty strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "",
                  "one",
                  "",
                  ""])),
                Testing.universalTestCaseExpected = ((\s -> s) "one")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all empty strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "",
                  "",
                  "",
                  ""])),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "\241",
                  "\19990",
                  "\127757"])),
                Testing.universalTestCaseExpected = ((\s -> s) "\241\19990\127757")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "combining characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "e",
                  "\769"])),
                Testing.universalTestCaseExpected = ((\s -> s) "e\769")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "control characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "\n",
                  "\t",
                  "\r"])),
                Testing.universalTestCaseExpected = ((\s -> s) "\n\t\r")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat [
                  "hello",
                  "\NUL",
                  "world"])),
                Testing.universalTestCaseExpected = ((\s -> s) "hello\NULworld")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "cat2",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic concatenation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "hello" "world")),
                Testing.universalTestCaseExpected = ((\s -> s) "helloworld")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty first string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "" "world")),
                Testing.universalTestCaseExpected = ((\s -> s) "world")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty second string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "hello" "")),
                Testing.universalTestCaseExpected = ((\s -> s) "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "both empty strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "" "")),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "\241" "\19990")),
                Testing.universalTestCaseExpected = ((\s -> s) "\241\19990")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "special characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "\n" "\t")),
                Testing.universalTestCaseExpected = ((\s -> s) "\n\t")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.cat2 "hello\NUL" "world")),
                Testing.universalTestCaseExpected = ((\s -> s) "hello\NULworld")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "charAt",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 0 "hello")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 104)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "middle character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 2 "hello")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 108)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "last character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 4 "hello")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 111)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single character string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 0 "a")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 97)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 0 "\241")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 241)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multi-byte unicode",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 0 "\19990")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 19990)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "second of combining pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.charAt 1 "e\769")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 769)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "fromList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic ascii string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [
                  104,
                  101,
                  108,
                  108,
                  111])),
                Testing.universalTestCaseExpected = ((\s -> s) "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty code point list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [])),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [
                  97])),
                Testing.universalTestCaseExpected = ((\s -> s) "a")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [
                  241,
                  19990,
                  127757])),
                Testing.universalTestCaseExpected = ((\s -> s) "\241\19990\127757")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "combining character sequence",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [
                  101,
                  769])),
                Testing.universalTestCaseExpected = ((\s -> s) "e\769")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "special characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [
                  10,
                  9,
                  13])),
                Testing.universalTestCaseExpected = ((\s -> s) "\n\t\r")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.fromList [
                  104,
                  0,
                  105])),
                Testing.universalTestCaseExpected = ((\s -> s) "h\NULi")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "intercalate",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "comma separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "," [
                  "one",
                  "two",
                  "three"])),
                Testing.universalTestCaseExpected = ((\s -> s) "one,two,three")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "" [
                  "a",
                  "b",
                  "c"])),
                Testing.universalTestCaseExpected = ((\s -> s) "abc")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multi-character separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate " | " [
                  "A",
                  "B",
                  "C"])),
                Testing.universalTestCaseExpected = ((\s -> s) "A | B | C")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "," [])),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single item list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "," [
                  "only"])),
                Testing.universalTestCaseExpected = ((\s -> s) "only")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty strings in list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "," [
                  "",
                  "a",
                  ""])),
                Testing.universalTestCaseExpected = ((\s -> s) ",a,")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "\127757" [
                  "link1",
                  "link2"])),
                Testing.universalTestCaseExpected = ((\s -> s) "link1\127757link2")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "newline separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.intercalate "\n" [
                  "line1",
                  "line2"])),
                Testing.universalTestCaseExpected = ((\s -> s) "line1\nline2")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "length",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.length "")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 0)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.length "a")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 1)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic word",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.length "hello")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 5)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.length "\241\19990\127757")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 3)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "combining character sequence",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.length "e\769")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 2)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "special characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\n -> Literals.showInt32 n) (Strings.length "\n\t\r")),
                Testing.universalTestCaseExpected = ((\n -> Literals.showInt32 n) 3)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "lines",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single line",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "hello world")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "hello world"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two lines",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "hello\nworld")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "hello",
                  "world"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three lines",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "one\ntwo\nthree")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "one",
                  "two",
                  "three"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just newline",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "\n")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "trailing newline",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "hello\n")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "hello"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "leading newline",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "\nhello")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  "hello"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple consecutive newlines",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "a\n\nb")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "a",
                  "",
                  "b"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode content",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "\241\n\19990")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "\241",
                  "\19990"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "tabs not split",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.lines "a\tb\nc")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "a\tb",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybeCharAt",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 104))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "middle character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 108))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "last character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 111))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "out of bounds",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative index",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "null",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null "")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null "a")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "space",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null " ")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode space",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null "\160")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "newline",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null "\n")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null "\NUL")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multi-character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\b -> Literals.showBoolean b) (Strings.null "hello")),
                Testing.universalTestCaseExpected = ((\b -> Literals.showBoolean b) False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "splitOn",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "ss" "Mississippi")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "Mi",
                  "i",
                  "ippi"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single char separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn " " "one two three")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "one",
                  "two",
                  "three"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multi-char separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "  " "a  b  c")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "a",
                  "b",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "separator not found",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "x" "hello")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "hello"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "separator at start",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "h" "hello")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  "ello"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "separator at end",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "o" "hello")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "hell",
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "leading and trailing separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn " " " one two ")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  "one",
                  "two",
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "whole string as separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "Mississippi" "Mississippi")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "consecutive separators",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn " " "a  b")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "a",
                  "",
                  "b"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple occurrences",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "l" "hello")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "he",
                  "",
                  "o"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "overlapping pattern",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "aa" "aaa")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  "a"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "" "abc")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  "a",
                  "b",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "separator on empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "x" "")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "both empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "" "")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single char both",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "a" "a")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "",
                  ""])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "\19990" "hello\19990world")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "hello",
                  "world"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode content",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "," "\241,\19990,\127757")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "\241",
                  "\19990",
                  "\127757"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "newline separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) (Strings.splitOn "\n" "line1\nline2\nline3")),
                Testing.universalTestCaseExpected = ((\xs -> Literals.showString (Strings.intercalate ", " xs)) [
                  "line1",
                  "line2",
                  "line3"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "toList",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "a")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [
                  97])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "basic word",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "hello")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [
                  104,
                  101,
                  108,
                  108,
                  111])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "\241\19990\127757")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [
                  241,
                  19990,
                  127757])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "combining character sequence",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "e\769")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [
                  101,
                  769])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "control characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "\n\t\r")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [
                  10,
                  9,
                  13])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "null character",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) (Strings.toList "h\NULi")),
                Testing.universalTestCaseExpected = ((\xs -> Strings.intercalate ", " (Lists.map (\n -> Literals.showInt32 n) xs)) [
                  104,
                  0,
                  105])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "toLower",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed case",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "Hello World")),
                Testing.universalTestCaseExpected = ((\s -> s) "hello world")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all uppercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "HELLO")),
                Testing.universalTestCaseExpected = ((\s -> s) "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all lowercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "hello")),
                Testing.universalTestCaseExpected = ((\s -> s) "hello")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "")),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "with numbers and punctuation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "Abc123, XYZ!")),
                Testing.universalTestCaseExpected = ((\s -> s) "abc123, xyz!")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "control characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "\n\t\r")),
                Testing.universalTestCaseExpected = ((\s -> s) "\n\t\r")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode accented chars",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toLower "\209\193\201\205\211\218")),
                Testing.universalTestCaseExpected = ((\s -> s) "\241\225\233\237\243\250")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "toUpper",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed case",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "hello World")),
                Testing.universalTestCaseExpected = ((\s -> s) "HELLO WORLD")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all lowercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "hello")),
                Testing.universalTestCaseExpected = ((\s -> s) "HELLO")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all uppercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "HELLO")),
                Testing.universalTestCaseExpected = ((\s -> s) "HELLO")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "")),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "with numbers and punctuation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "abc123, xyz!")),
                Testing.universalTestCaseExpected = ((\s -> s) "ABC123, XYZ!")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "control characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "\n\t\r")),
                Testing.universalTestCaseExpected = ((\s -> s) "\n\t\r")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode accented chars",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.toUpper "\241\225\233\237\243\250")),
                Testing.universalTestCaseExpected = ((\s -> s) "\209\193\201\205\211\218")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "unlines",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple lines",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.unlines [
                  "one",
                  "two",
                  "three"])),
                Testing.universalTestCaseExpected = ((\s -> s) "one\ntwo\nthree\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single line",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.unlines [
                  "hello"])),
                Testing.universalTestCaseExpected = ((\s -> s) "hello\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.unlines [])),
                Testing.universalTestCaseExpected = ((\s -> s) "")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "with empty lines",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.unlines [
                  "hello",
                  "",
                  "world"])),
                Testing.universalTestCaseExpected = ((\s -> s) "hello\n\nworld\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all empty lines",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.unlines [
                  "",
                  "",
                  ""])),
                Testing.universalTestCaseExpected = ((\s -> s) "\n\n\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unicode content",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = ((\s -> s) (Strings.unlines [
                  "\241o\241o",
                  "\19990\30028"])),
                Testing.universalTestCaseExpected = ((\s -> s) "\241o\241o\n\19990\30028\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
