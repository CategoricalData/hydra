-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.strings"},ModuleName {unModuleName = "Strings"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.StringsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.strings primitives" $ do
  H.describe "cat" $ do
    H.it "basic concatenation" $ H.shouldBe
      (Strings.cat [
          "one",
          "two",
          "three"])
      ("onetwothree")
    H.it "single string" $ H.shouldBe
      (Strings.cat [
          "hello"])
      ("hello")
    H.it "empty list" $ H.shouldBe
      (Strings.cat [])
      ("")
    H.it "with empty strings" $ H.shouldBe
      (Strings.cat [
          "",
          "one",
          "",
          ""])
      ("one")
    H.it "all empty strings" $ H.shouldBe
      (Strings.cat [
          "",
          "",
          "",
          ""])
      ("")
    H.it "unicode strings" $ H.shouldBe
      (Strings.cat [
          "\241",
          "\19990",
          "\127757"])
      ("\241\19990\127757")
    H.it "combining characters" $ H.shouldBe
      (Strings.cat [
          "e",
          "\769"])
      ("e\769")
    H.it "control characters" $ H.shouldBe
      (Strings.cat [
          "\n",
          "\t",
          "\r"])
      ("\n\t\r")
    H.it "null character" $ H.shouldBe
      (Strings.cat [
          "hello",
          "\NUL",
          "world"])
      ("hello\NULworld")
  H.describe "cat2" $ do
    H.it "basic concatenation" $ H.shouldBe
      (Strings.cat2 "hello" "world")
      ("helloworld")
    H.it "empty first string" $ H.shouldBe
      (Strings.cat2 "" "world")
      ("world")
    H.it "empty second string" $ H.shouldBe
      (Strings.cat2 "hello" "")
      ("hello")
    H.it "both empty strings" $ H.shouldBe
      (Strings.cat2 "" "")
      ("")
    H.it "unicode characters" $ H.shouldBe
      (Strings.cat2 "\241" "\19990")
      ("\241\19990")
    H.it "special characters" $ H.shouldBe
      (Strings.cat2 "\n" "\t")
      ("\n\t")
    H.it "null characters" $ H.shouldBe
      (Strings.cat2 "hello\NUL" "world")
      ("hello\NULworld")
  H.describe "charAt" $ do
    H.it "first character" $ H.shouldBe
      (Strings.charAt 0 "hello")
      (104)
    H.it "middle character" $ H.shouldBe
      (Strings.charAt 2 "hello")
      (108)
    H.it "last character" $ H.shouldBe
      (Strings.charAt 4 "hello")
      (111)
    H.it "single character string" $ H.shouldBe
      (Strings.charAt 0 "a")
      (97)
    H.it "unicode character" $ H.shouldBe
      (Strings.charAt 0 "\241")
      (241)
    H.it "multi-byte unicode" $ H.shouldBe
      (Strings.charAt 0 "\19990")
      (19990)
    H.it "second of combining tuple2" $ H.shouldBe
      (Strings.charAt 1 "e\769")
      (769)
  H.describe "fromList" $ do
    H.it "basic ascii string" $ H.shouldBe
      (Strings.fromList [
          104,
          101,
          108,
          108,
          111])
      ("hello")
    H.it "empty code point list" $ H.shouldBe
      (Strings.fromList [])
      ("")
    H.it "single character" $ H.shouldBe
      (Strings.fromList [
          97])
      ("a")
    H.it "unicode characters" $ H.shouldBe
      (Strings.fromList [
          241,
          19990,
          127757])
      ("\241\19990\127757")
    H.it "combining character sequence" $ H.shouldBe
      (Strings.fromList [
          101,
          769])
      ("e\769")
    H.it "special characters" $ H.shouldBe
      (Strings.fromList [
          10,
          9,
          13])
      ("\n\t\r")
    H.it "null character" $ H.shouldBe
      (Strings.fromList [
          104,
          0,
          105])
      ("h\NULi")
  H.describe "intercalate" $ do
    H.it "comma separator" $ H.shouldBe
      (Strings.intercalate "," [
          "one",
          "two",
          "three"])
      ("one,two,three")
    H.it "empty separator" $ H.shouldBe
      (Strings.intercalate "" [
          "a",
          "b",
          "c"])
      ("abc")
    H.it "multi-character separator" $ H.shouldBe
      (Strings.intercalate " | " [
          "A",
          "B",
          "C"])
      ("A | B | C")
    H.it "empty string list" $ H.shouldBe
      (Strings.intercalate "," [])
      ("")
    H.it "single item list" $ H.shouldBe
      (Strings.intercalate "," [
          "only"])
      ("only")
    H.it "empty strings in list" $ H.shouldBe
      (Strings.intercalate "," [
          "",
          "a",
          ""])
      (",a,")
    H.it "unicode separator" $ H.shouldBe
      (Strings.intercalate "\127757" [
          "link1",
          "link2"])
      ("link1\127757link2")
    H.it "newline separator" $ H.shouldBe
      (Strings.intercalate "\n" [
          "line1",
          "line2"])
      ("line1\nline2")
  H.describe "length" $ do
    H.it "empty string" $ H.shouldBe
      (Strings.length "")
      (0)
    H.it "single character" $ H.shouldBe
      (Strings.length "a")
      (1)
    H.it "basic word" $ H.shouldBe
      (Strings.length "hello")
      (5)
    H.it "unicode characters" $ H.shouldBe
      (Strings.length "\241\19990\127757")
      (3)
    H.it "combining character sequence" $ H.shouldBe
      (Strings.length "e\769")
      (2)
    H.it "special characters" $ H.shouldBe
      (Strings.length "\n\t\r")
      (3)
  H.describe "lines" $ do
    H.it "single line" $ H.shouldBe
      (Strings.lines "hello world")
      ([
          "hello world"])
    H.it "two lines" $ H.shouldBe
      (Strings.lines "hello\nworld")
      ([
          "hello",
          "world"])
    H.it "three lines" $ H.shouldBe
      (Strings.lines "one\ntwo\nthree")
      ([
          "one",
          "two",
          "three"])
    H.it "empty string" $ H.shouldBe
      (Strings.lines "")
      ([])
    H.it "just newline" $ H.shouldBe
      (Strings.lines "\n")
      ([
          ""])
    H.it "trailing newline" $ H.shouldBe
      (Strings.lines "hello\n")
      ([
          "hello"])
    H.it "leading newline" $ H.shouldBe
      (Strings.lines "\nhello")
      ([
          "",
          "hello"])
    H.it "multiple consecutive newlines" $ H.shouldBe
      (Strings.lines "a\n\nb")
      ([
          "a",
          "",
          "b"])
    H.it "unicode content" $ H.shouldBe
      (Strings.lines "\241\n\19990")
      ([
          "\241",
          "\19990"])
    H.it "tabs not split" $ H.shouldBe
      (Strings.lines "a\tb\nc")
      ([
          "a\tb",
          "c"])
  H.describe "null" $ do
    H.it "empty string" $ H.shouldBe
      (Strings.null "")
      (True)
    H.it "single character" $ H.shouldBe
      (Strings.null "a")
      (False)
    H.it "space" $ H.shouldBe
      (Strings.null " ")
      (False)
    H.it "unicode space" $ H.shouldBe
      (Strings.null "\160")
      (False)
    H.it "newline" $ H.shouldBe
      (Strings.null "\n")
      (False)
    H.it "null character" $ H.shouldBe
      (Strings.null "\NUL")
      (False)
    H.it "multi-character" $ H.shouldBe
      (Strings.null "hello")
      (False)
  H.describe "splitOn" $ do
    H.it "basic separator" $ H.shouldBe
      (Strings.splitOn "ss" "Mississippi")
      ([
          "Mi",
          "i",
          "ippi"])
    H.it "single char separator" $ H.shouldBe
      (Strings.splitOn " " "one two three")
      ([
          "one",
          "two",
          "three"])
    H.it "multi-char separator" $ H.shouldBe
      (Strings.splitOn "  " "a  b  c")
      ([
          "a",
          "b",
          "c"])
    H.it "separator not found" $ H.shouldBe
      (Strings.splitOn "x" "hello")
      ([
          "hello"])
    H.it "separator at start" $ H.shouldBe
      (Strings.splitOn "h" "hello")
      ([
          "",
          "ello"])
    H.it "separator at end" $ H.shouldBe
      (Strings.splitOn "o" "hello")
      ([
          "hell",
          ""])
    H.it "leading and trailing separator" $ H.shouldBe
      (Strings.splitOn " " " one two ")
      ([
          "",
          "one",
          "two",
          ""])
    H.it "whole string as separator" $ H.shouldBe
      (Strings.splitOn "Mississippi" "Mississippi")
      ([
          "",
          ""])
    H.it "consecutive separators" $ H.shouldBe
      (Strings.splitOn " " "a  b")
      ([
          "a",
          "",
          "b"])
    H.it "multiple occurrences" $ H.shouldBe
      (Strings.splitOn "l" "hello")
      ([
          "he",
          "",
          "o"])
    H.it "overlapping pattern" $ H.shouldBe
      (Strings.splitOn "aa" "aaa")
      ([
          "",
          "a"])
    H.it "empty separator" $ H.shouldBe
      (Strings.splitOn "" "abc")
      ([
          "",
          "a",
          "b",
          "c"])
    H.it "separator on empty string" $ H.shouldBe
      (Strings.splitOn "x" "")
      ([
          ""])
    H.it "both empty" $ H.shouldBe
      (Strings.splitOn "" "")
      ([
          ""])
    H.it "single char both" $ H.shouldBe
      (Strings.splitOn "a" "a")
      ([
          "",
          ""])
    H.it "unicode separator" $ H.shouldBe
      (Strings.splitOn "\19990" "hello\19990world")
      ([
          "hello",
          "world"])
    H.it "unicode content" $ H.shouldBe
      (Strings.splitOn "," "\241,\19990,\127757")
      ([
          "\241",
          "\19990",
          "\127757"])
    H.it "newline separator" $ H.shouldBe
      (Strings.splitOn "\n" "line1\nline2\nline3")
      ([
          "line1",
          "line2",
          "line3"])
  H.describe "toList" $ do
    H.it "empty string" $ H.shouldBe
      (Strings.toList "")
      ([])
    H.it "single character" $ H.shouldBe
      (Strings.toList "a")
      ([
          97])
    H.it "basic word" $ H.shouldBe
      (Strings.toList "hello")
      ([
          104,
          101,
          108,
          108,
          111])
    H.it "unicode characters" $ H.shouldBe
      (Strings.toList "\241\19990\127757")
      ([
          241,
          19990,
          127757])
    H.it "combining character sequence" $ H.shouldBe
      (Strings.toList "e\769")
      ([
          101,
          769])
    H.it "control characters" $ H.shouldBe
      (Strings.toList "\n\t\r")
      ([
          10,
          9,
          13])
    H.it "null character" $ H.shouldBe
      (Strings.toList "h\NULi")
      ([
          104,
          0,
          105])
  H.describe "toLower" $ do
    H.it "mixed case" $ H.shouldBe
      (Strings.toLower "Hello World")
      ("hello world")
    H.it "all uppercase" $ H.shouldBe
      (Strings.toLower "HELLO")
      ("hello")
    H.it "all lowercase" $ H.shouldBe
      (Strings.toLower "hello")
      ("hello")
    H.it "empty string" $ H.shouldBe
      (Strings.toLower "")
      ("")
    H.it "with numbers and punctuation" $ H.shouldBe
      (Strings.toLower "Abc123, XYZ!")
      ("abc123, xyz!")
    H.it "control characters" $ H.shouldBe
      (Strings.toLower "\n\t\r")
      ("\n\t\r")
    H.it "unicode accented chars" $ H.shouldBe
      (Strings.toLower "\209\193\201\205\211\218")
      ("\241\225\233\237\243\250")
  H.describe "toUpper" $ do
    H.it "mixed case" $ H.shouldBe
      (Strings.toUpper "hello World")
      ("HELLO WORLD")
    H.it "all lowercase" $ H.shouldBe
      (Strings.toUpper "hello")
      ("HELLO")
    H.it "all uppercase" $ H.shouldBe
      (Strings.toUpper "HELLO")
      ("HELLO")
    H.it "empty string" $ H.shouldBe
      (Strings.toUpper "")
      ("")
    H.it "with numbers and punctuation" $ H.shouldBe
      (Strings.toUpper "abc123, xyz!")
      ("ABC123, XYZ!")
    H.it "control characters" $ H.shouldBe
      (Strings.toUpper "\n\t\r")
      ("\n\t\r")
    H.it "unicode accented chars" $ H.shouldBe
      (Strings.toUpper "\241\225\233\237\243\250")
      ("\209\193\201\205\211\218")
  H.describe "unlines" $ do
    H.it "multiple lines" $ H.shouldBe
      (Strings.unlines [
          "one",
          "two",
          "three"])
      ("one\ntwo\nthree\n")
    H.it "single line" $ H.shouldBe
      (Strings.unlines [
          "hello"])
      ("hello\n")
    H.it "empty list" $ H.shouldBe
      (Strings.unlines [])
      ("")
    H.it "with empty lines" $ H.shouldBe
      (Strings.unlines [
          "hello",
          "",
          "world"])
      ("hello\n\nworld\n")
    H.it "all empty lines" $ H.shouldBe
      (Strings.unlines [
          "",
          "",
          ""])
      ("\n\n\n")
    H.it "unicode content" $ H.shouldBe
      (Strings.unlines [
          "\241o\241o",
          "\19990\30028"])
      ("\241o\241o\n\19990\30028\n")
