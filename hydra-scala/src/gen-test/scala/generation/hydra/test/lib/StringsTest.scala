// Note: this is an automatically generated file. Do not edit.
// hydra.lib.strings primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class StringsTest extends AnyFunSuite {

  // cat

  test("cat - basic concatenation") {

    assert((

      hydra.lib.strings.cat(Seq("one", "two", "three"))) == (

      "onetwothree"))

  }

  test("cat - single string") {

    assert((

      hydra.lib.strings.cat(Seq("hello"))) == (

      "hello"))

  }

  test("cat - empty list") {

    assert((

      hydra.lib.strings.cat(Seq())) == (

      ""))

  }

  test("cat - with empty strings") {

    assert((

      hydra.lib.strings.cat(Seq("", "one", "", ""))) == (

      "one"))

  }

  test("cat - all empty strings") {

    assert((

      hydra.lib.strings.cat(Seq("", "", "", ""))) == (

      ""))

  }

  test("cat - unicode strings") {

    assert((

      hydra.lib.strings.cat(Seq("\u00F1", "\u4E16", "\uD83C\uDF0D"))) == (

      "\u00F1\u4E16\uD83C\uDF0D"))

  }

  test("cat - combining characters") {

    assert((

      hydra.lib.strings.cat(Seq("e", "\u0301"))) == (

      "e\u0301"))

  }

  test("cat - control characters") {

    assert((

      hydra.lib.strings.cat(Seq("\n", "\t", "\r"))) == (

      "\n\t\r"))

  }

  test("cat - null character") {

    assert((

      hydra.lib.strings.cat(Seq("hello", "\u0000", "world"))) == (

      "hello\u0000world"))

  }

  // cat2

  test("cat2 - basic concatenation") {

    assert((

      hydra.lib.strings.cat2("hello")("world")) == (

      "helloworld"))

  }

  test("cat2 - empty first string") {

    assert((

      hydra.lib.strings.cat2("")("world")) == (

      "world"))

  }

  test("cat2 - empty second string") {

    assert((

      hydra.lib.strings.cat2("hello")("")) == (

      "hello"))

  }

  test("cat2 - both empty strings") {

    assert((

      hydra.lib.strings.cat2("")("")) == (

      ""))

  }

  test("cat2 - unicode characters") {

    assert((

      hydra.lib.strings.cat2("\u00F1")("\u4E16")) == (

      "\u00F1\u4E16"))

  }

  test("cat2 - special characters") {

    assert((

      hydra.lib.strings.cat2("\n")("\t")) == (

      "\n\t"))

  }

  test("cat2 - null characters") {

    assert((

      hydra.lib.strings.cat2("hello\u0000")("world")) == (

      "hello\u0000world"))

  }

  // charAt

  test("charAt - first character") {

    assert((

      hydra.lib.strings.charAt(0)("hello")) == (

      104))

  }

  test("charAt - middle character") {

    assert((

      hydra.lib.strings.charAt(2)("hello")) == (

      108))

  }

  test("charAt - last character") {

    assert((

      hydra.lib.strings.charAt(4)("hello")) == (

      111))

  }

  test("charAt - single character string") {

    assert((

      hydra.lib.strings.charAt(0)("a")) == (

      97))

  }

  test("charAt - unicode character") {

    assert((

      hydra.lib.strings.charAt(0)("\u00F1")) == (

      241))

  }

  test("charAt - multi-byte unicode") {

    assert((

      hydra.lib.strings.charAt(0)("\u4E16")) == (

      19990))

  }

  test("charAt - second of combining pair") {

    assert((

      hydra.lib.strings.charAt(1)("e\u0301")) == (

      769))

  }

  // fromList

  test("fromList - basic ascii string") {

    assert((

      hydra.lib.strings.fromList(Seq(104, 101, 108, 108, 111))) == (

      "hello"))

  }

  test("fromList - empty code point list") {

    assert((

      hydra.lib.strings.fromList(Seq())) == (

      ""))

  }

  test("fromList - single character") {

    assert((

      hydra.lib.strings.fromList(Seq(97))) == (

      "a"))

  }

  test("fromList - unicode characters") {

    assert((

      hydra.lib.strings.fromList(Seq(241, 19990, 127757))) == (

      "\u00F1\u4E16\uD83C\uDF0D"))

  }

  test("fromList - combining character sequence") {

    assert((

      hydra.lib.strings.fromList(Seq(101, 769))) == (

      "e\u0301"))

  }

  test("fromList - special characters") {

    assert((

      hydra.lib.strings.fromList(Seq(10, 9, 13))) == (

      "\n\t\r"))

  }

  test("fromList - null character") {

    assert((

      hydra.lib.strings.fromList(Seq(104, 0, 105))) == (

      "h\u0000i"))

  }

  // intercalate

  test("intercalate - comma separator") {

    assert((

      hydra.lib.strings.intercalate(",")(Seq("one", "two", "three"))) == (

      "one,two,three"))

  }

  test("intercalate - empty separator") {

    assert((

      hydra.lib.strings.intercalate("")(Seq("a", "b", "c"))) == (

      "abc"))

  }

  test("intercalate - multi-character separator") {

    assert((

      hydra.lib.strings.intercalate(" | ")(Seq("A", "B", "C"))) == (

      "A | B | C"))

  }

  test("intercalate - empty string list") {

    assert((

      hydra.lib.strings.intercalate(",")(Seq())) == (

      ""))

  }

  test("intercalate - single item list") {

    assert((

      hydra.lib.strings.intercalate(",")(Seq("only"))) == (

      "only"))

  }

  test("intercalate - empty strings in list") {

    assert((

      hydra.lib.strings.intercalate(",")(Seq("", "a", ""))) == (

      ",a,"))

  }

  test("intercalate - unicode separator") {

    assert((

      hydra.lib.strings.intercalate("\uD83C\uDF0D")(Seq("link1", "link2"))) == (

      "link1\uD83C\uDF0Dlink2"))

  }

  test("intercalate - newline separator") {

    assert((

      hydra.lib.strings.intercalate("\n")(Seq("line1", "line2"))) == (

      "line1\nline2"))

  }

  // length

  test("length - empty string") {

    assert((

      hydra.lib.strings.length("")) == (

      0))

  }

  test("length - single character") {

    assert((

      hydra.lib.strings.length("a")) == (

      1))

  }

  test("length - basic word") {

    assert((

      hydra.lib.strings.length("hello")) == (

      5))

  }

  test("length - unicode characters") {

    assert((

      hydra.lib.strings.length("\u00F1\u4E16\uD83C\uDF0D")) == (

      3))

  }

  test("length - combining character sequence") {

    assert((

      hydra.lib.strings.length("e\u0301")) == (

      2))

  }

  test("length - special characters") {

    assert((

      hydra.lib.strings.length("\n\t\r")) == (

      3))

  }

  // lines

  test("lines - single line") {

    assert((

      hydra.lib.strings.lines("hello world")) == (

      Seq("hello world")))

  }

  test("lines - two lines") {

    assert((

      hydra.lib.strings.lines("hello\nworld")) == (

      Seq("hello", "world")))

  }

  test("lines - three lines") {

    assert((

      hydra.lib.strings.lines("one\ntwo\nthree")) == (

      Seq("one", "two", "three")))

  }

  test("lines - empty string") {

    assert((

      hydra.lib.strings.lines("")) == (

      Seq()))

  }

  test("lines - just newline") {

    assert((

      hydra.lib.strings.lines("\n")) == (

      Seq("")))

  }

  test("lines - trailing newline") {

    assert((

      hydra.lib.strings.lines("hello\n")) == (

      Seq("hello")))

  }

  test("lines - leading newline") {

    assert((

      hydra.lib.strings.lines("\nhello")) == (

      Seq("", "hello")))

  }

  test("lines - multiple consecutive newlines") {

    assert((

      hydra.lib.strings.lines("a\n\nb")) == (

      Seq("a", "", "b")))

  }

  test("lines - unicode content") {

    assert((

      hydra.lib.strings.lines("\u00F1\n\u4E16")) == (

      Seq("\u00F1", "\u4E16")))

  }

  test("lines - tabs not split") {

    assert((

      hydra.lib.strings.lines("a\tb\nc")) == (

      Seq("a\tb", "c")))

  }

  // null

  test("null - empty string") {

    assert((

      hydra.lib.strings.`null`("")) == (

      true))

  }

  test("null - single character") {

    assert((

      hydra.lib.strings.`null`("a")) == (

      false))

  }

  test("null - space") {

    assert((

      hydra.lib.strings.`null`(" ")) == (

      false))

  }

  test("null - unicode space") {

    assert((

      hydra.lib.strings.`null`("\u00A0")) == (

      false))

  }

  test("null - newline") {

    assert((

      hydra.lib.strings.`null`("\n")) == (

      false))

  }

  test("null - null character") {

    assert((

      hydra.lib.strings.`null`("\u0000")) == (

      false))

  }

  test("null - multi-character") {

    assert((

      hydra.lib.strings.`null`("hello")) == (

      false))

  }

  // splitOn

  test("splitOn - basic separator") {

    assert((

      hydra.lib.strings.splitOn("ss")("Mississippi")) == (

      Seq("Mi", "i", "ippi")))

  }

  test("splitOn - single char separator") {

    assert((

      hydra.lib.strings.splitOn(" ")("one two three")) == (

      Seq("one", "two", "three")))

  }

  test("splitOn - multi-char separator") {

    assert((

      hydra.lib.strings.splitOn("  ")("a  b  c")) == (

      Seq("a", "b", "c")))

  }

  test("splitOn - separator not found") {

    assert((

      hydra.lib.strings.splitOn("x")("hello")) == (

      Seq("hello")))

  }

  test("splitOn - separator at start") {

    assert((

      hydra.lib.strings.splitOn("h")("hello")) == (

      Seq("", "ello")))

  }

  test("splitOn - separator at end") {

    assert((

      hydra.lib.strings.splitOn("o")("hello")) == (

      Seq("hell", "")))

  }

  test("splitOn - leading and trailing separator") {

    assert((

      hydra.lib.strings.splitOn(" ")(" one two ")) == (

      Seq("", "one", "two", "")))

  }

  test("splitOn - whole string as separator") {

    assert((

      hydra.lib.strings.splitOn("Mississippi")("Mississippi")) == (

      Seq("", "")))

  }

  test("splitOn - consecutive separators") {

    assert((

      hydra.lib.strings.splitOn(" ")("a  b")) == (

      Seq("a", "", "b")))

  }

  test("splitOn - multiple occurrences") {

    assert((

      hydra.lib.strings.splitOn("l")("hello")) == (

      Seq("he", "", "o")))

  }

  test("splitOn - overlapping pattern") {

    assert((

      hydra.lib.strings.splitOn("aa")("aaa")) == (

      Seq("", "a")))

  }

  test("splitOn - empty separator") {

    assert((

      hydra.lib.strings.splitOn("")("abc")) == (

      Seq("", "a", "b", "c")))

  }

  test("splitOn - separator on empty string") {

    assert((

      hydra.lib.strings.splitOn("x")("")) == (

      Seq("")))

  }

  test("splitOn - both empty") {

    assert((

      hydra.lib.strings.splitOn("")("")) == (

      Seq("")))

  }

  test("splitOn - single char both") {

    assert((

      hydra.lib.strings.splitOn("a")("a")) == (

      Seq("", "")))

  }

  test("splitOn - unicode separator") {

    assert((

      hydra.lib.strings.splitOn("\u4E16")("hello\u4E16world")) == (

      Seq("hello", "world")))

  }

  test("splitOn - unicode content") {

    assert((

      hydra.lib.strings.splitOn(",")("\u00F1,\u4E16,\uD83C\uDF0D")) == (

      Seq("\u00F1", "\u4E16", "\uD83C\uDF0D")))

  }

  test("splitOn - newline separator") {

    assert((

      hydra.lib.strings.splitOn("\n")("line1\nline2\nline3")) == (

      Seq("line1", "line2", "line3")))

  }

  // toList

  test("toList - empty string") {

    assert((

      hydra.lib.strings.toList("")) == (

      Seq()))

  }

  test("toList - single character") {

    assert((

      hydra.lib.strings.toList("a")) == (

      Seq(97)))

  }

  test("toList - basic word") {

    assert((

      hydra.lib.strings.toList("hello")) == (

      Seq(104, 101, 108, 108, 111)))

  }

  test("toList - unicode characters") {

    assert((

      hydra.lib.strings.toList("\u00F1\u4E16\uD83C\uDF0D")) == (

      Seq(241, 19990, 127757)))

  }

  test("toList - combining character sequence") {

    assert((

      hydra.lib.strings.toList("e\u0301")) == (

      Seq(101, 769)))

  }

  test("toList - control characters") {

    assert((

      hydra.lib.strings.toList("\n\t\r")) == (

      Seq(10, 9, 13)))

  }

  test("toList - null character") {

    assert((

      hydra.lib.strings.toList("h\u0000i")) == (

      Seq(104, 0, 105)))

  }

  // toLower

  test("toLower - mixed case") {

    assert((

      hydra.lib.strings.toLower("Hello World")) == (

      "hello world"))

  }

  test("toLower - all uppercase") {

    assert((

      hydra.lib.strings.toLower("HELLO")) == (

      "hello"))

  }

  test("toLower - all lowercase") {

    assert((

      hydra.lib.strings.toLower("hello")) == (

      "hello"))

  }

  test("toLower - empty string") {

    assert((

      hydra.lib.strings.toLower("")) == (

      ""))

  }

  test("toLower - with numbers and punctuation") {

    assert((

      hydra.lib.strings.toLower("Abc123, XYZ!")) == (

      "abc123, xyz!"))

  }

  test("toLower - control characters") {

    assert((

      hydra.lib.strings.toLower("\n\t\r")) == (

      "\n\t\r"))

  }

  test("toLower - unicode accented chars") {

    assert((

      hydra.lib.strings.toLower("\u00D1\u00C1\u00C9\u00CD\u00D3\u00DA")) == (

      "\u00F1\u00E1\u00E9\u00ED\u00F3\u00FA"))

  }

  // toUpper

  test("toUpper - mixed case") {

    assert((

      hydra.lib.strings.toUpper("hello World")) == (

      "HELLO WORLD"))

  }

  test("toUpper - all lowercase") {

    assert((

      hydra.lib.strings.toUpper("hello")) == (

      "HELLO"))

  }

  test("toUpper - all uppercase") {

    assert((

      hydra.lib.strings.toUpper("HELLO")) == (

      "HELLO"))

  }

  test("toUpper - empty string") {

    assert((

      hydra.lib.strings.toUpper("")) == (

      ""))

  }

  test("toUpper - with numbers and punctuation") {

    assert((

      hydra.lib.strings.toUpper("abc123, xyz!")) == (

      "ABC123, XYZ!"))

  }

  test("toUpper - control characters") {

    assert((

      hydra.lib.strings.toUpper("\n\t\r")) == (

      "\n\t\r"))

  }

  test("toUpper - unicode accented chars") {

    assert((

      hydra.lib.strings.toUpper("\u00F1\u00E1\u00E9\u00ED\u00F3\u00FA")) == (

      "\u00D1\u00C1\u00C9\u00CD\u00D3\u00DA"))

  }

  // unlines

  test("unlines - multiple lines") {

    assert((

      hydra.lib.strings.unlines(Seq("one", "two", "three"))) == (

      "one\ntwo\nthree\n"))

  }

  test("unlines - single line") {

    assert((

      hydra.lib.strings.unlines(Seq("hello"))) == (

      "hello\n"))

  }

  test("unlines - empty list") {

    assert((

      hydra.lib.strings.unlines(Seq())) == (

      ""))

  }

  test("unlines - with empty lines") {

    assert((

      hydra.lib.strings.unlines(Seq("hello", "", "world"))) == (

      "hello\n\nworld\n"))

  }

  test("unlines - all empty lines") {

    assert((

      hydra.lib.strings.unlines(Seq("", "", ""))) == (

      "\n\n\n"))

  }

  test("unlines - unicode content") {

    assert((

      hydra.lib.strings.unlines(Seq("\u00F1o\u00F1o", "\u4E16\u754C"))) == (

      "\u00F1o\u00F1o\n\u4E16\u754C\n"))

  }
}
