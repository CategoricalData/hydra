// Note: this is an automatically generated file. Do not edit.
// hydra.lib.strings primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class StringsTest {

    // cat

    @Test

    public void testCatBasicConcatenation() {

        assertEquals(

            "onetwothree",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "one",
  "two",
  "three")));

    }

    @Test

    public void testCatSingleString() {

        assertEquals(

            "hello",

            hydra.lib.strings.Cat.apply(java.util.List.of("hello")));

    }

    @Test

    public void testCatEmptyList() {

        assertEquals(

            "",

            hydra.lib.strings.Cat.apply((java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testCatWithEmptyStrings() {

        assertEquals(

            "one",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "",
  "one",
  "",
  "")));

    }

    @Test

    public void testCatAllEmptyStrings() {

        assertEquals(

            "",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "",
  "",
  "",
  "")));

    }

    @Test

    public void testCatUnicodeStrings() {

        assertEquals(

            "\u00F1\u4E16\uD83C\uDF0D",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "\u00F1",
  "\u4E16",
  "\uD83C\uDF0D")));

    }

    @Test

    public void testCatCombiningCharacters() {

        assertEquals(

            "e\u0301",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "e",
  "\u0301")));

    }

    @Test

    public void testCatControlCharacters() {

        assertEquals(

            "\n\t\r",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "\n",
  "\t",
  "\r")));

    }

    @Test

    public void testCatNullCharacter() {

        assertEquals(

            "hello\u0000world",

            hydra.lib.strings.Cat.apply(java.util.List.of(
  "hello",
  "\u0000",
  "world")));

    }

    // cat2

    @Test

    public void testCat2BasicConcatenation() {

        assertEquals(

            "helloworld",

            hydra.lib.strings.Cat2.apply(
  "hello",
  "world"));

    }

    @Test

    public void testCat2EmptyFirstString() {

        assertEquals(

            "world",

            hydra.lib.strings.Cat2.apply(
  "",
  "world"));

    }

    @Test

    public void testCat2EmptySecondString() {

        assertEquals(

            "hello",

            hydra.lib.strings.Cat2.apply(
  "hello",
  ""));

    }

    @Test

    public void testCat2BothEmptyStrings() {

        assertEquals(

            "",

            hydra.lib.strings.Cat2.apply(
  "",
  ""));

    }

    @Test

    public void testCat2UnicodeCharacters() {

        assertEquals(

            "\u00F1\u4E16",

            hydra.lib.strings.Cat2.apply(
  "\u00F1",
  "\u4E16"));

    }

    @Test

    public void testCat2SpecialCharacters() {

        assertEquals(

            "\n\t",

            hydra.lib.strings.Cat2.apply(
  "\n",
  "\t"));

    }

    @Test

    public void testCat2NullCharacters() {

        assertEquals(

            "hello\u0000world",

            hydra.lib.strings.Cat2.apply(
  "hello\u0000",
  "world"));

    }

    // charAt

    @Test

    public void testCharatFirstCharacter() {

        assertEquals(

            104,

            hydra.lib.strings.CharAt.apply(
  0,
  "hello"));

    }

    @Test

    public void testCharatMiddleCharacter() {

        assertEquals(

            108,

            hydra.lib.strings.CharAt.apply(
  2,
  "hello"));

    }

    @Test

    public void testCharatLastCharacter() {

        assertEquals(

            111,

            hydra.lib.strings.CharAt.apply(
  4,
  "hello"));

    }

    @Test

    public void testCharatSingleCharacterString() {

        assertEquals(

            97,

            hydra.lib.strings.CharAt.apply(
  0,
  "a"));

    }

    @Test

    public void testCharatUnicodeCharacter() {

        assertEquals(

            241,

            hydra.lib.strings.CharAt.apply(
  0,
  "\u00F1"));

    }

    @Test

    public void testCharatMultiNegbyteUnicode() {

        assertEquals(

            19990,

            hydra.lib.strings.CharAt.apply(
  0,
  "\u4E16"));

    }

    @Test

    public void testCharatSecondOfCombiningPair() {

        assertEquals(

            769,

            hydra.lib.strings.CharAt.apply(
  1,
  "e\u0301"));

    }

    // fromList

    @Test

    public void testFromlistBasicAsciiString() {

        assertEquals(

            "hello",

            hydra.lib.strings.FromList.apply(java.util.List.of(
  104,
  101,
  108,
  108,
  111)));

    }

    @Test

    public void testFromlistEmptyCodePointList() {

        assertEquals(

            "",

            hydra.lib.strings.FromList.apply((java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    @Test

    public void testFromlistSingleCharacter() {

        assertEquals(

            "a",

            hydra.lib.strings.FromList.apply(java.util.List.of(97)));

    }

    @Test

    public void testFromlistUnicodeCharacters() {

        assertEquals(

            "\u00F1\u4E16\uD83C\uDF0D",

            hydra.lib.strings.FromList.apply(java.util.List.of(
  241,
  19990,
  127757)));

    }

    @Test

    public void testFromlistCombiningCharacterSequence() {

        assertEquals(

            "e\u0301",

            hydra.lib.strings.FromList.apply(java.util.List.of(
  101,
  769)));

    }

    @Test

    public void testFromlistSpecialCharacters() {

        assertEquals(

            "\n\t\r",

            hydra.lib.strings.FromList.apply(java.util.List.of(
  10,
  9,
  13)));

    }

    @Test

    public void testFromlistNullCharacter() {

        assertEquals(

            "h\u0000i",

            hydra.lib.strings.FromList.apply(java.util.List.of(
  104,
  0,
  105)));

    }

    // intercalate

    @Test

    public void testIntercalateCommaSeparator() {

        assertEquals(

            "one,two,three",

            hydra.lib.strings.Intercalate.apply(
  ",",
  java.util.List.of(
    "one",
    "two",
    "three")));

    }

    @Test

    public void testIntercalateEmptySeparator() {

        assertEquals(

            "abc",

            hydra.lib.strings.Intercalate.apply(
  "",
  java.util.List.of(
    "a",
    "b",
    "c")));

    }

    @Test

    public void testIntercalateMultiNegcharacterSeparator() {

        assertEquals(

            "A | B | C",

            hydra.lib.strings.Intercalate.apply(
  " | ",
  java.util.List.of(
    "A",
    "B",
    "C")));

    }

    @Test

    public void testIntercalateEmptyStringList() {

        assertEquals(

            "",

            hydra.lib.strings.Intercalate.apply(
  ",",
  (java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testIntercalateSingleItemList() {

        assertEquals(

            "only",

            hydra.lib.strings.Intercalate.apply(
  ",",
  java.util.List.of("only")));

    }

    @Test

    public void testIntercalateEmptyStringsInList() {

        assertEquals(

            ",a,",

            hydra.lib.strings.Intercalate.apply(
  ",",
  java.util.List.of(
    "",
    "a",
    "")));

    }

    @Test

    public void testIntercalateUnicodeSeparator() {

        assertEquals(

            "link1\uD83C\uDF0Dlink2",

            hydra.lib.strings.Intercalate.apply(
  "\uD83C\uDF0D",
  java.util.List.of(
    "link1",
    "link2")));

    }

    @Test

    public void testIntercalateNewlineSeparator() {

        assertEquals(

            "line1\nline2",

            hydra.lib.strings.Intercalate.apply(
  "\n",
  java.util.List.of(
    "line1",
    "line2")));

    }

    // length

    @Test

    public void testLengthEmptyString() {

        assertEquals(

            0,

            hydra.lib.strings.Length.apply(""));

    }

    @Test

    public void testLengthSingleCharacter() {

        assertEquals(

            1,

            hydra.lib.strings.Length.apply("a"));

    }

    @Test

    public void testLengthBasicWord() {

        assertEquals(

            5,

            hydra.lib.strings.Length.apply("hello"));

    }

    @Test

    public void testLengthUnicodeCharacters() {

        assertEquals(

            3,

            hydra.lib.strings.Length.apply("\u00F1\u4E16\uD83C\uDF0D"));

    }

    @Test

    public void testLengthCombiningCharacterSequence() {

        assertEquals(

            2,

            hydra.lib.strings.Length.apply("e\u0301"));

    }

    @Test

    public void testLengthSpecialCharacters() {

        assertEquals(

            3,

            hydra.lib.strings.Length.apply("\n\t\r"));

    }

    // lines

    @Test

    public void testLinesSingleLine() {

        assertEquals(

            java.util.List.of("hello world"),

            hydra.lib.strings.Lines.apply("hello world"));

    }

    @Test

    public void testLinesTwoLines() {

        assertEquals(

            java.util.List.of(
  "hello",
  "world"),

            hydra.lib.strings.Lines.apply("hello\nworld"));

    }

    @Test

    public void testLinesThreeLines() {

        assertEquals(

            java.util.List.of(
  "one",
  "two",
  "three"),

            hydra.lib.strings.Lines.apply("one\ntwo\nthree"));

    }

    @Test

    public void testLinesEmptyString() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.strings.Lines.apply(""));

    }

    @Test

    public void testLinesJustNewline() {

        assertEquals(

            java.util.List.of(""),

            hydra.lib.strings.Lines.apply("\n"));

    }

    @Test

    public void testLinesTrailingNewline() {

        assertEquals(

            java.util.List.of("hello"),

            hydra.lib.strings.Lines.apply("hello\n"));

    }

    @Test

    public void testLinesLeadingNewline() {

        assertEquals(

            java.util.List.of(
  "",
  "hello"),

            hydra.lib.strings.Lines.apply("\nhello"));

    }

    @Test

    public void testLinesMultipleConsecutiveNewlines() {

        assertEquals(

            java.util.List.of(
  "a",
  "",
  "b"),

            hydra.lib.strings.Lines.apply("a\n\nb"));

    }

    @Test

    public void testLinesUnicodeContent() {

        assertEquals(

            java.util.List.of(
  "\u00F1",
  "\u4E16"),

            hydra.lib.strings.Lines.apply("\u00F1\n\u4E16"));

    }

    @Test

    public void testLinesTabsNotSplit() {

        assertEquals(

            java.util.List.of(
  "a\tb",
  "c"),

            hydra.lib.strings.Lines.apply("a\tb\nc"));

    }

    // null

    @Test

    public void testNullEmptyString() {

        assertEquals(

            true,

            hydra.lib.strings.Null.apply(""));

    }

    @Test

    public void testNullSingleCharacter() {

        assertEquals(

            false,

            hydra.lib.strings.Null.apply("a"));

    }

    @Test

    public void testNullSpace() {

        assertEquals(

            false,

            hydra.lib.strings.Null.apply(" "));

    }

    @Test

    public void testNullUnicodeSpace() {

        assertEquals(

            false,

            hydra.lib.strings.Null.apply("\u00A0"));

    }

    @Test

    public void testNullNewline() {

        assertEquals(

            false,

            hydra.lib.strings.Null.apply("\n"));

    }

    @Test

    public void testNullNullCharacter() {

        assertEquals(

            false,

            hydra.lib.strings.Null.apply("\u0000"));

    }

    @Test

    public void testNullMultiNegcharacter() {

        assertEquals(

            false,

            hydra.lib.strings.Null.apply("hello"));

    }

    // splitOn

    @Test

    public void testSplitonBasicSeparator() {

        assertEquals(

            java.util.List.of(
  "Mi",
  "i",
  "ippi"),

            hydra.lib.strings.SplitOn.apply(
  "ss",
  "Mississippi"));

    }

    @Test

    public void testSplitonSingleCharSeparator() {

        assertEquals(

            java.util.List.of(
  "one",
  "two",
  "three"),

            hydra.lib.strings.SplitOn.apply(
  " ",
  "one two three"));

    }

    @Test

    public void testSplitonMultiNegcharSeparator() {

        assertEquals(

            java.util.List.of(
  "a",
  "b",
  "c"),

            hydra.lib.strings.SplitOn.apply(
  "  ",
  "a  b  c"));

    }

    @Test

    public void testSplitonSeparatorNotFound() {

        assertEquals(

            java.util.List.of("hello"),

            hydra.lib.strings.SplitOn.apply(
  "x",
  "hello"));

    }

    @Test

    public void testSplitonSeparatorAtStart() {

        assertEquals(

            java.util.List.of(
  "",
  "ello"),

            hydra.lib.strings.SplitOn.apply(
  "h",
  "hello"));

    }

    @Test

    public void testSplitonSeparatorAtEnd() {

        assertEquals(

            java.util.List.of(
  "hell",
  ""),

            hydra.lib.strings.SplitOn.apply(
  "o",
  "hello"));

    }

    @Test

    public void testSplitonLeadingAndTrailingSeparator() {

        assertEquals(

            java.util.List.of(
  "",
  "one",
  "two",
  ""),

            hydra.lib.strings.SplitOn.apply(
  " ",
  " one two "));

    }

    @Test

    public void testSplitonWholeStringAsSeparator() {

        assertEquals(

            java.util.List.of(
  "",
  ""),

            hydra.lib.strings.SplitOn.apply(
  "Mississippi",
  "Mississippi"));

    }

    @Test

    public void testSplitonConsecutiveSeparators() {

        assertEquals(

            java.util.List.of(
  "a",
  "",
  "b"),

            hydra.lib.strings.SplitOn.apply(
  " ",
  "a  b"));

    }

    @Test

    public void testSplitonMultipleOccurrences() {

        assertEquals(

            java.util.List.of(
  "he",
  "",
  "o"),

            hydra.lib.strings.SplitOn.apply(
  "l",
  "hello"));

    }

    @Test

    public void testSplitonOverlappingPattern() {

        assertEquals(

            java.util.List.of(
  "",
  "a"),

            hydra.lib.strings.SplitOn.apply(
  "aa",
  "aaa"));

    }

    @Test

    public void testSplitonEmptySeparator() {

        assertEquals(

            java.util.List.of(
  "",
  "a",
  "b",
  "c"),

            hydra.lib.strings.SplitOn.apply(
  "",
  "abc"));

    }

    @Test

    public void testSplitonSeparatorOnEmptyString() {

        assertEquals(

            java.util.List.of(""),

            hydra.lib.strings.SplitOn.apply(
  "x",
  ""));

    }

    @Test

    public void testSplitonBothEmpty() {

        assertEquals(

            java.util.List.of(""),

            hydra.lib.strings.SplitOn.apply(
  "",
  ""));

    }

    @Test

    public void testSplitonSingleCharBoth() {

        assertEquals(

            java.util.List.of(
  "",
  ""),

            hydra.lib.strings.SplitOn.apply(
  "a",
  "a"));

    }

    @Test

    public void testSplitonUnicodeSeparator() {

        assertEquals(

            java.util.List.of(
  "hello",
  "world"),

            hydra.lib.strings.SplitOn.apply(
  "\u4E16",
  "hello\u4E16world"));

    }

    @Test

    public void testSplitonUnicodeContent() {

        assertEquals(

            java.util.List.of(
  "\u00F1",
  "\u4E16",
  "\uD83C\uDF0D"),

            hydra.lib.strings.SplitOn.apply(
  ",",
  "\u00F1,\u4E16,\uD83C\uDF0D"));

    }

    @Test

    public void testSplitonNewlineSeparator() {

        assertEquals(

            java.util.List.of(
  "line1",
  "line2",
  "line3"),

            hydra.lib.strings.SplitOn.apply(
  "\n",
  "line1\nline2\nline3"));

    }

    // toList

    @Test

    public void testTolistEmptyString() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.strings.ToList.apply(""));

    }

    @Test

    public void testTolistSingleCharacter() {

        assertEquals(

            java.util.List.of(97),

            hydra.lib.strings.ToList.apply("a"));

    }

    @Test

    public void testTolistBasicWord() {

        assertEquals(

            java.util.List.of(
  104,
  101,
  108,
  108,
  111),

            hydra.lib.strings.ToList.apply("hello"));

    }

    @Test

    public void testTolistUnicodeCharacters() {

        assertEquals(

            java.util.List.of(
  241,
  19990,
  127757),

            hydra.lib.strings.ToList.apply("\u00F1\u4E16\uD83C\uDF0D"));

    }

    @Test

    public void testTolistCombiningCharacterSequence() {

        assertEquals(

            java.util.List.of(
  101,
  769),

            hydra.lib.strings.ToList.apply("e\u0301"));

    }

    @Test

    public void testTolistControlCharacters() {

        assertEquals(

            java.util.List.of(
  10,
  9,
  13),

            hydra.lib.strings.ToList.apply("\n\t\r"));

    }

    @Test

    public void testTolistNullCharacter() {

        assertEquals(

            java.util.List.of(
  104,
  0,
  105),

            hydra.lib.strings.ToList.apply("h\u0000i"));

    }

    // toLower

    @Test

    public void testTolowerMixedCase() {

        assertEquals(

            "hello world",

            hydra.lib.strings.ToLower.apply("Hello World"));

    }

    @Test

    public void testTolowerAllUppercase() {

        assertEquals(

            "hello",

            hydra.lib.strings.ToLower.apply("HELLO"));

    }

    @Test

    public void testTolowerAllLowercase() {

        assertEquals(

            "hello",

            hydra.lib.strings.ToLower.apply("hello"));

    }

    @Test

    public void testTolowerEmptyString() {

        assertEquals(

            "",

            hydra.lib.strings.ToLower.apply(""));

    }

    @Test

    public void testTolowerWithNumbersAndPunctuation() {

        assertEquals(

            "abc123, xyz!",

            hydra.lib.strings.ToLower.apply("Abc123, XYZ!"));

    }

    @Test

    public void testTolowerControlCharacters() {

        assertEquals(

            "\n\t\r",

            hydra.lib.strings.ToLower.apply("\n\t\r"));

    }

    @Test

    public void testTolowerUnicodeAccentedChars() {

        assertEquals(

            "\u00F1\u00E1\u00E9\u00ED\u00F3\u00FA",

            hydra.lib.strings.ToLower.apply("\u00D1\u00C1\u00C9\u00CD\u00D3\u00DA"));

    }

    // toUpper

    @Test

    public void testToupperMixedCase() {

        assertEquals(

            "HELLO WORLD",

            hydra.lib.strings.ToUpper.apply("hello World"));

    }

    @Test

    public void testToupperAllLowercase() {

        assertEquals(

            "HELLO",

            hydra.lib.strings.ToUpper.apply("hello"));

    }

    @Test

    public void testToupperAllUppercase() {

        assertEquals(

            "HELLO",

            hydra.lib.strings.ToUpper.apply("HELLO"));

    }

    @Test

    public void testToupperEmptyString() {

        assertEquals(

            "",

            hydra.lib.strings.ToUpper.apply(""));

    }

    @Test

    public void testToupperWithNumbersAndPunctuation() {

        assertEquals(

            "ABC123, XYZ!",

            hydra.lib.strings.ToUpper.apply("abc123, xyz!"));

    }

    @Test

    public void testToupperControlCharacters() {

        assertEquals(

            "\n\t\r",

            hydra.lib.strings.ToUpper.apply("\n\t\r"));

    }

    @Test

    public void testToupperUnicodeAccentedChars() {

        assertEquals(

            "\u00D1\u00C1\u00C9\u00CD\u00D3\u00DA",

            hydra.lib.strings.ToUpper.apply("\u00F1\u00E1\u00E9\u00ED\u00F3\u00FA"));

    }

    // unlines

    @Test

    public void testUnlinesMultipleLines() {

        assertEquals(

            "one\ntwo\nthree\n",

            hydra.lib.strings.Unlines.apply(java.util.List.of(
  "one",
  "two",
  "three")));

    }

    @Test

    public void testUnlinesSingleLine() {

        assertEquals(

            "hello\n",

            hydra.lib.strings.Unlines.apply(java.util.List.of("hello")));

    }

    @Test

    public void testUnlinesEmptyList() {

        assertEquals(

            "",

            hydra.lib.strings.Unlines.apply((java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testUnlinesWithEmptyLines() {

        assertEquals(

            "hello\n\nworld\n",

            hydra.lib.strings.Unlines.apply(java.util.List.of(
  "hello",
  "",
  "world")));

    }

    @Test

    public void testUnlinesAllEmptyLines() {

        assertEquals(

            "\n\n\n",

            hydra.lib.strings.Unlines.apply(java.util.List.of(
  "",
  "",
  "")));

    }

    @Test

    public void testUnlinesUnicodeContent() {

        assertEquals(

            "\u00F1o\u00F1o\n\u4E16\u754C\n",

            hydra.lib.strings.Unlines.apply(java.util.List.of(
  "\u00F1o\u00F1o",
  "\u4E16\u754C")));

    }
}
