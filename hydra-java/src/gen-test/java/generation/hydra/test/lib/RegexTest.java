// Note: this is an automatically generated file. Do not edit.
// hydra.lib.regex primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class RegexTest {

    // matches

    @Test

    public void testMatchesExactMatch() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "hello",
  "hello"));

    }

    @Test

    public void testMatchesPatternMatch() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "[a-z]+",
  "hello"));

    }

    @Test

    public void testMatchesNoMatch() {

        assertEquals(

            false,

            hydra.lib.regex.Matches.apply(
  "[0-9]+",
  "hello"));

    }

    @Test

    public void testMatchesPartialContentDoesNotMatch() {

        assertEquals(

            false,

            hydra.lib.regex.Matches.apply(
  "[a-z]+",
  "hello123"));

    }

    @Test

    public void testMatchesDigitPattern() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "[0-9]+",
  "12345"));

    }

    @Test

    public void testMatchesMixedPattern() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "[a-z]+[0-9]+",
  "hello123"));

    }

    @Test

    public void testMatchesEmptyPatternMatchesEmpty() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "",
  ""));

    }

    @Test

    public void testMatchesEmptyPatternDoesNotMatchNonNegempty() {

        assertEquals(

            false,

            hydra.lib.regex.Matches.apply(
  "",
  "hello"));

    }

    @Test

    public void testMatchesStarMatchesEmpty() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "a*",
  ""));

    }

    @Test

    public void testMatchesAlternation() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "cat|dog",
  "cat"));

    }

    @Test

    public void testMatchesAlternationSecond() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "cat|dog",
  "dog"));

    }

    @Test

    public void testMatchesAlternationNoMatch() {

        assertEquals(

            false,

            hydra.lib.regex.Matches.apply(
  "cat|dog",
  "bird"));

    }

    @Test

    public void testMatchesQuantifier() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "ab?c",
  "ac"));

    }

    @Test

    public void testMatchesQuantifierWithOptional() {

        assertEquals(

            true,

            hydra.lib.regex.Matches.apply(
  "ab?c",
  "abc"));

    }

    // find

    @Test

    public void testFindSimpleFind() {

        assertEquals(

            hydra.util.Maybe.just("123"),

            hydra.lib.regex.Find.apply(
  "[0-9]+",
  "abc123def"));

    }

    @Test

    public <T0> void testFindNoMatch() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.regex.Find.apply(
  "[0-9]+",
  "abcdef"));

    }

    @Test

    public void testFindFindFirst() {

        assertEquals(

            hydra.util.Maybe.just("abc"),

            hydra.lib.regex.Find.apply(
  "[a-z]+",
  "123abc456def"));

    }

    @Test

    public <T0> void testFindEmptyInput() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.regex.Find.apply(
  "[0-9]+",
  ""));

    }

    @Test

    public void testFindFullMatch() {

        assertEquals(

            hydra.util.Maybe.just("hello"),

            hydra.lib.regex.Find.apply(
  ".*",
  "hello"));

    }

    // findAll

    @Test

    public void testFindallMultipleMatches() {

        assertEquals(

            hydra.util.ConsList.of(
  "1",
  "2",
  "3"),

            hydra.lib.regex.FindAll.apply(
  "[0-9]+",
  "a1b2c3"));

    }

    @Test

    public <T0> void testFindallNoMatches() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.regex.FindAll.apply(
  "[0-9]+",
  "abc"));

    }

    @Test

    public void testFindallOverlappingWords() {

        assertEquals(

            hydra.util.ConsList.of(
  "abc",
  "def",
  "ghi"),

            hydra.lib.regex.FindAll.apply(
  "[a-z]+",
  "abc def ghi"));

    }

    @Test

    public void testFindallSingleMatch() {

        assertEquals(

            hydra.util.ConsList.of("hello"),

            hydra.lib.regex.FindAll.apply(
  "hello",
  "say hello world"));

    }

    // replace

    @Test

    public void testReplaceBasicReplace() {

        assertEquals(

            "abcXdef456",

            hydra.lib.regex.Replace.apply(
  "[0-9]+",
  "X",
  "abc123def456"));

    }

    @Test

    public void testReplaceNoMatch() {

        assertEquals(

            "abcdef",

            hydra.lib.regex.Replace.apply(
  "[0-9]+",
  "X",
  "abcdef"));

    }

    @Test

    public void testReplaceReplaceAtStart() {

        assertEquals(

            "X123",

            hydra.lib.regex.Replace.apply(
  "^[a-z]+",
  "X",
  "abc123"));

    }

    @Test

    public void testReplaceEmptyReplacement() {

        assertEquals(

            "abcdef",

            hydra.lib.regex.Replace.apply(
  "[0-9]+",
  "",
  "abc123def"));

    }

    // replaceAll

    @Test

    public void testReplaceallReplaceAllDigits() {

        assertEquals(

            "aXbXcX",

            hydra.lib.regex.ReplaceAll.apply(
  "[0-9]+",
  "X",
  "a1b2c3"));

    }

    @Test

    public void testReplaceallNoMatch() {

        assertEquals(

            "abc",

            hydra.lib.regex.ReplaceAll.apply(
  "[0-9]+",
  "X",
  "abc"));

    }

    @Test

    public void testReplaceallReplaceAllWords() {

        assertEquals(

            "X 123 X",

            hydra.lib.regex.ReplaceAll.apply(
  "[a-z]+",
  "X",
  "abc 123 def"));

    }

    @Test

    public void testReplaceallEmptyReplacement() {

        assertEquals(

            "abc",

            hydra.lib.regex.ReplaceAll.apply(
  "[0-9]+",
  "",
  "a1b2c3"));

    }

    // split

    @Test

    public void testSplitSplitOnComma() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  "c"),

            hydra.lib.regex.Split.apply(
  ",",
  "a,b,c"));

    }

    @Test

    public void testSplitSplitOnSpaces() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  "c"),

            hydra.lib.regex.Split.apply(
  " +",
  "a b  c"));

    }

    @Test

    public void testSplitNoMatch() {

        assertEquals(

            hydra.util.ConsList.of("abc"),

            hydra.lib.regex.Split.apply(
  ",",
  "abc"));

    }

    @Test

    public void testSplitSplitOnDigits() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  "c"),

            hydra.lib.regex.Split.apply(
  "[0-9]+",
  "a1b2c"));

    }

    @Test

    public void testSplitTrailingDelimiter() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  ""),

            hydra.lib.regex.Split.apply(
  ",",
  "a,b,"));

    }
}
