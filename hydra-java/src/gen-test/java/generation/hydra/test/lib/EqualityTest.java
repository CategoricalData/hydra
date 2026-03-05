// Note: this is an automatically generated file. Do not edit.
// hydra.lib.equality primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class EqualityTest {

    // compare

    @Test

    public void testCompareLessThan() {

        assertEquals(

            new hydra.util.Comparison.LessThan(),

            hydra.lib.equality.Compare.apply(
  3,
  5));

    }

    @Test

    public void testCompareEqual() {

        assertEquals(

            new hydra.util.Comparison.EqualTo(),

            hydra.lib.equality.Compare.apply(
  5,
  5));

    }

    @Test

    public void testCompareGreaterThan() {

        assertEquals(

            new hydra.util.Comparison.GreaterThan(),

            hydra.lib.equality.Compare.apply(
  5,
  3));

    }

    // equal

    @Test

    public void testEqualEqualIntegers() {

        assertEquals(

            true,

            hydra.lib.equality.Equal.apply(
  5,
  5));

    }

    @Test

    public void testEqualUnequalIntegers() {

        assertEquals(

            false,

            hydra.lib.equality.Equal.apply(
  5,
  3));

    }

    // gt

    @Test

    public void testGtGreater() {

        assertEquals(

            true,

            hydra.lib.equality.Gt.apply(
  5,
  3));

    }

    @Test

    public void testGtEqual() {

        assertEquals(

            false,

            hydra.lib.equality.Gt.apply(
  5,
  5));

    }

    @Test

    public void testGtLess() {

        assertEquals(

            false,

            hydra.lib.equality.Gt.apply(
  3,
  5));

    }

    // gte

    @Test

    public void testGteGreater() {

        assertEquals(

            true,

            hydra.lib.equality.Gte.apply(
  5,
  3));

    }

    @Test

    public void testGteEqual() {

        assertEquals(

            true,

            hydra.lib.equality.Gte.apply(
  5,
  5));

    }

    @Test

    public void testGteLess() {

        assertEquals(

            false,

            hydra.lib.equality.Gte.apply(
  3,
  5));

    }

    // identity

    @Test

    public void testIdentityInteger() {

        assertEquals(

            42,

            hydra.lib.equality.Identity.apply(42));

    }

    // lt

    @Test

    public void testLtLess() {

        assertEquals(

            true,

            hydra.lib.equality.Lt.apply(
  3,
  5));

    }

    @Test

    public void testLtEqual() {

        assertEquals(

            false,

            hydra.lib.equality.Lt.apply(
  5,
  5));

    }

    @Test

    public void testLtGreater() {

        assertEquals(

            false,

            hydra.lib.equality.Lt.apply(
  5,
  3));

    }

    // lte

    @Test

    public void testLteLess() {

        assertEquals(

            true,

            hydra.lib.equality.Lte.apply(
  3,
  5));

    }

    @Test

    public void testLteEqual() {

        assertEquals(

            true,

            hydra.lib.equality.Lte.apply(
  5,
  5));

    }

    @Test

    public void testLteGreater() {

        assertEquals(

            false,

            hydra.lib.equality.Lte.apply(
  5,
  3));

    }

    // max

    @Test

    public void testMaxFirstGreater() {

        assertEquals(

            5,

            hydra.lib.equality.Max.apply(
  5,
  3));

    }

    @Test

    public void testMaxSecondGreater() {

        assertEquals(

            5,

            hydra.lib.equality.Max.apply(
  3,
  5));

    }

    @Test

    public void testMaxEqual() {

        assertEquals(

            5,

            hydra.lib.equality.Max.apply(
  5,
  5));

    }

    // min

    @Test

    public void testMinFirstLess() {

        assertEquals(

            3,

            hydra.lib.equality.Min.apply(
  3,
  5));

    }

    @Test

    public void testMinSecondLess() {

        assertEquals(

            3,

            hydra.lib.equality.Min.apply(
  5,
  3));

    }

    @Test

    public void testMinEqual() {

        assertEquals(

            5,

            hydra.lib.equality.Min.apply(
  5,
  5));

    }

    // compare strings

    @Test

    public void testCompareStringsLessThanLexicographic() {

        assertEquals(

            new hydra.util.Comparison.LessThan(),

            hydra.lib.equality.Compare.apply(
  "apple",
  "banana"));

    }

    @Test

    public void testCompareStringsEqual() {

        assertEquals(

            new hydra.util.Comparison.EqualTo(),

            hydra.lib.equality.Compare.apply(
  "hello",
  "hello"));

    }

    @Test

    public void testCompareStringsGreaterThanLexicographic() {

        assertEquals(

            new hydra.util.Comparison.GreaterThan(),

            hydra.lib.equality.Compare.apply(
  "zebra",
  "apple"));

    }

    @Test

    public void testCompareStringsEmptyVsNonNegempty() {

        assertEquals(

            new hydra.util.Comparison.LessThan(),

            hydra.lib.equality.Compare.apply(
  "",
  "a"));

    }

    @Test

    public void testCompareStringsPrefixVsLonger() {

        assertEquals(

            new hydra.util.Comparison.LessThan(),

            hydra.lib.equality.Compare.apply(
  "ab",
  "abc"));

    }

    // lt strings

    @Test

    public void testLtStringsLessLexicographic() {

        assertEquals(

            true,

            hydra.lib.equality.Lt.apply(
  "apple",
  "banana"));

    }

    @Test

    public void testLtStringsEqual() {

        assertEquals(

            false,

            hydra.lib.equality.Lt.apply(
  "hello",
  "hello"));

    }

    @Test

    public void testLtStringsGreater() {

        assertEquals(

            false,

            hydra.lib.equality.Lt.apply(
  "zebra",
  "apple"));

    }

    // gt strings

    @Test

    public void testGtStringsGreaterLexicographic() {

        assertEquals(

            true,

            hydra.lib.equality.Gt.apply(
  "zebra",
  "apple"));

    }

    @Test

    public void testGtStringsEqual() {

        assertEquals(

            false,

            hydra.lib.equality.Gt.apply(
  "hello",
  "hello"));

    }

    @Test

    public void testGtStringsLess() {

        assertEquals(

            false,

            hydra.lib.equality.Gt.apply(
  "apple",
  "banana"));

    }

    // max strings

    @Test

    public void testMaxStringsFirstGreater() {

        assertEquals(

            "zebra",

            hydra.lib.equality.Max.apply(
  "zebra",
  "apple"));

    }

    @Test

    public void testMaxStringsSecondGreater() {

        assertEquals(

            "zebra",

            hydra.lib.equality.Max.apply(
  "apple",
  "zebra"));

    }

    @Test

    public void testMaxStringsEqual() {

        assertEquals(

            "hello",

            hydra.lib.equality.Max.apply(
  "hello",
  "hello"));

    }

    // min strings

    @Test

    public void testMinStringsFirstLess() {

        assertEquals(

            "apple",

            hydra.lib.equality.Min.apply(
  "apple",
  "zebra"));

    }

    @Test

    public void testMinStringsSecondLess() {

        assertEquals(

            "apple",

            hydra.lib.equality.Min.apply(
  "zebra",
  "apple"));

    }

    @Test

    public void testMinStringsEqual() {

        assertEquals(

            "hello",

            hydra.lib.equality.Min.apply(
  "hello",
  "hello"));

    }

    // compare floats

    @Test

    public void testCompareFloatsLessThan() {

        assertEquals(

            new hydra.util.Comparison.LessThan(),

            hydra.lib.equality.Compare.apply(
  1.5,
  2.5));

    }

    @Test

    public void testCompareFloatsEqual() {

        assertEquals(

            new hydra.util.Comparison.EqualTo(),

            hydra.lib.equality.Compare.apply(
  3.14,
  3.14));

    }

    @Test

    public void testCompareFloatsGreaterThan() {

        assertEquals(

            new hydra.util.Comparison.GreaterThan(),

            hydra.lib.equality.Compare.apply(
  5.0,
  3.0));

    }

    @Test

    public void testCompareFloatsNegativeVsPositive() {

        assertEquals(

            new hydra.util.Comparison.LessThan(),

            hydra.lib.equality.Compare.apply(
  -1.0,
  1.0));

    }

    // lt floats

    @Test

    public void testLtFloatsLess() {

        assertEquals(

            true,

            hydra.lib.equality.Lt.apply(
  1.5,
  2.5));

    }

    @Test

    public void testLtFloatsEqual() {

        assertEquals(

            false,

            hydra.lib.equality.Lt.apply(
  3.14,
  3.14));

    }

    @Test

    public void testLtFloatsGreater() {

        assertEquals(

            false,

            hydra.lib.equality.Lt.apply(
  5.0,
  3.0));

    }

    // gt floats

    @Test

    public void testGtFloatsGreater() {

        assertEquals(

            true,

            hydra.lib.equality.Gt.apply(
  5.0,
  3.0));

    }

    @Test

    public void testGtFloatsEqual() {

        assertEquals(

            false,

            hydra.lib.equality.Gt.apply(
  3.14,
  3.14));

    }

    @Test

    public void testGtFloatsLess() {

        assertEquals(

            false,

            hydra.lib.equality.Gt.apply(
  1.5,
  2.5));

    }
}
