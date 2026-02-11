// Note: this is an automatically generated file. Do not edit.
// reduction

package generation.hydra.test;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class ReductionTest {

    // beta reduction

    @Test

    public void testBetaReductionIdentityFunctionAppliedToLiteral() {

        assertEquals(

            42,

            ((java.util.function.Function<Integer, Integer>) (x -> x)).apply(42));

    }

    @Test

    public void testBetaReductionConstantFunction() {

        assertEquals(

            1,

            ((java.util.function.Function<Integer, Integer>) (x -> 1)).apply(42));

    }

    @Test

    public void testBetaReductionNestedApplication() {

        assertEquals(

            1,

            (((java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (x -> (java.util.function.Function<Integer, Integer>) (y -> x))).apply(1)).apply(2));

    }

    // monomorphic primitives

    @Test

    public void testMonomorphicPrimitivesToupperOnLowercase() {

        assertEquals(

            "HELLO",

            hydra.lib.strings.ToUpper.apply("hello"));

    }

    @Test

    public void testMonomorphicPrimitivesToupperOnMixedCase() {

        assertEquals(

            "HELLO WORLD",

            hydra.lib.strings.ToUpper.apply("Hello World"));

    }

    @Test

    public void testMonomorphicPrimitivesToupperOnEmptyString() {

        assertEquals(

            "",

            hydra.lib.strings.ToUpper.apply(""));

    }

    @Test

    public void testMonomorphicPrimitivesTolowerOnUppercase() {

        assertEquals(

            "hello",

            hydra.lib.strings.ToLower.apply("HELLO"));

    }

    @Test

    public void testMonomorphicPrimitivesStringLength() {

        assertEquals(

            5,

            hydra.lib.strings.Length.apply("hello"));

    }

    @Test

    public void testMonomorphicPrimitivesStringLengthOfEmpty() {

        assertEquals(

            0,

            hydra.lib.strings.Length.apply(""));

    }

    @Test

    public void testMonomorphicPrimitivesAddTwoPositiveIntegers() {

        assertEquals(

            8,

            hydra.lib.math.Add.apply(
  3,
  5));

    }

    @Test

    public void testMonomorphicPrimitivesAddNegativeAndPositive() {

        assertEquals(

            -7,

            hydra.lib.math.Add.apply(
  -10,
  3));

    }

    @Test

    public void testMonomorphicPrimitivesAddWithZero() {

        assertEquals(

            42,

            hydra.lib.math.Add.apply(
  0,
  42));

    }

    @Test

    public void testMonomorphicPrimitivesSubtractIntegers() {

        assertEquals(

            7,

            hydra.lib.math.Sub.apply(
  10,
  3));

    }

    @Test

    public void testMonomorphicPrimitivesMultiplyIntegers() {

        assertEquals(

            42,

            hydra.lib.math.Mul.apply(
  6,
  7));

    }

    @Test

    public void testMonomorphicPrimitivesMultiplyByZero() {

        assertEquals(

            0,

            hydra.lib.math.Mul.apply(
  100,
  0));

    }

    @Test

    public void testMonomorphicPrimitivesDivideIntegers() {

        assertEquals(

            5,

            hydra.lib.math.Div.apply(
  20,
  4));

    }

    @Test

    public void testMonomorphicPrimitivesModulo() {

        assertEquals(

            2,

            hydra.lib.math.Mod.apply(
  17,
  5));

    }

    @Test

    public void testMonomorphicPrimitivesSplitonBasic() {

        assertEquals(

            java.util.List.of(
  "a",
  "b",
  "c"),

            hydra.lib.strings.SplitOn.apply(
  ",",
  "a,b,c"));

    }

    @Test

    public void testMonomorphicPrimitivesCat2Strings() {

        assertEquals(

            "helloworld",

            hydra.lib.strings.Cat2.apply(
  "hello",
  "world"));

    }

    // polymorphic primitives

    @Test

    public void testPolymorphicPrimitivesLengthOfIntegerList() {

        assertEquals(

            3,

            hydra.lib.lists.Length.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testPolymorphicPrimitivesLengthOfStringList() {

        assertEquals(

            2,

            hydra.lib.lists.Length.apply(java.util.List.of(
  "a",
  "b")));

    }

    @Test

    public void testPolymorphicPrimitivesLengthOfEmptyList() {

        assertEquals(

            0,

            hydra.lib.lists.Length.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testPolymorphicPrimitivesLengthOfSingleElementList() {

        assertEquals(

            1,

            hydra.lib.lists.Length.apply(java.util.List.of(true)));

    }

    @Test

    public void testPolymorphicPrimitivesHeadOfIntegerList() {

        assertEquals(

            10,

            hydra.lib.lists.Head.apply(java.util.List.of(
  10,
  20,
  30)));

    }

    @Test

    public void testPolymorphicPrimitivesHeadOfStringList() {

        assertEquals(

            "first",

            hydra.lib.lists.Head.apply(java.util.List.of(
  "first",
  "second")));

    }

    @Test

    public void testPolymorphicPrimitivesLastOfIntegerList() {

        assertEquals(

            30,

            hydra.lib.lists.Last.apply(java.util.List.of(
  10,
  20,
  30)));

    }

    @Test

    public void testPolymorphicPrimitivesConcatTwoIntegerLists() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3,
  4),

            hydra.lib.lists.Concat2.apply(
  java.util.List.of(
    1,
    2),
  java.util.List.of(
    3,
    4)));

    }

    @Test

    public void testPolymorphicPrimitivesConcatWithEmptyList() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.lists.Concat2.apply(
  (java.util.List<Integer>) (java.util.List.<Integer>of()),
  java.util.List.of(
    1,
    2)));

    }

    @Test

    public void testPolymorphicPrimitivesReverseIntegerList() {

        assertEquals(

            java.util.List.of(
  3,
  2,
  1),

            hydra.lib.lists.Reverse.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testPolymorphicPrimitivesReverseEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Reverse.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    // nullary primitives

    @Test

    public void testNullaryPrimitivesEmptySetHasSizeZero() {

        assertEquals(

            0,

            hydra.lib.sets.Size.apply((java.util.Set<java.lang.Object>) (hydra.lib.sets.Empty.<java.lang.Object>apply())));

    }

    // literals as values

    @Test

    public void testLiteralsAsValuesIntegerLiteralIsAValue() {

        assertEquals(

            42,

            42);

    }

    @Test

    public void testLiteralsAsValuesNegativeIntegerLiteral() {

        assertEquals(

            -17,

            -17);

    }

    @Test

    public void testLiteralsAsValuesZeroIntegerLiteral() {

        assertEquals(

            0,

            0);

    }

    @Test

    public void testLiteralsAsValuesStringLiteralIsAValue() {

        assertEquals(

            "hello",

            "hello");

    }

    @Test

    public void testLiteralsAsValuesEmptyStringLiteral() {

        assertEquals(

            "",

            "");

    }

    @Test

    public void testLiteralsAsValuesStringWithSpecialCharacters() {

        assertEquals(

            "hello\nworld\ttab",

            "hello\nworld\ttab");

    }

    @Test

    public void testLiteralsAsValuesBooleanTrueIsAValue() {

        assertEquals(

            true,

            true);

    }

    @Test

    public void testLiteralsAsValuesBooleanFalseIsAValue() {

        assertEquals(

            false,

            false);

    }

    @Test

    public void testLiteralsAsValuesFloatLiteralIsAValue() {

        assertEquals(

            3.14,

            3.14,

            1e-15);

    }

    @Test

    public void testLiteralsAsValuesNegativeFloatLiteral() {

        assertEquals(

            -2.718,

            -2.718,

            1e-15);

    }

    @Test

    public void testLiteralsAsValuesZeroFloatLiteral() {

        assertEquals(

            0.0,

            0.0,

            1e-15);

    }

    // list reduction

    @Test

    public void testListReductionEmptyListIsAValue() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()));

    }

    @Test

    public void testListReductionListOfLiteralsIsAValue() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            java.util.List.of(
  1,
  2,
  3));

    }

    @Test

    public void testListReductionListWithReducibleElement() {

        assertEquals(

            java.util.List.of(42),

            java.util.List.of(((java.util.function.Function<Integer, Integer>) (x -> x)).apply(42)));

    }

    // optional reduction

    @Test

    public void testOptionalReductionNothingIsAValue() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()));

    }

    @Test

    public void testOptionalReductionJustLiteralIsAValue() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.util.Maybe.just(42));

    }

    @Test

    public void testOptionalReductionJustWithReducibleContent() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.util.Maybe.just(((java.util.function.Function<Integer, Integer>) (x -> x)).apply(42)));

    }
}
