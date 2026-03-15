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

            hydra.util.ConsList.of(
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

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testPolymorphicPrimitivesLengthOfStringList() {

        assertEquals(

            2,

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(
  "a",
  "b")));

    }

    @Test

    public <T1> void testPolymorphicPrimitivesLengthOfEmptyList() {

        assertEquals(

            0,

            hydra.lib.lists.Length.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testPolymorphicPrimitivesLengthOfSingleElementList() {

        assertEquals(

            1,

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(true)));

    }

    @Test

    public void testPolymorphicPrimitivesHeadOfIntegerList() {

        assertEquals(

            10,

            hydra.lib.lists.Head.apply(hydra.util.ConsList.of(
  10,
  20,
  30)));

    }

    @Test

    public void testPolymorphicPrimitivesHeadOfStringList() {

        assertEquals(

            "first",

            hydra.lib.lists.Head.apply(hydra.util.ConsList.of(
  "first",
  "second")));

    }

    @Test

    public void testPolymorphicPrimitivesLastOfIntegerList() {

        assertEquals(

            30,

            hydra.lib.lists.Last.apply(hydra.util.ConsList.of(
  10,
  20,
  30)));

    }

    @Test

    public void testPolymorphicPrimitivesConcatTwoIntegerLists() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3,
  4),

            hydra.lib.lists.Concat2.apply(
  hydra.util.ConsList.of(
    1,
    2),
  hydra.util.ConsList.of(
    3,
    4)));

    }

    @Test

    public void testPolymorphicPrimitivesConcatWithEmptyList() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2),

            hydra.lib.lists.Concat2.apply(
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
  hydra.util.ConsList.of(
    1,
    2)));

    }

    @Test

    public void testPolymorphicPrimitivesReverseIntegerList() {

        assertEquals(

            hydra.util.ConsList.of(
  3,
  2,
  1),

            hydra.lib.lists.Reverse.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public <T0, T1> void testPolymorphicPrimitivesReverseEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Reverse.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    // nullary primitives

    @Test

    public <T1> void testNullaryPrimitivesEmptySetHasSizeZero() {

        assertEquals(

            0,

            hydra.lib.sets.Size.apply((hydra.util.PersistentSet<T1>) (hydra.lib.sets.Empty.<T1>apply())));

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

    public <T0> void testListReductionEmptyListIsAValue() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()));

    }

    @Test

    public void testListReductionListOfLiteralsIsAValue() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.util.ConsList.of(
  1,
  2,
  3));

    }

    @Test

    public void testListReductionListWithReducibleElement() {

        assertEquals(

            hydra.util.ConsList.of(42),

            hydra.util.ConsList.of(((java.util.function.Function<Integer, Integer>) (x -> x)).apply(42)));

    }

    // optional reduction

    @Test

    public <T0> void testOptionalReductionNothingIsAValue() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()));

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
