// Note: this is an automatically generated file. Do not edit.
// hydra.lib.lists primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class ListsTest {

    // apply

    // string transformations

    @Test

    public void testApplyStringTransformationsStringTransformations() {

        assertEquals(

            hydra.util.ConsList.of(
  "ONE",
  "TWO",
  "THREE",
  "one",
  "two",
  "three"),

            hydra.lib.lists.Apply.apply(
  hydra.util.ConsList.of(
    hydra.lib.strings.ToUpper::apply,
    hydra.lib.strings.ToLower::apply),
  hydra.util.ConsList.of(
    "One",
    "Two",
    "Three")));

    }

    // edge cases

    @Test

    public <T0, T1> void testApplyEdgeCasesEmptyFunctionList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Apply.apply(
  (hydra.util.ConsList<java.util.function.Function<String, T1>>) (hydra.util.ConsList.<java.util.function.Function<String, T1>>empty()),
  hydra.util.ConsList.of(
    "a",
    "b")));

    }

    @Test

    public <T0> void testApplyEdgeCasesEmptyInputList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Apply.apply(
  hydra.util.ConsList.of(hydra.lib.strings.ToUpper::apply),
  (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())));

    }

    @Test

    public void testApplyEdgeCasesSingleFunction() {

        assertEquals(

            hydra.util.ConsList.of("HELLO"),

            hydra.lib.lists.Apply.apply(
  hydra.util.ConsList.of(hydra.lib.strings.ToUpper::apply),
  hydra.util.ConsList.of("hello")));

    }

    @Test

    public void testApplyEdgeCasesSingleInput() {

        assertEquals(

            hydra.util.ConsList.of(
  "TEST",
  "test"),

            hydra.lib.lists.Apply.apply(
  hydra.util.ConsList.of(
    hydra.lib.strings.ToUpper::apply,
    hydra.lib.strings.ToLower::apply),
  hydra.util.ConsList.of("Test")));

    }

    // at

    @Test

    public void testAtFirstElement() {

        assertEquals(

            1,

            hydra.lib.lists.At.apply(
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testAtMiddleElement() {

        assertEquals(

            2,

            hydra.lib.lists.At.apply(
  1,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testAtLastElement() {

        assertEquals(

            3,

            hydra.lib.lists.At.apply(
  2,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testAtSingleElementList() {

        assertEquals(

            42,

            hydra.lib.lists.At.apply(
  0,
  hydra.util.ConsList.of(42)));

    }

    @Test

    public void testAtStringListAccess() {

        assertEquals(

            "world",

            hydra.lib.lists.At.apply(
  1,
  hydra.util.ConsList.of(
    "hello",
    "world")));

    }

    // bind

    @Test

    public void testBindNegationFunction() {

        assertEquals(

            hydra.util.ConsList.of(
  -1,
  -2,
  -3,
  -4),

            hydra.lib.lists.Bind.apply(
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4),
  (java.util.function.Function<Integer, hydra.util.ConsList<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    @Test

    public <T0> void testBindEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Bind.apply(
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
  (java.util.function.Function<Integer, hydra.util.ConsList<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    @Test

    public void testBindSingleElement() {

        assertEquals(

            hydra.util.ConsList.of(-5),

            hydra.lib.lists.Bind.apply(
  hydra.util.ConsList.of(5),
  (java.util.function.Function<Integer, hydra.util.ConsList<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    @Test

    public void testBindDuplicateElements() {

        assertEquals(

            hydra.util.ConsList.of(
  -1,
  -1,
  -2),

            hydra.lib.lists.Bind.apply(
  hydra.util.ConsList.of(
    1,
    1,
    2),
  (java.util.function.Function<Integer, hydra.util.ConsList<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    // concat

    @Test

    public void testConcatMultipleNonNegemptyLists() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8),

            hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    4,
    5),
  hydra.util.ConsList.of(
    6,
    7,
    8))));

    }

    @Test

    public void testConcatEmptyListsIncluded() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
  hydra.util.ConsList.of(
    1,
    2),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
  hydra.util.ConsList.of(3))));

    }

    @Test

    public void testConcatSingleList() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2,
  3))));

    }

    @Test

    public <T0, T4> void testConcatAllEmptyLists() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
  (hydra.util.ConsList<T4>) (hydra.util.ConsList.<T4>empty()),
  (hydra.util.ConsList<T4>) (hydra.util.ConsList.<T4>empty()),
  (hydra.util.ConsList<T4>) (hydra.util.ConsList.<T4>empty()))));

    }

    @Test

    public <T0> void testConcatEmptyListOfLists() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Concat.apply((hydra.util.ConsList<hydra.util.ConsList<T0>>) (hydra.util.ConsList.<hydra.util.ConsList<T0>>empty())));

    }

    // concat2

    @Test

    public void testConcat2TwoNonNegemptyLists() {

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

    public void testConcat2FirstListEmpty() {

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

    public void testConcat2SecondListEmpty() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2),

            hydra.lib.lists.Concat2.apply(
  hydra.util.ConsList.of(
    1,
    2),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public <T0, T3> void testConcat2BothListsEmpty() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Concat2.apply(
  (hydra.util.ConsList<T3>) (hydra.util.ConsList.<T3>empty()),
  (hydra.util.ConsList<T3>) (hydra.util.ConsList.<T3>empty())));

    }

    @Test

    public void testConcat2SingleElements() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2),

            hydra.lib.lists.Concat2.apply(
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2)));

    }

    @Test

    public void testConcat2StringLists() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  "c",
  "d"),

            hydra.lib.lists.Concat2.apply(
  hydra.util.ConsList.of(
    "a",
    "b"),
  hydra.util.ConsList.of(
    "c",
    "d")));

    }

    // cons

    @Test

    public void testConsConsToNonNegemptyList() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Cons.apply(
  1,
  hydra.util.ConsList.of(
    2,
    3)));

    }

    @Test

    public void testConsConsToEmptyList() {

        assertEquals(

            hydra.util.ConsList.of(1),

            hydra.lib.lists.Cons.apply(
  1,
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public void testConsConsNegativeNumber() {

        assertEquals(

            hydra.util.ConsList.of(
  -1,
  2,
  3),

            hydra.lib.lists.Cons.apply(
  -1,
  hydra.util.ConsList.of(
    2,
    3)));

    }

    @Test

    public void testConsConsString() {

        assertEquals(

            hydra.util.ConsList.of(
  "hello",
  "world"),

            hydra.lib.lists.Cons.apply(
  "hello",
  hydra.util.ConsList.of("world")));

    }

    // drop

    @Test

    public void testDropDropFromBeginning() {

        assertEquals(

            hydra.util.ConsList.of(
  3,
  4,
  5),

            hydra.lib.lists.Drop.apply(
  2,
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4,
    5)));

    }

    @Test

    public void testDropDropZeroElements() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Drop.apply(
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testDropDropAllElements() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Drop.apply(
  3,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testDropDropMoreThanLength() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Drop.apply(
  5,
  hydra.util.ConsList.of(
    1,
    2)));

    }

    @Test

    public <T0, T2> void testDropDropFromEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Drop.apply(
  3,
  (hydra.util.ConsList<T2>) (hydra.util.ConsList.<T2>empty())));

    }

    @Test

    public void testDropDropNegativeAmount() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Drop.apply(
  -1,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    // dropWhile

    @Test

    public void testDropwhileDropWhileLessThan3() {

        assertEquals(

            hydra.util.ConsList.of(
  3,
  2,
  1),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    3)),
  hydra.util.ConsList.of(
    1,
    2,
    3,
    2,
    1)));

    }

    @Test

    public <T0> void testDropwhileDropAllElements() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testDropwhileDropNoElements() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    0)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testDropwhileEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    5)),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    // elem

    @Test

    public void testElemElementPresent() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  2,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testElemElementNotPresent() {

        assertEquals(

            false,

            hydra.lib.lists.Elem.apply(
  4,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testElemEmptyList() {

        assertEquals(

            false,

            hydra.lib.lists.Elem.apply(
  1,
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public void testElemSingleElementPresent() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  1,
  hydra.util.ConsList.of(1)));

    }

    @Test

    public void testElemSingleElementNotPresent() {

        assertEquals(

            false,

            hydra.lib.lists.Elem.apply(
  2,
  hydra.util.ConsList.of(1)));

    }

    @Test

    public void testElemDuplicateElements() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  2,
  hydra.util.ConsList.of(
    1,
    2,
    2,
    3)));

    }

    @Test

    public void testElemStringElementPresent() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  "hello",
  hydra.util.ConsList.of(
    "world",
    "hello",
    "test")));

    }

    @Test

    public void testElemStringElementNotPresent() {

        assertEquals(

            false,

            hydra.lib.lists.Elem.apply(
  "missing",
  hydra.util.ConsList.of(
    "world",
    "hello")));

    }

    // filter

    @Test

    public void testFilterFilterPositiveNumbers() {

        assertEquals(

            hydra.util.ConsList.of(
  2,
  4,
  5),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  hydra.util.ConsList.of(
    -1,
    2,
    -3,
    4,
    5)));

    }

    @Test

    public void testFilterFilterAllElements() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testFilterFilterNoElements() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testFilterEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    // find

    @Test

    public void testFindFindExistingElement() {

        assertEquals(

            hydra.util.Maybe.just(4),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    3)),
  hydra.util.ConsList.of(
    1,
    2,
    4,
    5)));

    }

    @Test

    public void testFindFindFirstMatching() {

        assertEquals(

            hydra.util.Maybe.just(1),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testFindFindNoMatch() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testFindFindInEmptyList() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public void testFindFindSingleElement() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Equal.apply(
    x,
    42)),
  hydra.util.ConsList.of(42)));

    }

    // foldl

    @Test

    public void testFoldlSumWithAddition() {

        assertEquals(

            10,

            hydra.lib.lists.Foldl.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4)));

    }

    @Test

    public void testFoldlProductWithMultiplication() {

        assertEquals(

            24,

            hydra.lib.lists.Foldl.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Mul.apply(
    p0,
    p1)),
  1,
  hydra.util.ConsList.of(
    2,
    3,
    4)));

    }

    @Test

    public void testFoldlEmptyList() {

        assertEquals(

            5,

            hydra.lib.lists.Foldl.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  5,
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public void testFoldlSingleElement() {

        assertEquals(

            15,

            hydra.lib.lists.Foldl.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  10,
  hydra.util.ConsList.of(5)));

    }

    @Test

    public void testFoldlSubtractionFold() {

        assertEquals(

            4,

            hydra.lib.lists.Foldl.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Sub.apply(
    p0,
    p1)),
  10,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    // foldr

    @Test

    public void testFoldrSubtractionFoldRight() {

        assertEquals(

            2,

            hydra.lib.lists.Foldr.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Sub.apply(
    p0,
    p1)),
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testFoldrEmptyList() {

        assertEquals(

            5,

            hydra.lib.lists.Foldr.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  5,
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public void testFoldrSingleElement() {

        assertEquals(

            15,

            hydra.lib.lists.Foldr.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  10,
  hydra.util.ConsList.of(5)));

    }

    @Test

    public void testFoldrSumWithAddition() {

        assertEquals(

            10,

            hydra.lib.lists.Foldr.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4)));

    }

    @Test

    public void testFoldrSubtractionVsFoldl() {

        assertEquals(

            -8,

            hydra.lib.lists.Foldr.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Sub.apply(
    p0,
    p1)),
  10,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    // group

    @Test

    public void testGroupConsecutiveDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    1),
  hydra.util.ConsList.of(
    2,
    2,
    2),
  hydra.util.ConsList.of(3),
  hydra.util.ConsList.of(1)),

            hydra.lib.lists.Group.apply(hydra.util.ConsList.of(
  1,
  1,
  2,
  2,
  2,
  3,
  1)));

    }

    @Test

    public void testGroupNoDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2),
  hydra.util.ConsList.of(3)),

            hydra.lib.lists.Group.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testGroupAllSame() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  1,
  1)),

            hydra.lib.lists.Group.apply(hydra.util.ConsList.of(
  1,
  1,
  1)));

    }

    @Test

    public <T0, T1> void testGroupEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Group.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testGroupSingleElement() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(1)),

            hydra.lib.lists.Group.apply(hydra.util.ConsList.of(1)));

    }

    // head

    @Test

    public void testHeadThreeElementList() {

        assertEquals(

            1,

            hydra.lib.lists.Head.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testHeadSingleElementList() {

        assertEquals(

            42,

            hydra.lib.lists.Head.apply(hydra.util.ConsList.of(42)));

    }

    @Test

    public void testHeadNegativeNumbers() {

        assertEquals(

            -1,

            hydra.lib.lists.Head.apply(hydra.util.ConsList.of(
  -1,
  -2,
  -3)));

    }

    @Test

    public void testHeadStringList() {

        assertEquals(

            "hello",

            hydra.lib.lists.Head.apply(hydra.util.ConsList.of(
  "hello",
  "world")));

    }

    // init

    @Test

    public void testInitMultipleElements() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Init.apply(hydra.util.ConsList.of(
  1,
  2,
  3,
  4)));

    }

    @Test

    public void testInitTwoElements() {

        assertEquals(

            hydra.util.ConsList.of(1),

            hydra.lib.lists.Init.apply(hydra.util.ConsList.of(
  1,
  2)));

    }

    @Test

    public <T0> void testInitSingleElement() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Init.apply(hydra.util.ConsList.of(1)));

    }

    @Test

    public void testInitStringList() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b"),

            hydra.lib.lists.Init.apply(hydra.util.ConsList.of(
  "a",
  "b",
  "c")));

    }

    // intercalate

    @Test

    public void testIntercalateDoubleZeroSeparator() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3,
  0,
  0,
  4,
  5,
  0,
  0,
  6,
  7,
  8),

            hydra.lib.lists.Intercalate.apply(
  hydra.util.ConsList.of(
    0,
    0),
  hydra.util.ConsList.of(
    hydra.util.ConsList.of(
      1,
      2,
      3),
    hydra.util.ConsList.of(
      4,
      5),
    hydra.util.ConsList.of(
      6,
      7,
      8))));

    }

    @Test

    public void testIntercalateEmptySeparator() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3,
  4),

            hydra.lib.lists.Intercalate.apply(
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
  hydra.util.ConsList.of(
    hydra.util.ConsList.of(
      1,
      2),
    hydra.util.ConsList.of(
      3,
      4))));

    }

    @Test

    public void testIntercalateSingleElementSeparator() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  99,
  2,
  99,
  3),

            hydra.lib.lists.Intercalate.apply(
  hydra.util.ConsList.of(99),
  hydra.util.ConsList.of(
    hydra.util.ConsList.of(1),
    hydra.util.ConsList.of(2),
    hydra.util.ConsList.of(3))));

    }

    @Test

    public <T0> void testIntercalateEmptyListOfLists() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Intercalate.apply(
  hydra.util.ConsList.of(0),
  (hydra.util.ConsList<hydra.util.ConsList<Integer>>) (hydra.util.ConsList.<hydra.util.ConsList<Integer>>empty())));

    }

    @Test

    public void testIntercalateSingleList() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Intercalate.apply(
  hydra.util.ConsList.of(0),
  hydra.util.ConsList.of(hydra.util.ConsList.of(
    1,
    2,
    3))));

    }

    @Test

    public void testIntercalateListsWithEmptyLists() {

        assertEquals(

            hydra.util.ConsList.of(
  0,
  1,
  0),

            hydra.lib.lists.Intercalate.apply(
  hydra.util.ConsList.of(0),
  hydra.util.ConsList.of(
    (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
    hydra.util.ConsList.of(1),
    (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))));

    }

    // intersperse

    @Test

    public void testIntersperseStringInterspersion() {

        assertEquals(

            hydra.util.ConsList.of(
  "one",
  "and",
  "two",
  "and",
  "three"),

            hydra.lib.lists.Intersperse.apply(
  "and",
  hydra.util.ConsList.of(
    "one",
    "two",
    "three")));

    }

    @Test

    public void testIntersperseSingleElement() {

        assertEquals(

            hydra.util.ConsList.of("only"),

            hydra.lib.lists.Intersperse.apply(
  "x",
  hydra.util.ConsList.of("only")));

    }

    @Test

    public <T0> void testIntersperseEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Intersperse.apply(
  "x",
  (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())));

    }

    @Test

    public void testIntersperseTwoElements() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "+",
  "b"),

            hydra.lib.lists.Intersperse.apply(
  "+",
  hydra.util.ConsList.of(
    "a",
    "b")));

    }

    @Test

    public void testIntersperseNumberInterspersion() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  0,
  2,
  0,
  3),

            hydra.lib.lists.Intersperse.apply(
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    // last

    @Test

    public void testLastThreeElementList() {

        assertEquals(

            3,

            hydra.lib.lists.Last.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testLastSingleElementList() {

        assertEquals(

            42,

            hydra.lib.lists.Last.apply(hydra.util.ConsList.of(42)));

    }

    @Test

    public void testLastNegativeNumbers() {

        assertEquals(

            -3,

            hydra.lib.lists.Last.apply(hydra.util.ConsList.of(
  -1,
  -2,
  -3)));

    }

    @Test

    public void testLastStringList() {

        assertEquals(

            "world",

            hydra.lib.lists.Last.apply(hydra.util.ConsList.of(
  "hello",
  "world")));

    }

    // length

    @Test

    public void testLengthThreeElements() {

        assertEquals(

            3,

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public <T1> void testLengthEmptyList() {

        assertEquals(

            0,

            hydra.lib.lists.Length.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testLengthSingleElement() {

        assertEquals(

            1,

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(42)));

    }

    @Test

    public void testLengthManyElements() {

        assertEquals(

            10,

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10)));

    }

    @Test

    public void testLengthStringList() {

        assertEquals(

            3,

            hydra.lib.lists.Length.apply(hydra.util.ConsList.of(
  "a",
  "b",
  "c")));

    }

    // map

    @Test

    public void testMapStringToUppercase() {

        assertEquals(

            hydra.util.ConsList.of(
  "ONE",
  "TWO"),

            hydra.lib.lists.Map.apply(
  hydra.lib.strings.ToUpper::apply,
  hydra.util.ConsList.of(
    "one",
    "two")));

    }

    @Test

    public <T0> void testMapEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Map.apply(
  hydra.lib.strings.ToUpper::apply,
  (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())));

    }

    @Test

    public void testMapSingleElement() {

        assertEquals(

            hydra.util.ConsList.of("HELLO"),

            hydra.lib.lists.Map.apply(
  hydra.lib.strings.ToUpper::apply,
  hydra.util.ConsList.of("hello")));

    }

    @Test

    public void testMapNumberNegation() {

        assertEquals(

            hydra.util.ConsList.of(
  -1,
  -2,
  -3),

            hydra.lib.lists.Map.apply(
  hydra.lib.math.Negate::apply,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testMapIdentityFunction() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Map.apply(
  (java.util.function.Function<Integer, Integer>) (hydra.lib.equality.Identity::apply),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    // nub

    @Test

    public void testNubRemoveDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3,
  4),

            hydra.lib.lists.Nub.apply(hydra.util.ConsList.of(
  1,
  2,
  1,
  3,
  2,
  4)));

    }

    @Test

    public void testNubNoDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Nub.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testNubAllDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(1),

            hydra.lib.lists.Nub.apply(hydra.util.ConsList.of(
  1,
  1,
  1)));

    }

    @Test

    public <T0, T1> void testNubEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Nub.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testNubSingleElement() {

        assertEquals(

            hydra.util.ConsList.of(1),

            hydra.lib.lists.Nub.apply(hydra.util.ConsList.of(1)));

    }

    @Test

    public void testNubStringDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  "c"),

            hydra.lib.lists.Nub.apply(hydra.util.ConsList.of(
  "a",
  "b",
  "a",
  "c")));

    }

    // null

    @Test

    public <T1> void testNullEmptyIntList() {

        assertEquals(

            true,

            hydra.lib.lists.Null.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testNullSingleElement() {

        assertEquals(

            false,

            hydra.lib.lists.Null.apply(hydra.util.ConsList.of(1)));

    }

    @Test

    public void testNullMultipleElements() {

        assertEquals(

            false,

            hydra.lib.lists.Null.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public <T1> void testNullEmptyStringList() {

        assertEquals(

            true,

            hydra.lib.lists.Null.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testNullNonNegemptyStringList() {

        assertEquals(

            false,

            hydra.lib.lists.Null.apply(hydra.util.ConsList.of("a")));

    }

    // partition

    @Test

    public void testPartitionPartitionGreaterThan3() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>(hydra.util.ConsList.of(
  4,
  5,
  6), hydra.util.ConsList.of(
  1,
  2,
  3)))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    3)),
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4,
    5,
    6)));

    }

    @Test

    public <T1> void testPartitionPartitionAllElements() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<T1>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<T1>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<T1>>(hydra.util.ConsList.of(
  1,
  2,
  3), (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testPartitionPartitionNoElements() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<Integer>>((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()), hydra.util.ConsList.of(
  1,
  2,
  3)))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testPartitionPartitionEvenNumbers() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>(hydra.util.ConsList.of(
  2,
  4,
  6), hydra.util.ConsList.of(
  1,
  3,
  5)))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.math.Even.apply(x)),
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4,
    5,
    6)));

    }

    @Test

    public <T0, T1> void testPartitionEmptyList() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) ((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) (new hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()), (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    5)),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    // pure

    @Test

    public void testPureStringElement() {

        assertEquals(

            hydra.util.ConsList.of("one"),

            hydra.lib.lists.Pure.apply("one"));

    }

    @Test

    public void testPureEmptyString() {

        assertEquals(

            hydra.util.ConsList.of(""),

            hydra.lib.lists.Pure.apply(""));

    }

    @Test

    public void testPureNumberElement() {

        assertEquals(

            hydra.util.ConsList.of(42),

            hydra.lib.lists.Pure.apply(42));

    }

    @Test

    public void testPureNegativeNumber() {

        assertEquals(

            hydra.util.ConsList.of(-5),

            hydra.lib.lists.Pure.apply(-5));

    }

    // replicate

    @Test

    public void testReplicateReplicateThreeTimes() {

        assertEquals(

            hydra.util.ConsList.of(
  42,
  42,
  42),

            hydra.lib.lists.Replicate.apply(
  3,
  42));

    }

    @Test

    public <T0> void testReplicateReplicateZeroTimes() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Replicate.apply(
  0,
  1));

    }

    @Test

    public void testReplicateReplicateOnce() {

        assertEquals(

            hydra.util.ConsList.of(99),

            hydra.lib.lists.Replicate.apply(
  1,
  99));

    }

    @Test

    public void testReplicateReplicateString() {

        assertEquals(

            hydra.util.ConsList.of(
  "hello",
  "hello"),

            hydra.lib.lists.Replicate.apply(
  2,
  "hello"));

    }

    // reverse

    @Test

    public void testReverseMultipleElements() {

        assertEquals(

            hydra.util.ConsList.of(
  4,
  3,
  2,
  1),

            hydra.lib.lists.Reverse.apply(hydra.util.ConsList.of(
  1,
  2,
  3,
  4)));

    }

    @Test

    public void testReverseSingleElement() {

        assertEquals(

            hydra.util.ConsList.of(1),

            hydra.lib.lists.Reverse.apply(hydra.util.ConsList.of(1)));

    }

    @Test

    public <T0, T1> void testReverseEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Reverse.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testReverseTwoElements() {

        assertEquals(

            hydra.util.ConsList.of(
  2,
  1),

            hydra.lib.lists.Reverse.apply(hydra.util.ConsList.of(
  1,
  2)));

    }

    @Test

    public void testReverseStringList() {

        assertEquals(

            hydra.util.ConsList.of(
  "c",
  "b",
  "a"),

            hydra.lib.lists.Reverse.apply(hydra.util.ConsList.of(
  "a",
  "b",
  "c")));

    }

    // safeHead

    @Test

    public void testSafeheadNonNegemptyIntList() {

        assertEquals(

            hydra.util.Maybe.just(1),

            hydra.lib.lists.SafeHead.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public <T0, T1> void testSafeheadEmptyIntList() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.lists.SafeHead.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testSafeheadSingleElement() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.lib.lists.SafeHead.apply(hydra.util.ConsList.of(42)));

    }

    @Test

    public void testSafeheadNonNegemptyStringList() {

        assertEquals(

            hydra.util.Maybe.just("hello"),

            hydra.lib.lists.SafeHead.apply(hydra.util.ConsList.of(
  "hello",
  "world")));

    }

    @Test

    public <T0, T1> void testSafeheadEmptyStringList() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.lists.SafeHead.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    // singleton

    @Test

    public void testSingletonNumberElement() {

        assertEquals(

            hydra.util.ConsList.of(42),

            hydra.lib.lists.Singleton.apply(42));

    }

    @Test

    public void testSingletonNegativeNumber() {

        assertEquals(

            hydra.util.ConsList.of(-1),

            hydra.lib.lists.Singleton.apply(-1));

    }

    @Test

    public void testSingletonZero() {

        assertEquals(

            hydra.util.ConsList.of(0),

            hydra.lib.lists.Singleton.apply(0));

    }

    @Test

    public void testSingletonStringElement() {

        assertEquals(

            hydra.util.ConsList.of("hello"),

            hydra.lib.lists.Singleton.apply("hello"));

    }

    // sort

    @Test

    public void testSortUnsortedNumbers() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  1,
  3,
  4,
  5),

            hydra.lib.lists.Sort.apply(hydra.util.ConsList.of(
  3,
  1,
  4,
  1,
  5)));

    }

    @Test

    public void testSortAlreadySorted() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Sort.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testSortReverseSorted() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Sort.apply(hydra.util.ConsList.of(
  3,
  2,
  1)));

    }

    @Test

    public void testSortSingleElement() {

        assertEquals(

            hydra.util.ConsList.of(1),

            hydra.lib.lists.Sort.apply(hydra.util.ConsList.of(1)));

    }

    @Test

    public <T0, T1> void testSortEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Sort.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())));

    }

    @Test

    public void testSortDuplicates() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  1,
  2,
  2,
  3),

            hydra.lib.lists.Sort.apply(hydra.util.ConsList.of(
  2,
  1,
  2,
  3,
  1)));

    }

    @Test

    public void testSortStringSort() {

        assertEquals(

            hydra.util.ConsList.of(
  "apple",
  "banana",
  "zebra"),

            hydra.lib.lists.Sort.apply(hydra.util.ConsList.of(
  "zebra",
  "apple",
  "banana")));

    }

    // sortOn

    @Test

    public void testSortonSortByStringLength() {

        assertEquals(

            hydra.util.ConsList.of(
  "hi",
  "hello",
  "world"),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.strings.Length::apply,
  hydra.util.ConsList.of(
    "hello",
    "hi",
    "world")));

    }

    @Test

    public <T0> void testSortonEmptyStringList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.strings.Length::apply,
  (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())));

    }

    @Test

    public void testSortonSingleStringElement() {

        assertEquals(

            hydra.util.ConsList.of("test"),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.strings.Length::apply,
  hydra.util.ConsList.of("test")));

    }

    @Test

    public void testSortonSortByNegation() {

        assertEquals(

            hydra.util.ConsList.of(
  3,
  2,
  1),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.math.Negate::apply,
  hydra.util.ConsList.of(
    1,
    3,
    2)));

    }

    @Test

    public void testSortonSortByAbsoluteValue() {

        assertEquals(

            hydra.util.ConsList.of(
  -1,
  2,
  -3),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.math.Abs::apply,
  hydra.util.ConsList.of(
    -1,
    -3,
    2)));

    }

    // span

    @Test

    public void testSpanSpanLessThan3() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<Integer>>(hydra.util.ConsList.of(
  1,
  2), hydra.util.ConsList.of(
  3,
  1,
  2)))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    3)),
  hydra.util.ConsList.of(
    1,
    2,
    3,
    1,
    2)));

    }

    @Test

    public <T1> void testSpanSpanAllElements() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<T1>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<T1>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<T1>>(hydra.util.ConsList.of(
  1,
  2,
  3), (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testSpanSpanNoElements() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<Integer>>((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()), hydra.util.ConsList.of(
  1,
  2,
  3)))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0, T1> void testSpanEmptyList() {

        assertEquals(

            (hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) ((hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>) (new hydra.util.Pair<hydra.util.ConsList<T0>, hydra.util.ConsList<T1>>((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()), (hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>empty())))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    5)),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    // tail

    @Test

    public void testTailMultipleElements() {

        assertEquals(

            hydra.util.ConsList.of(
  2,
  3,
  4),

            hydra.lib.lists.Tail.apply(hydra.util.ConsList.of(
  1,
  2,
  3,
  4)));

    }

    @Test

    public void testTailTwoElements() {

        assertEquals(

            hydra.util.ConsList.of(2),

            hydra.lib.lists.Tail.apply(hydra.util.ConsList.of(
  1,
  2)));

    }

    @Test

    public <T0> void testTailSingleElement() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Tail.apply(hydra.util.ConsList.of(1)));

    }

    @Test

    public void testTailStringList() {

        assertEquals(

            hydra.util.ConsList.of(
  "b",
  "c"),

            hydra.lib.lists.Tail.apply(hydra.util.ConsList.of(
  "a",
  "b",
  "c")));

    }

    // take

    @Test

    public void testTakeTakeFromBeginning() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2),

            hydra.lib.lists.Take.apply(
  2,
  hydra.util.ConsList.of(
    1,
    2,
    3,
    4,
    5)));

    }

    @Test

    public <T0> void testTakeTakeZeroElements() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Take.apply(
  0,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testTakeTakeAllElements() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.lists.Take.apply(
  3,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public void testTakeTakeMoreThanLength() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2),

            hydra.lib.lists.Take.apply(
  5,
  hydra.util.ConsList.of(
    1,
    2)));

    }

    @Test

    public <T0, T2> void testTakeTakeFromEmptyList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Take.apply(
  3,
  (hydra.util.ConsList<T2>) (hydra.util.ConsList.<T2>empty())));

    }

    @Test

    public <T0> void testTakeTakeNegativeAmount() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Take.apply(
  -1,
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    // transpose

    @Test

    public void testTransposeSquareMatrix() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    4),
  hydra.util.ConsList.of(
    2,
    5),
  hydra.util.ConsList.of(
    3,
    6)),

            hydra.lib.lists.Transpose.apply(hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    4,
    5,
    6))));

    }

    @Test

    public <T0> void testTransposeEmptyLists() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Transpose.apply((hydra.util.ConsList<hydra.util.ConsList<T0>>) (hydra.util.ConsList.<hydra.util.ConsList<T0>>empty())));

    }

    @Test

    public void testTransposeSingleRow() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2),
  hydra.util.ConsList.of(3)),

            hydra.lib.lists.Transpose.apply(hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2,
  3))));

    }

    @Test

    public void testTransposeSingleColumn() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2,
  3)),

            hydra.lib.lists.Transpose.apply(hydra.util.ConsList.of(
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2),
  hydra.util.ConsList.of(3))));

    }

    @Test

    public void testTransposeRaggedMatrix() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    3,
    4),
  hydra.util.ConsList.of(
    2,
    5),
  hydra.util.ConsList.of(6)),

            hydra.lib.lists.Transpose.apply(hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    2),
  hydra.util.ConsList.of(3),
  hydra.util.ConsList.of(
    4,
    5,
    6))));

    }

    // zip

    @Test

    public void testZipEqualLengthLists() {

        assertEquals(

            hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(3, "c")))),

            hydra.lib.lists.Zip.apply(
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    "a",
    "b",
    "c")));

    }

    @Test

    public void testZipFirstListShorter() {

        assertEquals(

            hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b")))),

            hydra.lib.lists.Zip.apply(
  hydra.util.ConsList.of(
    1,
    2),
  hydra.util.ConsList.of(
    "a",
    "b",
    "c")));

    }

    @Test

    public void testZipSecondListShorter() {

        assertEquals(

            hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b")))),

            hydra.lib.lists.Zip.apply(
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    "a",
    "b")));

    }

    @Test

    public <T0, T2> void testZipEmptyFirstList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Zip.apply(
  (hydra.util.ConsList<T2>) (hydra.util.ConsList.<T2>empty()),
  hydra.util.ConsList.of(
    "a",
    "b")));

    }

    @Test

    public <T0, T4> void testZipEmptySecondList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Zip.apply(
  hydra.util.ConsList.of(
    1,
    2),
  (hydra.util.ConsList<T4>) (hydra.util.ConsList.<T4>empty())));

    }

    @Test

    public <T0, T2, T4> void testZipBothEmptyLists() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.Zip.apply(
  (hydra.util.ConsList<T2>) (hydra.util.ConsList.<T2>empty()),
  (hydra.util.ConsList<T4>) (hydra.util.ConsList.<T4>empty())));

    }

    // zipWith

    @Test

    public void testZipwithAddition() {

        assertEquals(

            hydra.util.ConsList.of(
  5,
  7,
  9),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    4,
    5,
    6)));

    }

    @Test

    public void testZipwithFirstListShorter() {

        assertEquals(

            hydra.util.ConsList.of(
  5,
  7),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  hydra.util.ConsList.of(
    1,
    2),
  hydra.util.ConsList.of(
    4,
    5,
    6)));

    }

    @Test

    public void testZipwithSecondListShorter() {

        assertEquals(

            hydra.util.ConsList.of(
  5,
  7),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    4,
    5)));

    }

    @Test

    public <T0> void testZipwithEmptyFirstList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()),
  hydra.util.ConsList.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testZipwithEmptySecondList() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  hydra.util.ConsList.of(
    1,
    2,
    3),
  (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())));

    }

    @Test

    public void testZipwithStringConcatenation() {

        assertEquals(

            hydra.util.ConsList.of(
  "a1",
  "b2"),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.lib.strings.Cat2.apply(
    p0,
    p1)),
  hydra.util.ConsList.of(
    "a",
    "b"),
  hydra.util.ConsList.of(
    "1",
    "2")));

    }
}
