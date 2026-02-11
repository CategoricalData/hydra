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

            java.util.List.of(
  "ONE",
  "TWO",
  "THREE",
  "one",
  "two",
  "three"),

            hydra.lib.lists.Apply.apply(
  java.util.List.of(
    hydra.lib.strings.ToUpper::apply,
    hydra.lib.strings.ToLower::apply),
  java.util.List.of(
    "One",
    "Two",
    "Three")));

    }

    // edge cases

    @Test

    public void testApplyEdgeCasesEmptyFunctionList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Apply.apply(
  (java.util.List<java.util.function.Function<String, java.lang.Object>>) (java.util.List.<java.util.function.Function<String, java.lang.Object>>of()),
  java.util.List.of(
    "a",
    "b")));

    }

    @Test

    public void testApplyEdgeCasesEmptyInputList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Apply.apply(
  java.util.List.of(hydra.lib.strings.ToUpper::apply),
  (java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testApplyEdgeCasesSingleFunction() {

        assertEquals(

            java.util.List.of("HELLO"),

            hydra.lib.lists.Apply.apply(
  java.util.List.of(hydra.lib.strings.ToUpper::apply),
  java.util.List.of("hello")));

    }

    @Test

    public void testApplyEdgeCasesSingleInput() {

        assertEquals(

            java.util.List.of(
  "TEST",
  "test"),

            hydra.lib.lists.Apply.apply(
  java.util.List.of(
    hydra.lib.strings.ToUpper::apply,
    hydra.lib.strings.ToLower::apply),
  java.util.List.of("Test")));

    }

    // at

    @Test

    public void testAtFirstElement() {

        assertEquals(

            1,

            hydra.lib.lists.At.apply(
  0,
  java.util.List.of(
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
  java.util.List.of(
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
  java.util.List.of(
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
  java.util.List.of(42)));

    }

    @Test

    public void testAtStringListAccess() {

        assertEquals(

            "world",

            hydra.lib.lists.At.apply(
  1,
  java.util.List.of(
    "hello",
    "world")));

    }

    // bind

    @Test

    public void testBindNegationFunction() {

        assertEquals(

            java.util.List.of(
  -1,
  -2,
  -3,
  -4),

            hydra.lib.lists.Bind.apply(
  java.util.List.of(
    1,
    2,
    3,
    4),
  (java.util.function.Function<Integer, java.util.List<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    @Test

    public void testBindEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Bind.apply(
  (java.util.List<Integer>) (java.util.List.<Integer>of()),
  (java.util.function.Function<Integer, java.util.List<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    @Test

    public void testBindSingleElement() {

        assertEquals(

            java.util.List.of(-5),

            hydra.lib.lists.Bind.apply(
  java.util.List.of(5),
  (java.util.function.Function<Integer, java.util.List<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    @Test

    public void testBindDuplicateElements() {

        assertEquals(

            java.util.List.of(
  -1,
  -1,
  -2),

            hydra.lib.lists.Bind.apply(
  java.util.List.of(
    1,
    1,
    2),
  (java.util.function.Function<Integer, java.util.List<Integer>>) (x -> hydra.lib.lists.Pure.apply(hydra.lib.math.Negate.apply(x)))));

    }

    // concat

    @Test

    public void testConcatMultipleNonNegemptyLists() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8),

            hydra.lib.lists.Concat.apply(java.util.List.of(
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    4,
    5),
  java.util.List.of(
    6,
    7,
    8))));

    }

    @Test

    public void testConcatEmptyListsIncluded() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Concat.apply(java.util.List.of(
  (java.util.List<Integer>) (java.util.List.<Integer>of()),
  java.util.List.of(
    1,
    2),
  (java.util.List<Integer>) (java.util.List.<Integer>of()),
  java.util.List.of(3))));

    }

    @Test

    public void testConcatSingleList() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Concat.apply(java.util.List.of(java.util.List.of(
  1,
  2,
  3))));

    }

    @Test

    public void testConcatAllEmptyLists() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Concat.apply(java.util.List.of(
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()))));

    }

    @Test

    public void testConcatEmptyListOfLists() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Concat.apply((java.util.List<java.util.List<java.lang.Object>>) (java.util.List.<java.util.List<java.lang.Object>>of())));

    }

    // concat2

    @Test

    public void testConcat2TwoNonNegemptyLists() {

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

    public void testConcat2FirstListEmpty() {

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

    public void testConcat2SecondListEmpty() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.lists.Concat2.apply(
  java.util.List.of(
    1,
    2),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    @Test

    public void testConcat2BothListsEmpty() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Concat2.apply(
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testConcat2SingleElements() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.lists.Concat2.apply(
  java.util.List.of(1),
  java.util.List.of(2)));

    }

    @Test

    public void testConcat2StringLists() {

        assertEquals(

            java.util.List.of(
  "a",
  "b",
  "c",
  "d"),

            hydra.lib.lists.Concat2.apply(
  java.util.List.of(
    "a",
    "b"),
  java.util.List.of(
    "c",
    "d")));

    }

    // cons

    @Test

    public void testConsConsToNonNegemptyList() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Cons.apply(
  1,
  java.util.List.of(
    2,
    3)));

    }

    @Test

    public void testConsConsToEmptyList() {

        assertEquals(

            java.util.List.of(1),

            hydra.lib.lists.Cons.apply(
  1,
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    @Test

    public void testConsConsNegativeNumber() {

        assertEquals(

            java.util.List.of(
  -1,
  2,
  3),

            hydra.lib.lists.Cons.apply(
  -1,
  java.util.List.of(
    2,
    3)));

    }

    @Test

    public void testConsConsString() {

        assertEquals(

            java.util.List.of(
  "hello",
  "world"),

            hydra.lib.lists.Cons.apply(
  "hello",
  java.util.List.of("world")));

    }

    // drop

    @Test

    public void testDropDropFromBeginning() {

        assertEquals(

            java.util.List.of(
  3,
  4,
  5),

            hydra.lib.lists.Drop.apply(
  2,
  java.util.List.of(
    1,
    2,
    3,
    4,
    5)));

    }

    @Test

    public void testDropDropZeroElements() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Drop.apply(
  0,
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testDropDropAllElements() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Drop.apply(
  3,
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testDropDropMoreThanLength() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Drop.apply(
  5,
  java.util.List.of(
    1,
    2)));

    }

    @Test

    public void testDropDropFromEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Drop.apply(
  3,
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testDropDropNegativeAmount() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Drop.apply(
  -1,
  java.util.List.of(
    1,
    2,
    3)));

    }

    // dropWhile

    @Test

    public void testDropwhileDropWhileLessThan3() {

        assertEquals(

            java.util.List.of(
  3,
  2,
  1),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    3)),
  java.util.List.of(
    1,
    2,
    3,
    2,
    1)));

    }

    @Test

    public void testDropwhileDropAllElements() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testDropwhileDropNoElements() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    0)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testDropwhileEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.DropWhile.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    5)),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    // elem

    @Test

    public void testElemElementPresent() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  2,
  java.util.List.of(
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
  java.util.List.of(
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
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    @Test

    public void testElemSingleElementPresent() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  1,
  java.util.List.of(1)));

    }

    @Test

    public void testElemSingleElementNotPresent() {

        assertEquals(

            false,

            hydra.lib.lists.Elem.apply(
  2,
  java.util.List.of(1)));

    }

    @Test

    public void testElemDuplicateElements() {

        assertEquals(

            true,

            hydra.lib.lists.Elem.apply(
  2,
  java.util.List.of(
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
  java.util.List.of(
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
  java.util.List.of(
    "world",
    "hello")));

    }

    // filter

    @Test

    public void testFilterFilterPositiveNumbers() {

        assertEquals(

            java.util.List.of(
  2,
  4,
  5),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  java.util.List.of(
    -1,
    2,
    -3,
    4,
    5)));

    }

    @Test

    public void testFilterFilterAllElements() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testFilterFilterNoElements() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testFilterEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Filter.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

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
  java.util.List.of(
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
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testFindFindNoMatch() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testFindFindInEmptyList() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    0)),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    @Test

    public void testFindFindSingleElement() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.lib.lists.Find.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Equal.apply(
    x,
    42)),
  java.util.List.of(42)));

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
  java.util.List.of(
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
  java.util.List.of(
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
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

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
  java.util.List.of(5)));

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
  java.util.List.of(
    1,
    2,
    3)));

    }

    // group

    @Test

    public void testGroupConsecutiveDuplicates() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(
    1,
    1),
  java.util.List.of(
    2,
    2,
    2),
  java.util.List.of(3),
  java.util.List.of(1)),

            hydra.lib.lists.Group.apply(java.util.List.of(
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

            java.util.List.of(
  java.util.List.of(1),
  java.util.List.of(2),
  java.util.List.of(3)),

            hydra.lib.lists.Group.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testGroupAllSame() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  1,
  1)),

            hydra.lib.lists.Group.apply(java.util.List.of(
  1,
  1,
  1)));

    }

    @Test

    public void testGroupEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Group.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testGroupSingleElement() {

        assertEquals(

            java.util.List.of(java.util.List.of(1)),

            hydra.lib.lists.Group.apply(java.util.List.of(1)));

    }

    // head

    @Test

    public void testHeadThreeElementList() {

        assertEquals(

            1,

            hydra.lib.lists.Head.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testHeadSingleElementList() {

        assertEquals(

            42,

            hydra.lib.lists.Head.apply(java.util.List.of(42)));

    }

    @Test

    public void testHeadNegativeNumbers() {

        assertEquals(

            -1,

            hydra.lib.lists.Head.apply(java.util.List.of(
  -1,
  -2,
  -3)));

    }

    @Test

    public void testHeadStringList() {

        assertEquals(

            "hello",

            hydra.lib.lists.Head.apply(java.util.List.of(
  "hello",
  "world")));

    }

    // init

    @Test

    public void testInitMultipleElements() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Init.apply(java.util.List.of(
  1,
  2,
  3,
  4)));

    }

    @Test

    public void testInitTwoElements() {

        assertEquals(

            java.util.List.of(1),

            hydra.lib.lists.Init.apply(java.util.List.of(
  1,
  2)));

    }

    @Test

    public void testInitSingleElement() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Init.apply(java.util.List.of(1)));

    }

    @Test

    public void testInitStringList() {

        assertEquals(

            java.util.List.of(
  "a",
  "b"),

            hydra.lib.lists.Init.apply(java.util.List.of(
  "a",
  "b",
  "c")));

    }

    // intercalate

    @Test

    public void testIntercalateDoubleZeroSeparator() {

        assertEquals(

            java.util.List.of(
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
  java.util.List.of(
    0,
    0),
  java.util.List.of(
    java.util.List.of(
      1,
      2,
      3),
    java.util.List.of(
      4,
      5),
    java.util.List.of(
      6,
      7,
      8))));

    }

    @Test

    public void testIntercalateEmptySeparator() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3,
  4),

            hydra.lib.lists.Intercalate.apply(
  (java.util.List<Integer>) (java.util.List.<Integer>of()),
  java.util.List.of(
    java.util.List.of(
      1,
      2),
    java.util.List.of(
      3,
      4))));

    }

    @Test

    public void testIntercalateSingleElementSeparator() {

        assertEquals(

            java.util.List.of(
  1,
  99,
  2,
  99,
  3),

            hydra.lib.lists.Intercalate.apply(
  java.util.List.of(99),
  java.util.List.of(
    java.util.List.of(1),
    java.util.List.of(2),
    java.util.List.of(3))));

    }

    @Test

    public void testIntercalateEmptyListOfLists() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Intercalate.apply(
  java.util.List.of(0),
  (java.util.List<java.util.List<Integer>>) (java.util.List.<java.util.List<Integer>>of())));

    }

    @Test

    public void testIntercalateSingleList() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Intercalate.apply(
  java.util.List.of(0),
  java.util.List.of(java.util.List.of(
    1,
    2,
    3))));

    }

    @Test

    public void testIntercalateListsWithEmptyLists() {

        assertEquals(

            java.util.List.of(
  0,
  1,
  0),

            hydra.lib.lists.Intercalate.apply(
  java.util.List.of(0),
  java.util.List.of(
    (java.util.List<Integer>) (java.util.List.<Integer>of()),
    java.util.List.of(1),
    (java.util.List<Integer>) (java.util.List.<Integer>of()))));

    }

    // intersperse

    @Test

    public void testIntersperseStringInterspersion() {

        assertEquals(

            java.util.List.of(
  "one",
  "and",
  "two",
  "and",
  "three"),

            hydra.lib.lists.Intersperse.apply(
  "and",
  java.util.List.of(
    "one",
    "two",
    "three")));

    }

    @Test

    public void testIntersperseSingleElement() {

        assertEquals(

            java.util.List.of("only"),

            hydra.lib.lists.Intersperse.apply(
  "x",
  java.util.List.of("only")));

    }

    @Test

    public void testIntersperseEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Intersperse.apply(
  "x",
  (java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testIntersperseTwoElements() {

        assertEquals(

            java.util.List.of(
  "a",
  "+",
  "b"),

            hydra.lib.lists.Intersperse.apply(
  "+",
  java.util.List.of(
    "a",
    "b")));

    }

    @Test

    public void testIntersperseNumberInterspersion() {

        assertEquals(

            java.util.List.of(
  1,
  0,
  2,
  0,
  3),

            hydra.lib.lists.Intersperse.apply(
  0,
  java.util.List.of(
    1,
    2,
    3)));

    }

    // last

    @Test

    public void testLastThreeElementList() {

        assertEquals(

            3,

            hydra.lib.lists.Last.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testLastSingleElementList() {

        assertEquals(

            42,

            hydra.lib.lists.Last.apply(java.util.List.of(42)));

    }

    @Test

    public void testLastNegativeNumbers() {

        assertEquals(

            -3,

            hydra.lib.lists.Last.apply(java.util.List.of(
  -1,
  -2,
  -3)));

    }

    @Test

    public void testLastStringList() {

        assertEquals(

            "world",

            hydra.lib.lists.Last.apply(java.util.List.of(
  "hello",
  "world")));

    }

    // length

    @Test

    public void testLengthThreeElements() {

        assertEquals(

            3,

            hydra.lib.lists.Length.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testLengthEmptyList() {

        assertEquals(

            0,

            hydra.lib.lists.Length.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testLengthSingleElement() {

        assertEquals(

            1,

            hydra.lib.lists.Length.apply(java.util.List.of(42)));

    }

    @Test

    public void testLengthManyElements() {

        assertEquals(

            10,

            hydra.lib.lists.Length.apply(java.util.List.of(
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

            hydra.lib.lists.Length.apply(java.util.List.of(
  "a",
  "b",
  "c")));

    }

    // map

    @Test

    public void testMapStringToUppercase() {

        assertEquals(

            java.util.List.of(
  "ONE",
  "TWO"),

            hydra.lib.lists.Map.apply(
  hydra.lib.strings.ToUpper::apply,
  java.util.List.of(
    "one",
    "two")));

    }

    @Test

    public void testMapEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Map.apply(
  hydra.lib.strings.ToUpper::apply,
  (java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testMapSingleElement() {

        assertEquals(

            java.util.List.of("HELLO"),

            hydra.lib.lists.Map.apply(
  hydra.lib.strings.ToUpper::apply,
  java.util.List.of("hello")));

    }

    @Test

    public void testMapNumberNegation() {

        assertEquals(

            java.util.List.of(
  -1,
  -2,
  -3),

            hydra.lib.lists.Map.apply(
  hydra.lib.math.Negate::apply,
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testMapIdentityFunction() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Map.apply(
  (java.util.function.Function<Integer, Integer>) (hydra.lib.equality.Identity::apply),
  java.util.List.of(
    1,
    2,
    3)));

    }

    // nub

    @Test

    public void testNubRemoveDuplicates() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3,
  4),

            hydra.lib.lists.Nub.apply(java.util.List.of(
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

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Nub.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testNubAllDuplicates() {

        assertEquals(

            java.util.List.of(1),

            hydra.lib.lists.Nub.apply(java.util.List.of(
  1,
  1,
  1)));

    }

    @Test

    public void testNubEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Nub.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testNubSingleElement() {

        assertEquals(

            java.util.List.of(1),

            hydra.lib.lists.Nub.apply(java.util.List.of(1)));

    }

    @Test

    public void testNubStringDuplicates() {

        assertEquals(

            java.util.List.of(
  "a",
  "b",
  "c"),

            hydra.lib.lists.Nub.apply(java.util.List.of(
  "a",
  "b",
  "a",
  "c")));

    }

    // null

    @Test

    public void testNullEmptyIntList() {

        assertEquals(

            true,

            hydra.lib.lists.Null.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testNullSingleElement() {

        assertEquals(

            false,

            hydra.lib.lists.Null.apply(java.util.List.of(1)));

    }

    @Test

    public void testNullMultipleElements() {

        assertEquals(

            false,

            hydra.lib.lists.Null.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testNullEmptyStringList() {

        assertEquals(

            true,

            hydra.lib.lists.Null.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testNullNonNegemptyStringList() {

        assertEquals(

            false,

            hydra.lib.lists.Null.apply(java.util.List.of("a")));

    }

    // partition

    @Test

    public void testPartitionPartitionGreaterThan3() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>(java.util.List.of(
  4,
  5,
  6), java.util.List.of(
  1,
  2,
  3)))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    3)),
  java.util.List.of(
    1,
    2,
    3,
    4,
    5,
    6)));

    }

    @Test

    public void testPartitionPartitionAllElements() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>(java.util.List.of(
  1,
  2,
  3), (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testPartitionPartitionNoElements() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<Integer>>((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()), java.util.List.of(
  1,
  2,
  3)))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testPartitionPartitionEvenNumbers() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>(java.util.List.of(
  2,
  4,
  6), java.util.List.of(
  1,
  3,
  5)))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.math.Even.apply(x)),
  java.util.List.of(
    1,
    2,
    3,
    4,
    5,
    6)));

    }

    @Test

    public void testPartitionEmptyList() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>) ((hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>) (new hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()), (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.lists.Partition.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    5)),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    // pure

    @Test

    public void testPureStringElement() {

        assertEquals(

            java.util.List.of("one"),

            hydra.lib.lists.Pure.apply("one"));

    }

    @Test

    public void testPureEmptyString() {

        assertEquals(

            java.util.List.of(""),

            hydra.lib.lists.Pure.apply(""));

    }

    @Test

    public void testPureNumberElement() {

        assertEquals(

            java.util.List.of(42),

            hydra.lib.lists.Pure.apply(42));

    }

    @Test

    public void testPureNegativeNumber() {

        assertEquals(

            java.util.List.of(-5),

            hydra.lib.lists.Pure.apply(-5));

    }

    // replicate

    @Test

    public void testReplicateReplicateThreeTimes() {

        assertEquals(

            java.util.List.of(
  42,
  42,
  42),

            hydra.lib.lists.Replicate.apply(
  3,
  42));

    }

    @Test

    public void testReplicateReplicateZeroTimes() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Replicate.apply(
  0,
  1));

    }

    @Test

    public void testReplicateReplicateOnce() {

        assertEquals(

            java.util.List.of(99),

            hydra.lib.lists.Replicate.apply(
  1,
  99));

    }

    @Test

    public void testReplicateReplicateString() {

        assertEquals(

            java.util.List.of(
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

            java.util.List.of(
  4,
  3,
  2,
  1),

            hydra.lib.lists.Reverse.apply(java.util.List.of(
  1,
  2,
  3,
  4)));

    }

    @Test

    public void testReverseSingleElement() {

        assertEquals(

            java.util.List.of(1),

            hydra.lib.lists.Reverse.apply(java.util.List.of(1)));

    }

    @Test

    public void testReverseEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Reverse.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testReverseTwoElements() {

        assertEquals(

            java.util.List.of(
  2,
  1),

            hydra.lib.lists.Reverse.apply(java.util.List.of(
  1,
  2)));

    }

    @Test

    public void testReverseStringList() {

        assertEquals(

            java.util.List.of(
  "c",
  "b",
  "a"),

            hydra.lib.lists.Reverse.apply(java.util.List.of(
  "a",
  "b",
  "c")));

    }

    // safeHead

    @Test

    public void testSafeheadNonNegemptyIntList() {

        assertEquals(

            hydra.util.Maybe.just(1),

            hydra.lib.lists.SafeHead.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testSafeheadEmptyIntList() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.lists.SafeHead.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testSafeheadSingleElement() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.lib.lists.SafeHead.apply(java.util.List.of(42)));

    }

    @Test

    public void testSafeheadNonNegemptyStringList() {

        assertEquals(

            hydra.util.Maybe.just("hello"),

            hydra.lib.lists.SafeHead.apply(java.util.List.of(
  "hello",
  "world")));

    }

    @Test

    public void testSafeheadEmptyStringList() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.lists.SafeHead.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    // singleton

    @Test

    public void testSingletonNumberElement() {

        assertEquals(

            java.util.List.of(42),

            hydra.lib.lists.Singleton.apply(42));

    }

    @Test

    public void testSingletonNegativeNumber() {

        assertEquals(

            java.util.List.of(-1),

            hydra.lib.lists.Singleton.apply(-1));

    }

    @Test

    public void testSingletonZero() {

        assertEquals(

            java.util.List.of(0),

            hydra.lib.lists.Singleton.apply(0));

    }

    @Test

    public void testSingletonStringElement() {

        assertEquals(

            java.util.List.of("hello"),

            hydra.lib.lists.Singleton.apply("hello"));

    }

    // sort

    @Test

    public void testSortUnsortedNumbers() {

        assertEquals(

            java.util.List.of(
  1,
  1,
  3,
  4,
  5),

            hydra.lib.lists.Sort.apply(java.util.List.of(
  3,
  1,
  4,
  1,
  5)));

    }

    @Test

    public void testSortAlreadySorted() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Sort.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testSortReverseSorted() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Sort.apply(java.util.List.of(
  3,
  2,
  1)));

    }

    @Test

    public void testSortSingleElement() {

        assertEquals(

            java.util.List.of(1),

            hydra.lib.lists.Sort.apply(java.util.List.of(1)));

    }

    @Test

    public void testSortEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Sort.apply((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testSortDuplicates() {

        assertEquals(

            java.util.List.of(
  1,
  1,
  2,
  2,
  3),

            hydra.lib.lists.Sort.apply(java.util.List.of(
  2,
  1,
  2,
  3,
  1)));

    }

    @Test

    public void testSortStringSort() {

        assertEquals(

            java.util.List.of(
  "apple",
  "banana",
  "zebra"),

            hydra.lib.lists.Sort.apply(java.util.List.of(
  "zebra",
  "apple",
  "banana")));

    }

    // sortOn

    @Test

    public void testSortonSortByStringLength() {

        assertEquals(

            java.util.List.of(
  "hi",
  "hello",
  "world"),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.strings.Length::apply,
  java.util.List.of(
    "hello",
    "hi",
    "world")));

    }

    @Test

    public void testSortonEmptyStringList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.strings.Length::apply,
  (java.util.List<String>) (java.util.List.<String>of())));

    }

    @Test

    public void testSortonSingleStringElement() {

        assertEquals(

            java.util.List.of("test"),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.strings.Length::apply,
  java.util.List.of("test")));

    }

    @Test

    public void testSortonSortByNegation() {

        assertEquals(

            java.util.List.of(
  3,
  2,
  1),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.math.Negate::apply,
  java.util.List.of(
    1,
    3,
    2)));

    }

    @Test

    public void testSortonSortByAbsoluteValue() {

        assertEquals(

            java.util.List.of(
  -1,
  2,
  -3),

            hydra.lib.lists.SortOn.apply(
  hydra.lib.math.Abs::apply,
  java.util.List.of(
    -1,
    -3,
    2)));

    }

    // span

    @Test

    public void testSpanSpanLessThan3() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<Integer>>(java.util.List.of(
  1,
  2), java.util.List.of(
  3,
  1,
  2)))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    3)),
  java.util.List.of(
    1,
    2,
    3,
    1,
    2)));

    }

    @Test

    public void testSpanSpanAllElements() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>(java.util.List.of(
  1,
  2,
  3), (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testSpanSpanNoElements() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<Integer>>) ((hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<Integer>>) (new hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<Integer>>((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()), java.util.List.of(
  1,
  2,
  3)))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Gt.apply(
    x,
    10)),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testSpanEmptyList() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>) ((hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>) (new hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()), (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.lists.Span.apply(
  (java.util.function.Function<Integer, Boolean>) (x -> hydra.lib.equality.Lt.apply(
    x,
    5)),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    // tail

    @Test

    public void testTailMultipleElements() {

        assertEquals(

            java.util.List.of(
  2,
  3,
  4),

            hydra.lib.lists.Tail.apply(java.util.List.of(
  1,
  2,
  3,
  4)));

    }

    @Test

    public void testTailTwoElements() {

        assertEquals(

            java.util.List.of(2),

            hydra.lib.lists.Tail.apply(java.util.List.of(
  1,
  2)));

    }

    @Test

    public void testTailSingleElement() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Tail.apply(java.util.List.of(1)));

    }

    @Test

    public void testTailStringList() {

        assertEquals(

            java.util.List.of(
  "b",
  "c"),

            hydra.lib.lists.Tail.apply(java.util.List.of(
  "a",
  "b",
  "c")));

    }

    // take

    @Test

    public void testTakeTakeFromBeginning() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.lists.Take.apply(
  2,
  java.util.List.of(
    1,
    2,
    3,
    4,
    5)));

    }

    @Test

    public void testTakeTakeZeroElements() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Take.apply(
  0,
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testTakeTakeAllElements() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.lists.Take.apply(
  3,
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testTakeTakeMoreThanLength() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.lists.Take.apply(
  5,
  java.util.List.of(
    1,
    2)));

    }

    @Test

    public void testTakeTakeFromEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Take.apply(
  3,
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testTakeTakeNegativeAmount() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Take.apply(
  -1,
  java.util.List.of(
    1,
    2,
    3)));

    }

    // transpose

    @Test

    public void testTransposeSquareMatrix() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(
    1,
    4),
  java.util.List.of(
    2,
    5),
  java.util.List.of(
    3,
    6)),

            hydra.lib.lists.Transpose.apply(java.util.List.of(
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    4,
    5,
    6))));

    }

    @Test

    public void testTransposeEmptyLists() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Transpose.apply((java.util.List<java.util.List<java.lang.Object>>) (java.util.List.<java.util.List<java.lang.Object>>of())));

    }

    @Test

    public void testTransposeSingleRow() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(1),
  java.util.List.of(2),
  java.util.List.of(3)),

            hydra.lib.lists.Transpose.apply(java.util.List.of(java.util.List.of(
  1,
  2,
  3))));

    }

    @Test

    public void testTransposeSingleColumn() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  2,
  3)),

            hydra.lib.lists.Transpose.apply(java.util.List.of(
  java.util.List.of(1),
  java.util.List.of(2),
  java.util.List.of(3))));

    }

    @Test

    public void testTransposeRaggedMatrix() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(
    1,
    3,
    4),
  java.util.List.of(
    2,
    5),
  java.util.List.of(6)),

            hydra.lib.lists.Transpose.apply(java.util.List.of(
  java.util.List.of(
    1,
    2),
  java.util.List.of(3),
  java.util.List.of(
    4,
    5,
    6))));

    }

    // zip

    @Test

    public void testZipEqualLengthLists() {

        assertEquals(

            java.util.List.of(
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(2, "b"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(3, "c")))),

            hydra.lib.lists.Zip.apply(
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    "a",
    "b",
    "c")));

    }

    @Test

    public void testZipFirstListShorter() {

        assertEquals(

            java.util.List.of(
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(2, "b")))),

            hydra.lib.lists.Zip.apply(
  java.util.List.of(
    1,
    2),
  java.util.List.of(
    "a",
    "b",
    "c")));

    }

    @Test

    public void testZipSecondListShorter() {

        assertEquals(

            java.util.List.of(
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(2, "b")))),

            hydra.lib.lists.Zip.apply(
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    "a",
    "b")));

    }

    @Test

    public void testZipEmptyFirstList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Zip.apply(
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),
  java.util.List.of(
    "a",
    "b")));

    }

    @Test

    public void testZipEmptySecondList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Zip.apply(
  java.util.List.of(
    1,
    2),
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    @Test

    public void testZipBothEmptyLists() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.Zip.apply(
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),
  (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())));

    }

    // zipWith

    @Test

    public void testZipwithAddition() {

        assertEquals(

            java.util.List.of(
  5,
  7,
  9),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    4,
    5,
    6)));

    }

    @Test

    public void testZipwithFirstListShorter() {

        assertEquals(

            java.util.List.of(
  5,
  7),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  java.util.List.of(
    1,
    2),
  java.util.List.of(
    4,
    5,
    6)));

    }

    @Test

    public void testZipwithSecondListShorter() {

        assertEquals(

            java.util.List.of(
  5,
  7),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    4,
    5)));

    }

    @Test

    public void testZipwithEmptyFirstList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  (java.util.List<Integer>) (java.util.List.<Integer>of()),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testZipwithEmptySecondList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<Integer, java.util.function.Function<Integer, Integer>>) (p0 -> p1 -> hydra.lib.math.Add.apply(
    p0,
    p1)),
  java.util.List.of(
    1,
    2,
    3),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    @Test

    public void testZipwithStringConcatenation() {

        assertEquals(

            java.util.List.of(
  "a1",
  "b2"),

            hydra.lib.lists.ZipWith.apply(
  (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.lib.strings.Cat2.apply(
    p0,
    p1)),
  java.util.List.of(
    "a",
    "b"),
  java.util.List.of(
    "1",
    "2")));

    }
}
