// Note: this is an automatically generated file. Do not edit.
// hydra.lib.eithers primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class EithersTest {

    // bind

    @Test

    public void testBindBindRightWithSuccess() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, Integer>) ((hydra.util.Either<java.lang.Object, Integer>) (hydra.util.Either.<java.lang.Object, Integer>right(2))),

            hydra.lib.eithers.Bind.apply(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("ab"))),
  (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.strings.Null.apply(s),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(0))),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s))))))));

    }

    @Test

    public void testBindBindRightWithFailure() {

        assertEquals(

            (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(0))),

            hydra.lib.eithers.Bind.apply(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right(""))),
  (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.strings.Null.apply(s),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(0))),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s))))))));

    }

    @Test

    public void testBindBindLeftReturnsLeftUnchanged() {

        assertEquals(

            (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(42))),

            hydra.lib.eithers.Bind.apply(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(42))),
  (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.strings.Null.apply(s),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(0))),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s))))))));

    }

    // bimap

    @Test

    public void testBimapMapLeftValue() {

        assertEquals(

            (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(10))),

            hydra.lib.eithers.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(5)))));

    }

    @Test

    public void testBimapMapRightValue() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, Integer>) ((hydra.util.Either<java.lang.Object, Integer>) (hydra.util.Either.<java.lang.Object, Integer>right(2))),

            hydra.lib.eithers.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("ab")))));

    }

    // isLeft

    @Test

    public void testIsleftLeftValue() {

        assertEquals(

            true,

            hydra.lib.eithers.IsLeft.apply((hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(42)))));

    }

    @Test

    public void testIsleftRightValue() {

        assertEquals(

            false,

            hydra.lib.eithers.IsLeft.apply((hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("test")))));

    }

    // isRight

    @Test

    public void testIsrightRightValue() {

        assertEquals(

            true,

            hydra.lib.eithers.IsRight.apply((hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("test")))));

    }

    @Test

    public void testIsrightLeftValue() {

        assertEquals(

            false,

            hydra.lib.eithers.IsRight.apply((hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(42)))));

    }

    // fromLeft

    @Test

    public void testFromleftExtractLeft() {

        assertEquals(

            42,

            hydra.lib.eithers.FromLeft.apply(
  99,
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(42)))));

    }

    @Test

    public void testFromleftUseDefaultForRight() {

        assertEquals(

            99,

            hydra.lib.eithers.FromLeft.apply(
  99,
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("test")))));

    }

    // fromRight

    @Test

    public void testFromrightExtractRight() {

        assertEquals(

            "test",

            hydra.lib.eithers.FromRight.apply(
  "default",
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("test")))));

    }

    @Test

    public void testFromrightUseDefaultForLeft() {

        assertEquals(

            "default",

            hydra.lib.eithers.FromRight.apply(
  "default",
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(42)))));

    }

    // either

    @Test

    public void testEitherApplyLeftFunction() {

        assertEquals(

            10,

            hydra.lib.eithers.Either.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(5)))));

    }

    @Test

    public void testEitherApplyRightFunction() {

        assertEquals(

            2,

            hydra.lib.eithers.Either.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("ab")))));

    }

    // lefts

    @Test

    public void testLeftsFilterLeftValues() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.eithers.Lefts.apply(java.util.List.of(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(1))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("a"))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(2))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("b"))))));

    }

    @Test

    public void testLeftsAllLefts() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.eithers.Lefts.apply(java.util.List.of(
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(1))),
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(2))))));

    }

    @Test

    public void testLeftsAllRights() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.eithers.Lefts.apply(java.util.List.of(
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("a"))),
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("b"))))));

    }

    @Test

    public void testLeftsEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.eithers.Lefts.apply((java.util.List<hydra.util.Either<java.lang.Object, java.lang.Object>>) (java.util.List.<hydra.util.Either<java.lang.Object, java.lang.Object>>of())));

    }

    // rights

    @Test

    public void testRightsFilterRightValues() {

        assertEquals(

            java.util.List.of(
  "a",
  "b"),

            hydra.lib.eithers.Rights.apply(java.util.List.of(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(1))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("a"))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(2))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("b"))))));

    }

    @Test

    public void testRightsAllRights() {

        assertEquals(

            java.util.List.of(
  "a",
  "b"),

            hydra.lib.eithers.Rights.apply(java.util.List.of(
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("a"))),
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("b"))))));

    }

    @Test

    public void testRightsAllLefts() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.eithers.Rights.apply(java.util.List.of(
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(1))),
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(2))))));

    }

    @Test

    public void testRightsEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.eithers.Rights.apply((java.util.List<hydra.util.Either<java.lang.Object, java.lang.Object>>) (java.util.List.<hydra.util.Either<java.lang.Object, java.lang.Object>>of())));

    }

    // partitionEithers

    @Test

    public void testPartitioneithersPartitionMixed() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<String>>(java.util.List.of(
  1,
  2), java.util.List.of(
  "a",
  "b")))),

            hydra.lib.eithers.PartitionEithers.apply(java.util.List.of(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(1))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("a"))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(2))),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("b"))))));

    }

    @Test

    public void testPartitioneithersAllLefts() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>) ((hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>) (new hydra.util.Tuple.Tuple2<java.util.List<Integer>, java.util.List<java.lang.Object>>(java.util.List.of(
  1,
  2), (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.eithers.PartitionEithers.apply(java.util.List.of(
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(1))),
  (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(2))))));

    }

    @Test

    public void testPartitioneithersAllRights() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<String>>) ((hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<String>>) (new hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<String>>((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()), java.util.List.of(
  "a",
  "b")))),

            hydra.lib.eithers.PartitionEithers.apply(java.util.List.of(
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("a"))),
  (hydra.util.Either<java.lang.Object, String>) ((hydra.util.Either<java.lang.Object, String>) (hydra.util.Either.<java.lang.Object, String>right("b"))))));

    }

    @Test

    public void testPartitioneithersEmptyList() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>) ((hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>) (new hydra.util.Tuple.Tuple2<java.util.List<java.lang.Object>, java.util.List<java.lang.Object>>((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()), (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.eithers.PartitionEithers.apply((java.util.List<hydra.util.Either<java.lang.Object, java.lang.Object>>) (java.util.List.<hydra.util.Either<java.lang.Object, java.lang.Object>>of())));

    }

    // map

    @Test

    public void testMapMapRightValue() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, Integer>) ((hydra.util.Either<java.lang.Object, Integer>) (hydra.util.Either.<java.lang.Object, Integer>right(10))),

            hydra.lib.eithers.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.Either<java.lang.Object, Integer>) ((hydra.util.Either<java.lang.Object, Integer>) (hydra.util.Either.<java.lang.Object, Integer>right(5)))));

    }

    @Test

    public void testMapPreserveLeft() {

        assertEquals(

            (hydra.util.Either<Integer, java.lang.Object>) ((hydra.util.Either<Integer, java.lang.Object>) (hydra.util.Either.<Integer, java.lang.Object>left(99))),

            hydra.lib.eithers.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(99)))));

    }

    // mapList

    @Test

    public void testMaplistAllSucceed() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, java.util.List<Integer>>) ((hydra.util.Either<java.lang.Object, java.util.List<Integer>>) (hydra.util.Either.<java.lang.Object, java.util.List<Integer>>right(java.util.List.of(
  2,
  4,
  6)))),

            hydra.lib.eithers.MapList.apply(
  (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Equal.apply(
      x,
      0),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>left("zero"))),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
      x,
      2)))))),
  java.util.List.of(
    1,
    2,
    3)));

    }

    @Test

    public void testMaplistFirstFails() {

        assertEquals(

            (hydra.util.Either<String, java.lang.Object>) ((hydra.util.Either<String, java.lang.Object>) (hydra.util.Either.<String, java.lang.Object>left("zero"))),

            hydra.lib.eithers.MapList.apply(
  (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Equal.apply(
      x,
      0),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>left("zero"))),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
      x,
      2)))))),
  java.util.List.of(
    1,
    0,
    3)));

    }

    @Test

    public void testMaplistEmptyList() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, java.util.List<java.lang.Object>>) ((hydra.util.Either<java.lang.Object, java.util.List<java.lang.Object>>) (hydra.util.Either.<java.lang.Object, java.util.List<java.lang.Object>>right((java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of())))),

            hydra.lib.eithers.MapList.apply(
  (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Equal.apply(
      x,
      0),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>left("zero"))),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
      x,
      2)))))),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    // mapMaybe

    @Test

    public void testMapmaybeJustSucceeds() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, hydra.util.Maybe<Integer>>) ((hydra.util.Either<java.lang.Object, hydra.util.Maybe<Integer>>) (hydra.util.Either.<java.lang.Object, hydra.util.Maybe<Integer>>right(hydra.util.Maybe.just(10)))),

            hydra.lib.eithers.MapMaybe.apply(
  (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Equal.apply(
      x,
      0),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>left("zero"))),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
      x,
      2)))))),
  hydra.util.Maybe.just(5)));

    }

    @Test

    public void testMapmaybeJustFails() {

        assertEquals(

            (hydra.util.Either<String, java.lang.Object>) ((hydra.util.Either<String, java.lang.Object>) (hydra.util.Either.<String, java.lang.Object>left("zero"))),

            hydra.lib.eithers.MapMaybe.apply(
  (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Equal.apply(
      x,
      0),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>left("zero"))),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
      x,
      2)))))),
  hydra.util.Maybe.just(0)));

    }

    @Test

    public void testMapmaybeNothing() {

        assertEquals(

            (hydra.util.Either<java.lang.Object, hydra.util.Maybe<java.lang.Object>>) ((hydra.util.Either<java.lang.Object, hydra.util.Maybe<java.lang.Object>>) (hydra.util.Either.<java.lang.Object, hydra.util.Maybe<java.lang.Object>>right((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())))),

            hydra.lib.eithers.MapMaybe.apply(
  (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Equal.apply(
      x,
      0),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>left("zero"))),
    () -> (hydra.util.Either<String, Integer>) ((hydra.util.Either<String, Integer>) (hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
      x,
      2)))))),
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())));

    }
}
