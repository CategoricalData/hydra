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

    public <T0> void testBindBindRightWithSuccess() {

        assertEquals(

            (hydra.util.Either<T0, Integer>) ((hydra.util.Either<T0, Integer>) (hydra.util.Either.<T0, Integer>right(2))),

            hydra.lib.eithers.Bind.apply(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("ab"))),
  (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.strings.Null.apply(s),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(0))),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s))))))));

    }

    @Test

    public <T0> void testBindBindRightWithFailure() {

        assertEquals(

            (hydra.util.Either<Integer, T0>) ((hydra.util.Either<Integer, T0>) (hydra.util.Either.<Integer, T0>left(0))),

            hydra.lib.eithers.Bind.apply(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right(""))),
  (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.strings.Null.apply(s),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(0))),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s))))))));

    }

    @Test

    public <T0> void testBindBindLeftReturnsLeftUnchanged() {

        assertEquals(

            (hydra.util.Either<Integer, T0>) ((hydra.util.Either<Integer, T0>) (hydra.util.Either.<Integer, T0>left(42))),

            hydra.lib.eithers.Bind.apply(
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(42))),
  (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.strings.Null.apply(s),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(0))),
    () -> (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s))))))));

    }

    // bimap

    @Test

    public <T0> void testBimapMapLeftValue() {

        assertEquals(

            (hydra.util.Either<Integer, T0>) ((hydra.util.Either<Integer, T0>) (hydra.util.Either.<Integer, T0>left(10))),

            hydra.lib.eithers.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>left(5)))));

    }

    @Test

    public <T0> void testBimapMapRightValue() {

        assertEquals(

            (hydra.util.Either<T0, Integer>) ((hydra.util.Either<T0, Integer>) (hydra.util.Either.<T0, Integer>right(2))),

            hydra.lib.eithers.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Either<Integer, String>) ((hydra.util.Either<Integer, String>) (hydra.util.Either.<Integer, String>right("ab")))));

    }

    // isLeft

    @Test

    public <T2> void testIsleftLeftValue() {

        assertEquals(

            true,

            hydra.lib.eithers.IsLeft.apply((hydra.util.Either<Integer, T2>) ((hydra.util.Either<Integer, T2>) (hydra.util.Either.<Integer, T2>left(42)))));

    }

    @Test

    public <T2> void testIsleftRightValue() {

        assertEquals(

            false,

            hydra.lib.eithers.IsLeft.apply((hydra.util.Either<T2, String>) ((hydra.util.Either<T2, String>) (hydra.util.Either.<T2, String>right("test")))));

    }

    // isRight

    @Test

    public <T2> void testIsrightRightValue() {

        assertEquals(

            true,

            hydra.lib.eithers.IsRight.apply((hydra.util.Either<T2, String>) ((hydra.util.Either<T2, String>) (hydra.util.Either.<T2, String>right("test")))));

    }

    @Test

    public <T2> void testIsrightLeftValue() {

        assertEquals(

            false,

            hydra.lib.eithers.IsRight.apply((hydra.util.Either<Integer, T2>) ((hydra.util.Either<Integer, T2>) (hydra.util.Either.<Integer, T2>left(42)))));

    }

    // fromLeft

    @Test

    public <T3> void testFromleftExtractLeft() {

        assertEquals(

            42,

            hydra.lib.eithers.FromLeft.apply(
  99,
  (hydra.util.Either<Integer, T3>) ((hydra.util.Either<Integer, T3>) (hydra.util.Either.<Integer, T3>left(42)))));

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

    public <T3> void testFromrightExtractRight() {

        assertEquals(

            "test",

            hydra.lib.eithers.FromRight.apply(
  "default",
  (hydra.util.Either<T3, String>) ((hydra.util.Either<T3, String>) (hydra.util.Either.<T3, String>right("test")))));

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

    public <T4> void testLeftsAllLefts() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.eithers.Lefts.apply(java.util.List.of(
  (hydra.util.Either<Integer, T4>) ((hydra.util.Either<Integer, T4>) (hydra.util.Either.<Integer, T4>left(1))),
  (hydra.util.Either<Integer, T4>) ((hydra.util.Either<Integer, T4>) (hydra.util.Either.<Integer, T4>left(2))))));

    }

    @Test

    public <T0, T4> void testLeftsAllRights() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.eithers.Lefts.apply(java.util.List.of(
  (hydra.util.Either<T4, String>) ((hydra.util.Either<T4, String>) (hydra.util.Either.<T4, String>right("a"))),
  (hydra.util.Either<T4, String>) ((hydra.util.Either<T4, String>) (hydra.util.Either.<T4, String>right("b"))))));

    }

    @Test

    public <T0, T1> void testLeftsEmptyList() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.eithers.Lefts.apply((java.util.List<hydra.util.Either<T0, T1>>) (java.util.List.<hydra.util.Either<T0, T1>>of())));

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

    public <T4> void testRightsAllRights() {

        assertEquals(

            java.util.List.of(
  "a",
  "b"),

            hydra.lib.eithers.Rights.apply(java.util.List.of(
  (hydra.util.Either<T4, String>) ((hydra.util.Either<T4, String>) (hydra.util.Either.<T4, String>right("a"))),
  (hydra.util.Either<T4, String>) ((hydra.util.Either<T4, String>) (hydra.util.Either.<T4, String>right("b"))))));

    }

    @Test

    public <T0, T4> void testRightsAllLefts() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.eithers.Rights.apply(java.util.List.of(
  (hydra.util.Either<Integer, T4>) ((hydra.util.Either<Integer, T4>) (hydra.util.Either.<Integer, T4>left(1))),
  (hydra.util.Either<Integer, T4>) ((hydra.util.Either<Integer, T4>) (hydra.util.Either.<Integer, T4>left(2))))));

    }

    @Test

    public <T0, T1> void testRightsEmptyList() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.eithers.Rights.apply((java.util.List<hydra.util.Either<T0, T1>>) (java.util.List.<hydra.util.Either<T0, T1>>of())));

    }

    // partitionEithers

    @Test

    public void testPartitioneithersPartitionMixed() {

        assertEquals(

            (hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) ((hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) (new hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>(java.util.List.of(
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

    public <T1, T4> void testPartitioneithersAllLefts() {

        assertEquals(

            (hydra.util.Pair<java.util.List<Integer>, java.util.List<T1>>) ((hydra.util.Pair<java.util.List<Integer>, java.util.List<T1>>) (new hydra.util.Pair<java.util.List<Integer>, java.util.List<T1>>(java.util.List.of(
  1,
  2), (java.util.List<T1>) (java.util.List.<T1>of())))),

            hydra.lib.eithers.PartitionEithers.apply(java.util.List.of(
  (hydra.util.Either<Integer, T4>) ((hydra.util.Either<Integer, T4>) (hydra.util.Either.<Integer, T4>left(1))),
  (hydra.util.Either<Integer, T4>) ((hydra.util.Either<Integer, T4>) (hydra.util.Either.<Integer, T4>left(2))))));

    }

    @Test

    public <T0, T4> void testPartitioneithersAllRights() {

        assertEquals(

            (hydra.util.Pair<java.util.List<T0>, java.util.List<String>>) ((hydra.util.Pair<java.util.List<T0>, java.util.List<String>>) (new hydra.util.Pair<java.util.List<T0>, java.util.List<String>>((java.util.List<T0>) (java.util.List.<T0>of()), java.util.List.of(
  "a",
  "b")))),

            hydra.lib.eithers.PartitionEithers.apply(java.util.List.of(
  (hydra.util.Either<T4, String>) ((hydra.util.Either<T4, String>) (hydra.util.Either.<T4, String>right("a"))),
  (hydra.util.Either<T4, String>) ((hydra.util.Either<T4, String>) (hydra.util.Either.<T4, String>right("b"))))));

    }

    @Test

    public <T0, T1> void testPartitioneithersEmptyList() {

        assertEquals(

            (hydra.util.Pair<java.util.List<T0>, java.util.List<T1>>) ((hydra.util.Pair<java.util.List<T0>, java.util.List<T1>>) (new hydra.util.Pair<java.util.List<T0>, java.util.List<T1>>((java.util.List<T0>) (java.util.List.<T0>of()), (java.util.List<T1>) (java.util.List.<T1>of())))),

            hydra.lib.eithers.PartitionEithers.apply((java.util.List<hydra.util.Either<T0, T1>>) (java.util.List.<hydra.util.Either<T0, T1>>of())));

    }

    // map

    @Test

    public <T0, T7> void testMapMapRightValue() {

        assertEquals(

            (hydra.util.Either<T0, Integer>) ((hydra.util.Either<T0, Integer>) (hydra.util.Either.<T0, Integer>right(10))),

            hydra.lib.eithers.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.Either<T7, Integer>) ((hydra.util.Either<T7, Integer>) (hydra.util.Either.<T7, Integer>right(5)))));

    }

    @Test

    public <T0> void testMapPreserveLeft() {

        assertEquals(

            (hydra.util.Either<Integer, T0>) ((hydra.util.Either<Integer, T0>) (hydra.util.Either.<Integer, T0>left(99))),

            hydra.lib.eithers.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.Either<Integer, Integer>) ((hydra.util.Either<Integer, Integer>) (hydra.util.Either.<Integer, Integer>left(99)))));

    }

    // mapList

    @Test

    public <T1> void testMaplistAllSucceed() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(
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

    public <T0> void testMaplistFirstFails() {

        assertEquals(

            (hydra.util.Either<String, T0>) ((hydra.util.Either<String, T0>) (hydra.util.Either.<String, T0>left("zero"))),

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

    public <T0, T1> void testMaplistEmptyList() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<T0>>) ((hydra.util.Either<T1, java.util.List<T0>>) (hydra.util.Either.<T1, java.util.List<T0>>right((java.util.List<T0>) (java.util.List.<T0>of())))),

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

    public <T1> void testMapmaybeJustSucceeds() {

        assertEquals(

            (hydra.util.Either<T1, hydra.util.Maybe<Integer>>) ((hydra.util.Either<T1, hydra.util.Maybe<Integer>>) (hydra.util.Either.<T1, hydra.util.Maybe<Integer>>right(hydra.util.Maybe.just(10)))),

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

    public <T0> void testMapmaybeJustFails() {

        assertEquals(

            (hydra.util.Either<String, T0>) ((hydra.util.Either<String, T0>) (hydra.util.Either.<String, T0>left("zero"))),

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

    public <T0, T1> void testMapmaybeNothing() {

        assertEquals(

            (hydra.util.Either<T1, hydra.util.Maybe<T0>>) ((hydra.util.Either<T1, hydra.util.Maybe<T0>>) (hydra.util.Either.<T1, hydra.util.Maybe<T0>>right((hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing())))),

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
