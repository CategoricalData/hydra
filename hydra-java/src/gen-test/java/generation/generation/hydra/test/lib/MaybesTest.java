// Note: this is an automatically generated file. Do not edit.
// hydra.lib.maybes primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class MaybesTest {

    // apply

    @Test

    public void testApplyBothJust() {

        assertEquals(

            hydra.util.Maybe.just(8),

            hydra.lib.maybes.Apply.apply(
  hydra.util.Maybe.just(hydra.lib.math.Add.apply(3)),
  hydra.util.Maybe.just(5)));

    }

    @Test

    public void testApplyNothingFunction() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maybes.Apply.apply(
  (hydra.util.Maybe<java.util.function.Function<Integer, java.lang.Object>>) (hydra.util.Maybe.<java.util.function.Function<Integer, java.lang.Object>>nothing()),
  hydra.util.Maybe.just(5)));

    }

    @Test

    public void testApplyNothingValue() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maybes.Apply.apply(
  hydra.util.Maybe.just(hydra.lib.math.Add.apply(3)),
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())));

    }

    // bind

    @Test

    public void testBindJustToJust() {

        assertEquals(

            hydra.util.Maybe.just(10),

            hydra.lib.maybes.Bind.apply(
  hydra.util.Maybe.just(5),
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
    x,
    2)))));

    }

    @Test

    public void testBindNothingToNothing() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maybes.Bind.apply(
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
    x,
    2)))));

    }

    // cases

    @Test

    public void testCasesJustAppliesFunction() {

        assertEquals(

            10,

            hydra.lib.maybes.Cases.apply(
  hydra.util.Maybe.just(5),
  0,
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2))));

    }

    @Test

    public void testCasesNothingReturnsDefault() {

        assertEquals(

            99,

            hydra.lib.maybes.Cases.apply(
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
  99,
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2))));

    }

    // cat

    @Test

    public void testCatFiltersNothings() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.maybes.Cat.apply(java.util.List.of(
  hydra.util.Maybe.just(1),
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
  hydra.util.Maybe.just(2))));

    }

    @Test

    public void testCatAllJusts() {

        assertEquals(

            java.util.List.of(
  1,
  2),

            hydra.lib.maybes.Cat.apply(java.util.List.of(
  hydra.util.Maybe.just(1),
  hydra.util.Maybe.just(2))));

    }

    @Test

    public void testCatAllNothings() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maybes.Cat.apply(java.util.List.of(
  (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),
  (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()))));

    }

    @Test

    public void testCatEmptyList() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maybes.Cat.apply((java.util.List<hydra.util.Maybe<java.lang.Object>>) (java.util.List.<hydra.util.Maybe<java.lang.Object>>of())));

    }

    // compose

    @Test

    public void testComposeBothSucceed() {

        assertEquals(

            hydra.util.Maybe.just(12),

            hydra.lib.maybes.Compose.apply(
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Lte.apply(
      x,
      5),
    () -> hydra.util.Maybe.just(hydra.lib.math.Add.apply(
      x,
      1)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (y -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Gte.apply(
      y,
      5),
    () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
      y,
      2)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  5));

    }

    @Test

    public void testComposeFirstFails() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maybes.Compose.apply(
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Lte.apply(
      x,
      5),
    () -> hydra.util.Maybe.just(hydra.lib.math.Add.apply(
      x,
      1)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (y -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Gte.apply(
      y,
      5),
    () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
      y,
      2)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  10));

    }

    @Test

    public void testComposeSecondFails() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maybes.Compose.apply(
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Lte.apply(
      x,
      5),
    () -> hydra.util.Maybe.just(hydra.lib.math.Add.apply(
      x,
      1)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (y -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Gte.apply(
      y,
      5),
    () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
      y,
      2)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  3));

    }

    // fromJust

    @Test

    public void testFromjustExtractFromJust() {

        assertEquals(

            42,

            hydra.lib.maybes.FromJust.apply(hydra.util.Maybe.just(42)));

    }

    // fromMaybe

    @Test

    public void testFrommaybeJustValue() {

        assertEquals(

            42,

            hydra.lib.maybes.FromMaybe.apply(
  0,
  hydra.util.Maybe.just(42)));

    }

    @Test

    public void testFrommaybeNothingWithDefault() {

        assertEquals(

            99,

            hydra.lib.maybes.FromMaybe.apply(
  99,
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())));

    }

    // isJust

    @Test

    public void testIsjustJustValue() {

        assertEquals(

            true,

            hydra.lib.maybes.IsJust.apply(hydra.util.Maybe.just(42)));

    }

    @Test

    public void testIsjustNothing() {

        assertEquals(

            false,

            hydra.lib.maybes.IsJust.apply((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())));

    }

    // isNothing

    @Test

    public void testIsnothingJustValue() {

        assertEquals(

            false,

            hydra.lib.maybes.IsNothing.apply(hydra.util.Maybe.just(42)));

    }

    @Test

    public void testIsnothingNothing() {

        assertEquals(

            true,

            hydra.lib.maybes.IsNothing.apply((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())));

    }

    // map

    @Test

    public void testMapMapsJustValue() {

        assertEquals(

            hydra.util.Maybe.just(10),

            hydra.lib.maybes.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  hydra.util.Maybe.just(5)));

    }

    @Test

    public void testMapNothingUnchanged() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maybes.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())));

    }

    // mapMaybe

    @Test

    public void testMapmaybeFilterAndTransform() {

        assertEquals(

            java.util.List.of(
  6,
  8,
  10),

            hydra.lib.maybes.MapMaybe.apply(
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Gt.apply(
      x,
      2),
    () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
      x,
      2)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  java.util.List.of(
    1,
    2,
    3,
    4,
    5)));

    }

    @Test

    public void testMapmaybeEmptyResult() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maybes.MapMaybe.apply(
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Gt.apply(
      x,
      2),
    () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
      x,
      2)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  java.util.List.of(
    1,
    2)));

    }

    @Test

    public void testMapmaybeEmptyInput() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maybes.MapMaybe.apply(
  (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
    hydra.lib.equality.Gt.apply(
      x,
      2),
    () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
      x,
      2)),
    () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
  (java.util.List<Integer>) (java.util.List.<Integer>of())));

    }

    // maybe

    @Test

    public void testMaybeJustValueAppliesFunction() {

        assertEquals(

            10,

            hydra.lib.maybes.Maybe.apply(
  0,
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  hydra.util.Maybe.just(5)));

    }

    @Test

    public void testMaybeNothingReturnsDefault() {

        assertEquals(

            99,

            hydra.lib.maybes.Maybe.apply(
  99,
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())));

    }

    // pure

    @Test

    public void testPureWrapsInteger() {

        assertEquals(

            hydra.util.Maybe.just(42),

            hydra.lib.maybes.Pure.apply(42));

    }

    @Test

    public void testPureWrapsString() {

        assertEquals(

            hydra.util.Maybe.just("hello"),

            hydra.lib.maybes.Pure.apply("hello"));

    }
}
