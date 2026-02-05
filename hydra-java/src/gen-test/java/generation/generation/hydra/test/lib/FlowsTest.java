// Note: this is an automatically generated file. Do not edit.
// hydra.lib.flows primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class FlowsTest {

    // apply

    @Test

    public void testApplyApplyAdd() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(8), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(hydra.lib.math.Add.apply(3)),
  (java.util.function.Function<java.util.function.Function<Integer, Integer>, hydra.compute.Flow<java.lang.Void, Integer>>) (f -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(5),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, Integer>>) (x -> hydra.monads.Monads.pure(((f)).apply((x))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // bind

    @Test

    public void testBindBindAdd() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(10), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(5),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, Integer>>) (n -> hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
    (n),
    5)))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testBindBindMultiply() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(12), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(3),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, Integer>>) (n -> hydra.monads.Monads.pure(hydra.lib.math.Mul.apply(
    (n),
    4)))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // fail

    @Test

    public void testFailFailWithMessage() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.lang.Object>) ((hydra.compute.FlowState<java.lang.Void, java.lang.Object>) (new hydra.compute.FlowState<java.lang.Void, java.lang.Object>((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), java.util.List.of("Error: test error message ()"), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.lang.Object>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.lang.Object>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.lang.Object>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.lang.Object>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.fail("test error message"))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // foldl

    @Test

    public void testFoldlFoldlSum() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(6), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(0),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, Integer>>) (a0 -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
      (a0),
      1)),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, Integer>>) (a1 -> hydra.monads.Monads.bind(
      hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
        (a1),
        2)),
      (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, Integer>>) (a2 -> hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
        (a2),
        3)))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // map

    @Test

    public void testMapMapNegate() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(-5), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.lib.flows.Map.apply(
  (hydra.lib.math.Negate::apply),
  hydra.monads.Monads.pure(5)))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testMapMapAbs() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(3), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.lib.flows.Map.apply(
  (hydra.lib.math.Abs::apply),
  hydra.monads.Monads.pure(-3)))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // mapElems

    @Test

    public void testMapelemsMapelemsAddOne() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.util.Map<String, Integer>>) ((hydra.compute.FlowState<java.lang.Void, java.util.Map<String, Integer>>) (new hydra.compute.FlowState<java.lang.Void, java.util.Map<String, Integer>>(hydra.util.Maybe.just(java.util.Map.ofEntries(
  java.util.Map.entry(
    "a",
    2),
  java.util.Map.entry(
    "b",
    3))), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.Map<String, Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.Map<String, Integer>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.Map<String, Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.Map<String, Integer>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
    1,
    1)),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Map<String, Integer>>>) (v1 -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
      2,
      1)),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Map<String, Integer>>>) (v2 -> hydra.monads.Monads.pure(hydra.lib.maps.FromList.apply(java.util.List.of(
      (hydra.util.Tuple.Tuple2<String, Integer>) ((hydra.util.Tuple.Tuple2<String, Integer>) (new hydra.util.Tuple.Tuple2<String, Integer>("a", (v1)))),
      (hydra.util.Tuple.Tuple2<String, Integer>) ((hydra.util.Tuple.Tuple2<String, Integer>) (new hydra.util.Tuple.Tuple2<String, Integer>("b", (v2))))))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // mapKeys

    @Test

    public void testMapkeysMapkeysAddOne() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.util.Map<Integer, String>>) ((hydra.compute.FlowState<java.lang.Void, java.util.Map<Integer, String>>) (new hydra.compute.FlowState<java.lang.Void, java.util.Map<Integer, String>>(hydra.util.Maybe.just(java.util.Map.ofEntries(
  java.util.Map.entry(
    2,
    "a"),
  java.util.Map.entry(
    3,
    "b"))), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.Map<Integer, String>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.Map<Integer, String>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.Map<Integer, String>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.Map<Integer, String>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
    1,
    1)),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Map<Integer, String>>>) (k1 -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
      2,
      1)),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Map<Integer, String>>>) (k2 -> hydra.monads.Monads.pure(hydra.lib.maps.FromList.apply(java.util.List.of(
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>((k1), "a"))),
      (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>((k2), "b")))))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // mapList

    @Test

    public void testMaplistMaplistAddOne() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>) ((hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>) (new hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>(hydra.util.Maybe.just(java.util.List.of(
  2,
  3,
  4)), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
    1,
    1)),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>>) (y1 -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
      2,
      1)),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>>) (y2 -> hydra.monads.Monads.bind(
      hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
        3,
        1)),
      (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>>) (y3 -> hydra.monads.Monads.pure(java.util.List.of(
        (y1),
        (y2),
        (y3))))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // mapMaybe

    @Test

    public void testMapmaybeMapmaybeJust() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<Integer>>) ((hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<Integer>>) (new hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<Integer>>(hydra.util.Maybe.just(hydra.util.Maybe.just(6)), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, hydra.util.Maybe<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<Integer>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, hydra.util.Maybe<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<Integer>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
    5,
    1)),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, hydra.util.Maybe<Integer>>>) (y -> hydra.monads.Monads.pure(hydra.util.Maybe.just((y))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testMapmaybeMapmaybeNothing() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<java.lang.Object>>) ((hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<java.lang.Object>>) (new hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<java.lang.Object>>(hydra.util.Maybe.just((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, hydra.util.Maybe<java.lang.Object>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<java.lang.Object>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, hydra.util.Maybe<java.lang.Object>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, hydra.util.Maybe<java.lang.Object>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing())))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // mapSet

    @Test

    public void testMapsetMapsetAddOne() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.util.Set<Integer>>) ((hydra.compute.FlowState<java.lang.Void, java.util.Set<Integer>>) (new hydra.compute.FlowState<java.lang.Void, java.util.Set<Integer>>(hydra.util.Maybe.just(java.util.stream.Stream.of(
  2,
  3,
  4).collect(java.util.stream.Collectors.toSet())), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.Set<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.Set<Integer>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.Set<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.Set<Integer>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
    1,
    1)),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Set<Integer>>>) (y1 -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
      2,
      1)),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Set<Integer>>>) (y2 -> hydra.monads.Monads.bind(
      hydra.monads.Monads.pure(hydra.lib.math.Add.apply(
        3,
        1)),
      (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.Set<Integer>>>) (y3 -> hydra.monads.Monads.pure(hydra.lib.sets.FromList.apply(java.util.List.of(
        (y1),
        (y2),
        (y3)))))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // pure

    @Test

    public void testPurePureInteger() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(42), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure(42))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testPurePureZero() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(0), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure(0))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testPurePureNegative() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(-5), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure(-5))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testPurePureString() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, String>) ((hydra.compute.FlowState<java.lang.Void, String>) (new hydra.compute.FlowState<java.lang.Void, String>(hydra.util.Maybe.just("hello"), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, String>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, String>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, String>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, String>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure("hello"))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // sequence

    @Test

    public void testSequenceSequencePureList() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>) ((hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>) (new hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>(hydra.util.Maybe.just(java.util.List.of(
  1,
  2,
  3)), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.util.List<Integer>>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(1),
  (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>>) (x1 -> hydra.monads.Monads.bind(
    hydra.monads.Monads.pure(2),
    (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>>) (x2 -> hydra.monads.Monads.bind(
      hydra.monads.Monads.pure(3),
      (java.util.function.Function<Integer, hydra.compute.Flow<java.lang.Void, java.util.List<Integer>>>) (x3 -> hydra.monads.Monads.pure(java.util.List.of(
        (x1),
        (x2),
        (x3))))))))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }
}
