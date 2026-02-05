// Note: this is an automatically generated file. Do not edit.
// monads

package generation.hydra.test;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class MonadsTest {

    // pure

    @Test

    public void testPureInteger() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(42), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure(42))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testPureString() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, String>) ((hydra.compute.FlowState<java.lang.Void, String>) (new hydra.compute.FlowState<java.lang.Void, String>(hydra.util.Maybe.just("hello"), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, String>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, String>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, String>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, String>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.pure("hello"))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // map

    @Test

    public void testMapMapNegate() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(-5), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.map(
  (hydra.lib.math.Negate::apply),
  hydra.monads.Monads.pure(5)))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    @Test

    public void testMapMapAbsolute() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(3), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.map(
  (hydra.lib.math.Abs::apply),
  hydra.monads.Monads.pure(-3)))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }

    // bind

    @Test

    public void testBindBindAdd() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, Integer>) ((hydra.compute.FlowState<java.lang.Void, Integer>) (new hydra.compute.FlowState<java.lang.Void, Integer>(hydra.util.Maybe.just(15), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, Integer>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, Integer>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.bind(
  hydra.monads.Monads.pure(10),
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

    // error traces

    @Test

    public void testErrorTracesErrorTracesAreInTheRightOrder() {

        assertEquals(

            (hydra.compute.FlowState<java.lang.Void, java.lang.Object>) ((hydra.compute.FlowState<java.lang.Void, java.lang.Object>) (new hydra.compute.FlowState<java.lang.Void, java.lang.Object>((hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()), null, new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), java.util.List.of("Error: oops (one > two)"), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))))),

            ((((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.lang.Object>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.lang.Object>>>>) ((java.util.function.Function<hydra.compute.Flow<java.lang.Void, java.lang.Object>, java.util.function.Function<java.lang.Void, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<java.lang.Void, java.lang.Object>>>>) (wrapped -> ((wrapped)).value))).apply(hydra.monads.Monads.withTrace(
  "one",
  hydra.monads.Monads.withTrace(
    "two",
    hydra.monads.Monads.fail("oops"))))).apply(null)).apply(new hydra.compute.Trace((java.util.List<String>) (java.util.List.<String>of()), (java.util.List<String>) (java.util.List.<String>of()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (java.util.Map.<hydra.core.Name, hydra.core.Term>ofEntries())))));

    }
}
