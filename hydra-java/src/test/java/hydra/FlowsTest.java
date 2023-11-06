package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static hydra.Flows.EMPTY_TRACE;
import static hydra.Flows.bind;
import static hydra.Flows.getState;
import static hydra.Flows.map;
import static hydra.Flows.mapM;
import static hydra.Flows.pure;
import static hydra.Flows.putState;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class FlowsTest {
    @Test
    public void checkFail() {
        Flow<String, Integer> flow1 = Flows.fail("oops");
        FlowState<String, Integer> result1 = flow1.value.apply("").apply(EMPTY_TRACE);
        assertFalse(result1.value.isPresent());
        assertEquals(1, result1.trace.messages.size());
        assertEquals("Error: oops", result1.trace.messages.get(0));

        Function<String, Flow<Boolean, Integer>> nonzeroLen =
            s -> s.length() > 0 ? pure(s.length()) : Flows.fail("zero");
        List<String> strings = Arrays.asList("one", "two", "", "four");
        Flow<Boolean, List<Integer>> flow2 = mapM(strings, nonzeroLen);
        FlowState<Boolean, List<Integer>> result2 = flow2.value.apply(true).apply(EMPTY_TRACE);
        assertFalse(result2.value.isPresent());
    }

    @Test
    public void checkGetAndPutState() {
        Flow<Integer, Integer> flow1 = bind(pure(42), value -> map(getState(), state -> value + state));
        FlowState<Integer, Integer> result1 = flow1.value.apply(10).apply(EMPTY_TRACE);
        assertTrue(result1.value.isPresent());
        assertEquals(52, result1.value.get());

        Flow<Integer, Integer> flow2 = bind(pure(42), original -> bind(putState(100), ignored -> pure(original)));
        FlowState<Integer, Integer> result2 = flow2.value.apply(10).apply(EMPTY_TRACE);
        assertTrue(result2.value.isPresent());
        assertEquals(42, result2.value.get());
        assertEquals(100, result2.state);

        Flow<String, Integer> flow3 = bind(pure(42), value -> bind(getState(), state -> {
            String newState = state + ";" + value;
            return bind(putState(newState), ignored -> pure(value + 1));
        }));
        FlowState<String, Integer> result3 = flow3.value.apply("foo").apply(EMPTY_TRACE);
        assertEquals("foo;42", result3.state);
        assertEquals(Optional.of(43), result3.value);
    }

    @Test
    public void checkWarnings() {
        final Flow<Void, Integer> flow0 = pure(42);
        final Flow<Void, Integer> flow1 = Flows.warn("oops", flow0);
        final Flow<Void, Integer> flow2 = Flows.warn("drat", flow1);
        final Flow<Void, Integer> flow3 = Flows.warn("oops", flow2);
        final Flow<Void, Integer> flow4 = Flows.bind(flow3, x -> Flows.fail("failed"));

        final FlowState<Void, Integer> result0 = flow0.value.apply(null).apply(EMPTY_TRACE);
        final FlowState<Void, Integer> result1 = flow1.value.apply(null).apply(EMPTY_TRACE);
        final FlowState<Void, Integer> result2 = flow2.value.apply(null).apply(EMPTY_TRACE);
        final FlowState<Void, Integer> result3 = flow3.value.apply(null).apply(EMPTY_TRACE);
        final FlowState<Void, Integer> result4 = flow4.value.apply(null).apply(EMPTY_TRACE);

        assertTrue(result0.value.isPresent());
        assertTrue(result1.value.isPresent());
        assertTrue(result2.value.isPresent());
        assertTrue(result3.value.isPresent());
        assertFalse(result4.value.isPresent());

        assertEquals(0, result0.trace.messages.size());
        assertEquals(1, result1.trace.messages.size());
        assertEquals(2, result2.trace.messages.size());
        assertEquals(3, result3.trace.messages.size());
        assertEquals(4, result4.trace.messages.size());

        assertEquals("Warning: oops", result1.trace.messages.get(0));

        // Warning messages are FILO
        assertEquals("Warning: drat", result2.trace.messages.get(0));
        assertEquals("Warning: oops", result2.trace.messages.get(1));

        // Failure message appears at the end
        assertEquals("Warning: oops", result4.trace.messages.get(0));
        assertEquals("Warning: drat", result4.trace.messages.get(1));
        assertEquals("Warning: oops", result4.trace.messages.get(2));
        assertEquals("Error: failed", result4.trace.messages.get(3));
    }
}
