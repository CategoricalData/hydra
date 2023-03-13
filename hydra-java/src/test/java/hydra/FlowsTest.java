package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Trace;
import hydra.dsl.Flows;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

import static hydra.dsl.Flows.*;
import static org.junit.jupiter.api.Assertions.*;


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
}
