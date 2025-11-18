package hydra;

import hydra.compute.Coder;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.util.Maybe;
import hydra.util.Unit;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.lib.Libraries;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static hydra.Coders.roundTrip;
import static hydra.dsl.Flows.EMPTY_TRACE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;


/**
 * Base class for Hydra test utilities providing common assertion methods.
 */
public class HydraTestBase {

    /**
     * Assert that a flow fails when executed with the given initial state.
     * @param <S> the state type
     * @param <X> the result type
     * @param flow the flow to execute
     * @param initialState the initial state
     */
    protected static <S, X> void assertFails(Flow<S, X> flow, S initialState) {
        FlowState<S, X> result = flow.value.apply(initialState).apply(EMPTY_TRACE);
        assertTrue(!result.value.isJust());
        //assertTrue(result.trace.messages.size() > 1);
    }

    /**
     * Assert that a stateless flow fails when executed.
     * @param <X> the result type
     * @param flow the flow to execute
     */
    protected static <X> void assertFails(Flow<Unit, X> flow) {
        assertFails(flow, Flows.UNIT);
    }

    /**
     * Assert that encoding and then decoding a value results in the original value.
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the stateless coder to test
     * @param initialValue the initial value
     */
    protected static <V1, V2> void assertRoundTripIsNoop(Coder<Unit, Unit, V1, V2> coder, V1 initialValue) {
        assertRoundTripIsNoop(coder, new Unit(), initialValue);
    }

    /**
     * Assert that encoding and then decoding a value results in the original value.
     * @param <S> the state type
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the coder to test
     * @param initialState the initial state
     * @param initialValue the initial value
     */
    protected static <S, V1, V2> void assertRoundTripIsNoop(Coder<S, S, V1, V2> coder,
                                                            S initialState,
                                                            V1 initialValue) {
        assertSucceedsWith(initialValue, roundTrip(coder, initialValue), initialState);
    }

    /**
     * Assert that encoding and then decoding a value fails.
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the stateless coder to test
     * @param initialValue the initial value
     */
    protected static <V1, V2> void assertRoundTripFails(Coder<Unit, Unit, V1, V2> coder, V1 initialValue) {
        assertFails(roundTrip(coder, initialValue));
    }

    /**
     * Assert that encoding and then decoding a value fails.
     * @param <S> the state type
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the coder to test
     * @param initialState the initial state
     * @param initialValue the initial value
     */
    protected static <S, V1, V2> void assertRoundTripFails(Coder<S, S, V1, V2> coder,
                                                           S initialState,
                                                           V1 initialValue) {
        assertFails(roundTrip(coder, initialValue), initialState);
    }

    /**
     * Assert that a flow succeeds when executed with the given initial state.
     * @param <S> the state type
     * @param <X> the result type
     * @param flow the flow to execute
     * @param initialState the initial state
     */
    protected static <S, X> void assertSucceeds(Flow<S, X> flow, S initialState) {
        checkFlow(flow, initialState, x -> assertTrue(true));
    }

    /**
     * Assert that a stateless flow succeeds when executed.
     * @param <X> the result type
     * @param flow the flow to execute
     */
    protected static <X> void assertSucceeds(Flow<Unit, X> flow) {
        assertSucceeds(flow, new Unit());
    }

    /**
     * Assert that a stateless flow succeeds and produces the expected result.
     * @param <X> the result type
     * @param expected the expected result
     * @param flow the flow to execute
     */
    protected static <X> void assertSucceedsWith(X expected, Flow<Unit, X> flow) {
        assertSucceedsWith(expected, flow, new Unit());
    }

    /**
     * Assert that a flow succeeds and produces the expected result.
     * @param <S> the state type
     * @param <X> the result type
     * @param expected the expected result
     * @param flow the flow to execute
     * @param initialState the initial state
     */
    protected static <S, X> void assertSucceedsWith(X expected, Flow<S, X> flow, S initialState) {
        checkFlow(flow, initialState, x -> assertEquals(expected, x));
    }

    /**
     * Execute a flow and apply a consumer to the result if successful.
     * @param <S> the state type
     * @param <X> the result type
     * @param flow the flow to execute
     * @param initialState the initial state
     * @param consumer the consumer to apply to the result
     */
    protected static <S, X> void checkFlow(Flow<S, X> flow, S initialState, Consumer<X> consumer) {
        FlowState<S, X> result = flow.value.apply(initialState).apply(EMPTY_TRACE);
        assertTrue(result.value.isJust(), "Flow failed: " + result.trace.messages);
        consumer.accept(result.value.fromJust());
    }

    /**
     * Execute a stateless flow and apply a consumer to the result if successful.
     * @param <X> the result type
     * @param flow the flow to execute
     * @param consumer the consumer to apply to the result
     */
    protected static <X> void checkFlow(Flow<Unit, X> flow, Consumer<X> consumer) {
        checkFlow(flow, new Unit(), consumer);
    }

    /**
     * Create an empty graph with standard primitives.
     * @return an empty graph
     */
    protected static Graph emptyGraph() {
        Map<Name, hydra.core.Binding> elements = Collections.emptyMap();
        Map<Name, TypeScheme> types = Collections.emptyMap();
        Map<Name, Maybe<Term>> environment = Collections.emptyMap();
        Term body = Terms.string("empty graph");

        Map<Name, Primitive> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        Maybe<Graph> schema = Maybe.nothing();

        return new Graph(elements, environment, types, body, primitives, schema);
    }
}
