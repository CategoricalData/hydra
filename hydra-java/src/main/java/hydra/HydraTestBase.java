package hydra;

import hydra.compute.Coder;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Unit;
import hydra.dsl.Terms;
import hydra.graph.AnnotationClass;
import hydra.graph.Element;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.lib.Libraries;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

import static hydra.Coders.roundTrip;
import static hydra.Flows.EMPTY_TRACE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class HydraTestBase {

    protected static <S, X> void assertFails(Flow<S, X> flow, S initialState) {
        FlowState<S, X> result = flow.value.apply(initialState).apply(EMPTY_TRACE);
        assertTrue(!result.value.isPresent());
        //assertTrue(result.trace.messages.size() > 1);
    }

    protected static <X> void assertFails(Flow<Unit, X> flow) {
        assertFails(flow, Flows.UNIT);
    }

    protected static <V1, V2> void assertRoundTripIsNoop(Coder<Unit, Unit, V1, V2> coder, V1 initialValue) {
        assertRoundTripIsNoop(coder, new Unit(), initialValue);
    }

    protected static <S, V1, V2> void assertRoundTripIsNoop(Coder<S, S, V1, V2> coder,
                                                            S initialState,
                                                            V1 initialValue) {
        assertSucceedsWith(initialValue, roundTrip(coder, initialValue), initialState);
    }

    protected static <V1, V2> void assertRoundTripFails(Coder<Unit, Unit, V1, V2> coder, V1 initialValue) {
        assertFails(roundTrip(coder, initialValue));
    }

    protected static <S, V1, V2> void assertRoundTripFails(Coder<S, S, V1, V2> coder,
                                                           S initialState,
                                                           V1 initialValue) {
        assertFails(roundTrip(coder, initialValue), initialState);
    }

    protected static <X> void assertSucceedsWith(X expected, Flow<Unit, X> flow) {
        assertSucceedsWith(expected, flow, new Unit());
    }

    protected static <S, X> void assertSucceedsWith(X expected, Flow<S, X> flow, S initialState) {
        checkFlow(flow, initialState, x -> assertEquals(expected, x));
    }

    protected static <S, X> void checkFlow(Flow<S, X> flow, S initialState, Consumer<X> consumer) {
        FlowState<S, X> result = flow.value.apply(initialState).apply(EMPTY_TRACE);
        assertTrue(result.value.isPresent(), "Flow failed: " + result.trace.messages);
        consumer.accept(result.value.get());
    }

    protected static <X> void checkFlow(Flow<Unit, X> flow, Consumer<X> consumer) {
        checkFlow(flow, new Unit(), consumer);
    }

    protected static <A> Graph<A> emptyGraph(AnnotationClass<A> anns) {
        Map<Name, Element<A>> elements = Collections.emptyMap();
        Map<Name, Optional<Term<A>>> environment = Collections.emptyMap();
        Term<A> body = Terms.string("empty graph");

        Map<Name, Primitive<A>> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        Optional<Graph<A>> schema = Optional.empty();

        return new Graph<>(elements, environment, body, primitives, anns, schema);
    }
}
