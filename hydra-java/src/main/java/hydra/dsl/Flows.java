package hydra.dsl;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Trace;
import hydra.util.Maybe;
import hydra.util.Unit;
import hydra.tools.FlowException;
import hydra.tools.Function3;
import hydra.tools.Function4;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Objects.requireNonNull;


/**
 * A collection of convenience methods for constructing and composing flows, or stateful computations.
 */
public interface Flows {
    Trace EMPTY_TRACE
            = new Trace(Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());

    /**
     * The maximum size of a collection over which we can apply the mapM() functions.
     * This is a conservative limit which will avoid stack overflow conditions in typical JVM environments.
     */
    int MAX_MAPM_SIZE = 1000;

    Unit UNIT = new Unit();

    /**
     * Apply a function flow to a domain value flow.
     * @param <S> the state type
     * @param <A> the input type
     * @param <B> the output type
     * @param mapping the flow containing a function from A to B
     * @param input the flow containing an input value
     * @return a flow containing the result of applying the function to the input
     */
    static <S, A, B> Flow<S, B> apply(Flow<S, Function<A, B>> mapping, Flow<S, A> input) {
        requireNonNull(mapping, "mapping");
        requireNonNull(input, "input");

        return new Flow<>(s0 -> t0 -> {
            FlowState<S, Function<A, B>> fs1 = mapping.value.apply(s0).apply(t0);
            Maybe<Function<A, B>> mf = fs1.value;
            return mf.isJust()
                    ? map(mf.fromJust(), input).value.apply(fs1.state).apply(fs1.trace)
                    : new FlowState<>(Maybe.nothing(), fs1.state, fs1.trace);
        });
    }

    /**
     * Monadic bind function for flows.
     * @param <S> the state type
     * @param <A> the input type
     * @param <B> the output type
     * @param p the input flow
     * @param f the function to apply to the flow's value
     * @return a flow containing the result of the bind operation
     */
    static <S, A, B> Flow<S, B> bind(Flow<S, A> p, Function<A, Flow<S, B>> f) {
        requireNonNull(p, "p");
        requireNonNull(f, "f");

        return new Flow<>(s0 -> t0 -> {
            FlowState<S, A> fs1 = p.value.apply(s0).apply(t0);
            Maybe<A> a = fs1.value;
            return a.isJust()
                    ? f.apply(a.fromJust()).value.apply(fs1.state).apply(fs1.trace)
                    : new FlowState<>(Maybe.nothing(), fs1.state, fs1.trace);
        });
    }

    /**
     * Monadic bind with reversed arguments.
     * @param <S> the state type
     * @param <A> the input type
     * @param <B> the output type
     * @param f the function to apply to the flow's value
     * @param p the input flow
     * @return a flow containing the result of the bind operation
     */
    static <S, A, B> Flow<S, B> bind(Function<A, Flow<S, B>> f, Flow<S, A> p) {
        return bind(p, f);
    }

    /**
     * Variant of monadic bind which takes two monadic arguments and a binary function.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the output type
     * @param p1 the first input flow
     * @param p2 the second input flow
     * @param f the binary function to apply
     * @return a flow containing the result of the bind operation
     */
    static <S, A, B, C> Flow<S, C> bind2(Flow<S, A> p1, Flow<S, B> p2, BiFunction<A, B, Flow<S, C>> f) {
        requireNonNull(p1, "p1");
        requireNonNull(p2, "p2");
        requireNonNull(f, "f");

        return Flows.bind(p1, a -> Flows.bind(p2, b -> f.apply(a, b)));
    }

    /**
     * Two-argument monadic bind with reversed arguments.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the output type
     * @param f the binary function to apply
     * @param p1 the first input flow
     * @param p2 the second input flow
     * @return a flow containing the result of the bind operation
     */
    static <S, A, B, C> Flow<S, C> bind2(BiFunction<A, B, Flow<S, C>> f, Flow<S, A> p1, Flow<S, B> p2) {
        return Flows.bind2(p1, p2, f);
    }

    /**
     * Variant of monadic bind which takes three monadic arguments and an arity-3 function.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the third input type
     * @param <D> the output type
     * @param p1 the first input flow
     * @param p2 the second input flow
     * @param p3 the third input flow
     * @param f the arity-3 function to apply
     * @return a flow containing the result of the bind operation
     */
    static <S, A, B, C, D> Flow<S, D> bind3(Flow<S, A> p1,
                                            Flow<S, B> p2,
                                            Flow<S, C> p3,
                                            Function3<A, B, C, Flow<S, D>> f) {
        requireNonNull(p1, "p1");
        requireNonNull(p2, "p2");
        requireNonNull(p3, "p3");

        return Flows.bind(p1, a -> Flows.bind2(p2, p3, (b, c) -> f.apply(a, b, c)));
    }

    /**
     * Three-argument monadic bind with reversed arguments.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the third input type
     * @param <D> the output type
     * @param f the arity-3 function to apply
     * @param p1 the first input flow
     * @param p2 the second input flow
     * @param p3 the third input flow
     * @return a flow containing the result of the bind operation
     */
    static <S, A, B, C, D> Flow<S, D> bind3(Function3<A, B, C, Flow<S, D>> f,
                                            Flow<S, A> p1,
                                            Flow<S, B> p2,
                                            Flow<S, C> p3) {
        return Flows.bind3(p1, p2, p3, f);
    }

    /**
     * Check whether a given value satisfies a list of predicates, returning the value itself if all checks are
     * successful, or a failure flow for the first predicate that fails.
     * @param <S> the state type
     * @param <A> the value type
     * @param input the value to check
     * @param predicates the predicates to apply
     * @return a flow containing the input if all predicates pass, or a failure flow otherwise
     */
    static <S, A> Flow<S, A> check(A input, Function<A, Maybe<String>>... predicates) {
        requireNonNull(input, "input");
        requireNonNull(predicates, "predicates");

        for (Function<A, Maybe<String>> predicate : predicates) {
            Maybe<String> msg = predicate.apply(input);
            if (msg.isJust()) {
                return Flows.fail(msg.fromJust());
            }
        }

        return pure(input);
    }

    /**
     * Compose two monadic functions, feeding the output of the first into the second.
     * @param <S> the state type
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param f the first function
     * @param g the second function
     * @return a composed function from A to Flow&lt;S, C&gt;
     */
    static <S, A, B, C> Function<A, Flow<S, C>> compose(Function<A, Flow<S, B>> f, Function<B, Flow<S, C>> g) {
        requireNonNull(f, "f");
        requireNonNull(g, "g");

        return a -> Flows.bind(f.apply(a), g);
    }

    /**
     * Evaluate a flow and consume the result.
     * @param <S> the state type
     * @param <A> the value type
     * @param x the flow to evaluate
     * @param consumer the consumer to apply to the result
     * @return a flow containing Unit
     */
    static <S, A> Flow<S, Unit> consume(Flow<S, A> x, Consumer<A> consumer) {
        requireNonNull(x, "x");
        requireNonNull(consumer, "consumer");

        return map(x, a -> {
            consumer.accept(a);
            return new Unit();
        });
    }

    /**
     * Produce a failure flow with the provided message.
     * @param <S> the state type
     * @param <A> the value type
     * @param msg the error message
     * @return a failure flow
     */
    static <S, A> Flow<S, A> fail(String msg) {
        requireNonNull(msg, "msg");

        return new Flow<>(s -> trace -> {
            String errMsg = "Error: " + msg; // TODO: include stack trace
            List<String> messages = new ArrayList<>(trace.messages);
            messages.add(errMsg);
            return new FlowState<>(Maybe.nothing(), s, trace.withMessages(messages));
        });
    }

    /**
     * Produce a failure flow with the provided message and additional information from a Throwable.
     * @param <S> the state type
     * @param <A> the value type
     * @param msg the error message
     * @param cause the throwable that caused the failure
     * @return a failure flow
     */
    static <S, A> Flow<S, A> fail(String msg, Throwable cause) {
        requireNonNull(msg, "msg");

        return fail(msg + ": " + cause.getMessage());
    }

    /**
     * Extract the value from a flow, returning a default value instead if the flow failed.
     * @param <S> the state type
     * @param <A> the value type
     * @param dflt the default value to return if the flow failed
     * @param state the initial state
     * @param flow the flow to extract from
     * @return the extracted value or the default value
     */
    static <S, A> A fromFlow(A dflt, S state, Flow<S, A> flow) {
        requireNonNull(dflt, "dflt");
        requireNonNull(state, "state");
        requireNonNull(flow, "flow");

        FlowState<S, A> result = flow.value.apply(state).apply(EMPTY_TRACE);
        return result.value.orElse(dflt);
    }

    /**
     * Extract the value from a flow, throwing an exception if the flow failed.
     * @param <S> the state type
     * @param <A> the value type
     * @param state the initial state
     * @param flow the flow to extract from
     * @return the extracted value
     * @throws FlowException if the flow failed
     */
    static <S, A> A fromFlow(S state, Flow<S, A> flow) throws FlowException {
        requireNonNull(state, "state");
        requireNonNull(flow, "flow");

        FlowState<S, A> result = flow.value.apply(state).apply(EMPTY_TRACE);
        if (result.value.isJust()) {
            return result.value.fromJust();
        } else {
            throw new FlowException(result.trace);
        }
    }

    /**
     * Extract the value from a stateless flow, throwing an exception if the flow failed.
     * @param <A> the value type
     * @param flow the flow to extract from
     * @return the extracted value
     * @throws FlowException if the flow failed
     */
    static <A> A fromFlow(Flow<Unit, A> flow) throws FlowException {
        return fromFlow(new Unit(), flow);
    }

    /**
     * Extract the state from a flow.
     * @param <S> the state type
     * @return a flow containing the current state
     */
    static <S> Flow<S, S> getState() {
        return new Flow<>(s0 -> t0 -> new FlowState<>(Maybe.just(s0), s0, t0));
    }

    /**
     * Map a function over a flow.
     * @param <S> the state type
     * @param <A> the input type
     * @param <B> the output type
     * @param f the function to map
     * @param x the flow to map over
     * @return a flow containing the mapped result
     */
    static <S, A, B> Flow<S, B> map(Function<A, B> f, Flow<S, A> x) {
        requireNonNull(f, "f");
        requireNonNull(x, "x");

        return new Flow<>(s -> trace -> {
            FlowState<S, A> result = x.value.apply(s).apply(trace);
            return new FlowState<>(result.value.map(f), result.state, result.trace);
        });
    }

    /**
     * Map a function over a flow, with reversed arguments.
     * @param <S> the state type
     * @param <A> the input type
     * @param <B> the output type
     * @param x the flow to map over
     * @param f the function to map
     * @return a flow containing the mapped result
     */
    static <S, A, B> Flow<S, B> map(Flow<S, A> x, Function<A, B> f) {
        return map(f, x);
    }

    /**
     * Map a monadic function over a list, producing a flow of lists.
     * @param <S> the state type
     * @param <A> the input element type
     * @param <B> the output element type
     * @param as the list to map over
     * @param f the monadic function to apply
     * @return a flow containing a list of results
     */
    static <S, A, B> Flow<S, List<B>> mapM(List<A> as, Function<A, Flow<S, B>> f) {
        requireNonNull(as, "as");
        requireNonNull(f, "f");

        if (as.size() > MAX_MAPM_SIZE) {
            throw new IllegalArgumentException("Can't mapM over a collection with more than "
                + MAX_MAPM_SIZE + " (MAX_MAPM_SIZE) elements. This would present a risk of stack overflow.");
        }

        Flow<S, List<B>> result = pure(new ArrayList<>());
        for (A a : as) {
            result = bind(result, ys -> map(f.apply(a), b -> {
                List<B> newList = new ArrayList<>(ys);
                newList.add(b);
                return newList;
            }));
        }
        return result;
    }

    /**
     * Map a monadic function over an array, producing a flow of lists.
     * @param <S> the state type
     * @param <A> the input element type
     * @param <B> the output element type
     * @param xs the array to map over
     * @param f the monadic function to apply
     * @return a flow containing a list of results
     */
    static <S, A, B> Flow<S, List<B>> mapM(A[] xs, Function<A, Flow<S, B>> f) {
        requireNonNull(xs, "xs");
        requireNonNull(f, "f");

        return mapM(Arrays.asList(xs), f);
    }

    /**
     * Map a monadic function over the keys of a map, and another monadic function over the values of a map,
     * producing a flow of maps.
     * @param <S> the state type
     * @param <K1> the input key type
     * @param <V1> the input value type
     * @param <K2> the output key type
     * @param <V2> the output value type
     * @param xs the map to transform
     * @param kf the monadic function to apply to keys
     * @param vf the monadic function to apply to values
     * @return a flow containing the transformed map
     */
    static <S, K1, V1, K2, V2> Flow<S, Map<K2, V2>> mapM(Map<K1, V1> xs,
                                                         Function<K1, Flow<S, K2>> kf,
                                                         Function<V1, Flow<S, V2>> vf) {
        requireNonNull(xs, "xs");
        requireNonNull(kf, "kf");
        requireNonNull(vf, "vf");

        Set<Map.Entry<K1, V1>> entries1 = xs.entrySet();
        Flow<S, Set<Map.Entry<K2, V2>>> entries2 = mapM(entries1,
                e -> bind(kf.apply(e.getKey()), k2 -> map(vf.apply(e.getValue()),
                        v2 -> new AbstractMap.SimpleEntry<>(k2, v2))));
        return map(entries2, entries -> entries.stream()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
    }

    /**
     * Map a monadic function over an optional value, producing a flow of optionals.
     * @param <S> the state type
     * @param <A> the input value type
     * @param <B> the output value type
     * @param xs the optional to map over
     * @param f the monadic function to apply
     * @return a flow containing an optional result
     */
    static <S, A, B> Flow<S, Maybe<B>> mapM(Maybe<A> xs, Function<A, Flow<S, B>> f) {
        requireNonNull(xs, "xs");
        requireNonNull(f, "f");

        return xs.map(a -> map(f.apply(a), Maybe::just)).orElseGet(() -> pure(Maybe.nothing()));
    }

    /**
     * Map a monadic function over a set, producing a flow of sets.
     * @param <S> the state type
     * @param <A> the input element type
     * @param <B> the output element type
     * @param as the set to map over
     * @param f the monadic function to apply
     * @return a flow containing a set of results
     */
    static <S, A, B> Flow<S, Set<B>> mapM(Set<A> as, Function<A, Flow<S, B>> f) {
        requireNonNull(as, "as");
        requireNonNull(f, "f");

        if (as.size() > MAX_MAPM_SIZE) {
            throw new IllegalArgumentException("Can't mapM over a collection with more than "
                + MAX_MAPM_SIZE + " (MAX_MAPM_SIZE) elements. This would present a risk of stack overflow.");
        }

        Flow<S, Set<B>> result = pure(new HashSet<>(as.size()));
        for (A a : as) {
            result = bind(result, ys -> map(f.apply(a), b -> {
                Set<B> newSet = new HashSet<>(ys);
                newSet.add(b);
                return newSet;
            }));
        }
        return result;
    }

    /**
     * Map a bifunction over two flows, producing a flow.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the output type
     * @param x the first flow
     * @param y the second flow
     * @param f the bifunction to apply
     * @return a flow containing the result
     */
    static <S, A, B, C> Flow<S, C> map2(Flow<S, A> x, Flow<S, B> y, BiFunction<A, B, C> f) {
        requireNonNull(x, "x");
        requireNonNull(y, "y");
        requireNonNull(f, "f");

        return Flows.bind(x, a1 -> Flows.bind(y, b1 -> Flows.pure(f.apply(a1, b1))));
    }

    /**
     * Map an arity-3 function over three flows, producing a flow.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the third input type
     * @param <D> the output type
     * @param a the first flow
     * @param b the second flow
     * @param c the third flow
     * @param f the arity-3 function to apply
     * @return a flow containing the result
     */
    static <S, A, B, C, D> Flow<S, D> map3(Flow<S, A> a, Flow<S, B> b, Flow<S, C> c, Function3<A, B, C, D> f) {
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        requireNonNull(c, "c");
        requireNonNull(f, "f");

        return Flows.bind(a,
                a1 -> Flows.bind(b,
                        b1 -> Flows.bind(c,
                                c1 -> Flows.pure(f.apply(a1, b1, c1)))));
    }

    /**
     * Map an arity-4 function over four flows, producing a flow.
     * @param <S> the state type
     * @param <A> the first input type
     * @param <B> the second input type
     * @param <C> the third input type
     * @param <D> the fourth input type
     * @param <E> the output type
     * @param a the first flow
     * @param b the second flow
     * @param c the third flow
     * @param d the fourth flow
     * @param f the arity-4 function to apply
     * @return a flow containing the result
     */
    static <S, A, B, C, D, E> Flow<S, E> map4(Flow<S, A> a,
                                              Flow<S, B> b,
                                              Flow<S, C> c,
                                              Flow<S, D> d,
                                              Function4<A, B, C, D, E> f) {
        requireNonNull(a, "a");
        requireNonNull(b, "b");
        requireNonNull(c, "c");
        requireNonNull(d, "d");
        requireNonNull(f, "f");

        return Flows.bind(a,
                a1 -> Flows.bind(b,
                        b1 -> Flows.bind(c,
                                c1 -> Flows.bind(d,
                                        d1 -> Flows.pure(f.apply(a1, b1, c1, d1))))));
    }

    /**
     * Produce a given object as a pure flow; the value is guaranteed to be present,
     * and neither state nor trace are modified.
     * @param <S> the state type
     * @param <A> the value type
     * @param obj the object to wrap in a flow
     * @return a pure flow containing the object
     */
    static <S, A> Flow<S, A> pure(A obj) {
        return new Flow<>(s -> trace -> new FlowState<>(Maybe.just(obj), s, trace));
    }

    /**
     * Modify the state of a flow.
     * @param <S> the state type
     * @param snew the new state
     * @return a flow with the updated state
     */
    static <S> Flow<S, Boolean> putState(S snew) {
        requireNonNull(snew, "snew");

        // Note: for lack of a unit value other than null,
        // we use use a boolean as the ignorable value output of putState()
        return new Flow<>(s0 -> t0 -> new FlowState<>(Maybe.just(true), snew, t0));
    }

    /**
     * Test an optional value, producing a flow with the value if present, or an error flow if absent.
     * @param <S> the state type
     * @param <A> the value type
     * @param optValue the optional value to test
     * @param category a description for error messages
     * @return a flow containing the value if present, or a failure flow if absent
     */
    static <S, A> Flow<S, A> require(Maybe<A> optValue, String category) {
        requireNonNull(optValue, "optValue");
        requireNonNull(category, "category");

        return optValue.isJust()
                ? Flows.pure(optValue.fromJust())
                : Flows.fail("require " + category + " is missing/null");
    }

    /**
     * Evaluate each flow from left to right, and produce a flow of the resulting list.
     * Analogous to the sequence function in Haskell.
     * @param <S> the state type
     * @param <A> the element type
     * @param elements the list of flows to sequence
     * @return a flow containing a list of results
     */
    static <S, A> Flow<S, List<A>> sequence(List<Flow<S, A>> elements) {
        requireNonNull(elements, "elements");

        Flow<S, List<A>> result = Flows.pure(new ArrayList<>(elements.size()));
        for (Flow<S, A> element : elements) {
            result = Flows.bind(result, xs -> Flows.map(element, x -> {
                List<A> newList = new ArrayList<>(xs);
                newList.add(x);
                return newList;
            }));
        }
        return result;
    }

    /**
     * Produce an error flow indicating an unexpected value.
     * For example, if you expect a string but find an integer, use unexpected("string", myInt).
     * @param <S> the state type
     * @param <A> the value type
     * @param cat the expected category
     * @param obj the actual object found
     * @return a failure flow with an appropriate error message
     */
    static <S, A> Flow<S, A> unexpected(String cat, Object obj) {
        return fail("expected " + cat + " but found: " + obj);
    }

    /**
     * Produce an error flow indicating an unexpected class of value.
     * @param <S> the state type
     * @param <A> the value type
     * @param cat the expected category
     * @param obj the actual object found
     * @return a failure flow with an appropriate error message
     */
    static <S, A> Flow<S, A> unexpectedClass(String cat, Object obj) {
        return fail("expected " + cat + " but found an instance of " + obj.getClass().getName());
    }

    /**
     * Continue a flow after adding a warning message.
     * @param <S> the state type
     * @param <A> the value type
     * @param message the warning message
     * @param flow the flow to continue
     * @return the flow with the warning added to its trace
     */
    static <S, A> Flow<S, A> warn(String message, Flow<S, A> flow) {
        requireNonNull(message, "message");
        requireNonNull(flow, "flow");

        return new Flow<>(s0 -> t0 -> {
            FlowState<S, A> result = flow.value.apply(s0).apply(t0);
            String warnMsg = "Warning: " + message;
            List<String> messages = new ArrayList<>(result.trace.messages);
            messages.add(0, warnMsg);  // Prepend to maintain FILO ordering
            return new FlowState<>(result.value, result.state, result.trace.withMessages(messages));
        });
    }
}
