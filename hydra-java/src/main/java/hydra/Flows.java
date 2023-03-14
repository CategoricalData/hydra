package hydra;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.compute.Trace;
import hydra.tools.FlowException;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;


public interface Flows {
    Trace EMPTY_TRACE
            = new Trace(Collections.emptyList(), Collections.emptyList(), Collections.emptyMap());

    static <S, X, Y> Flow<S, Y> bind(Flow<S, X> p, Function<X, Flow<S, Y>> k) {
        return new Flow<>(s0 -> t0 -> {
            FlowState<S, X> fs1 = p.value.apply(s0).apply(t0);
            Optional<X> x = fs1.value;
            return x.isPresent()
                    ? k.apply(x.get()).value.apply(fs1.state).apply(fs1.trace)
                    : new FlowState<>(Optional.empty(), fs1.state, fs1.trace);
        });
    }

    static <S, X, Y, Z> Flow<S, Z> bind2(Flow<S, X> p1, Flow<S, Y> p2, BiFunction<X, Y, Flow <S, Z>> k) {
        return Flows.bind(p1, x -> Flows.bind(p2, y -> k.apply(x, y)));
    }

    static <S, X> Flow<S, X> fail(String msg) {
        return new Flow<>(s -> trace -> {
            String errMsg = "Error: " + msg; // TODO: include stack trace
            List<String> messages = new ArrayList<>(trace.messages);
            messages.add(errMsg);
            return new FlowState<>(Optional.empty(), s, trace.withMessages(messages));
        });
    }

    static <S, X> X fromFlow(Flow<S, X> flow) throws FlowException {
        FlowState<S, X> wrapper = flow.value.apply(null).apply(EMPTY_TRACE);

        if (!wrapper.value.isPresent()) {
            throw new FlowException(wrapper.trace);
        } else {
            return wrapper.value.get();
        }
    }

    static <S> Flow<S, S> getState() {
      return new Flow<>(s0 -> t0 -> new FlowState<>(Optional.of(s0), s0, t0));
    }

    static <S, X, Y> Flow<S, Y> map(Flow <S, X> x, Function<X, Y> f) {
        return new Flow<>(s -> trace -> {
            FlowState<S, X> result = x.value.apply(s).apply(trace);
            return new FlowState<>(result.value.map(f), result.state, result.trace);
        });
    }

    static <S, X, Y> Flow<S, List<Y>> mapM(List<X> xs, Function<X, Flow<S, Y>> f) {
        Flow<S, List<Y>> result = pure(new ArrayList<>());
        for (X x : xs) {
            result = bind(result, ys -> map(f.apply(x), y -> {
                ys.add(y); // Modify in place
                return ys;
            }));
        }
        return result;
    }

    static <S, K1, V1, K2, V2> Flow<S, Map<K2, V2>> mapM(Map<K1, V1> xs, Function<K1, Flow<S, K2>> kf, Function<V1, Flow<S, V2>> vf) {
        Set<Map.Entry<K1, V1>> entries1 = xs.entrySet();
        Flow<S, Set<Map.Entry<K2, V2>>> entries2 = mapM(entries1,
                e -> bind(kf.apply(e.getKey()), k2 -> map(vf.apply(e.getValue()), v2 -> new AbstractMap.SimpleEntry<>(k2, v2))));
        return map(entries2, entries -> entries.stream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
    }

    static <S, X, Y> Flow<S, Optional<Y>> mapM(Optional<X> xs, Function<X, Flow<S, Y>> f) {
        return xs.map(x -> map(f.apply(x), Optional::of)).orElseGet(() -> pure(Optional.empty()));
    }

    static <S, X, Y> Flow<S, Set<Y>> mapM(Set<X> xs, Function<X, Flow<S, Y>> f) {
        Flow<S, Set<Y>> result = pure(Collections.emptySet());
        for (X x : xs) {
            result = bind(result, ys -> map(f.apply(x), y -> {
                ys.add(y); // Modify in place
                return ys;
            }));
        }
        return result;
    }

    static <S, X, Y, Z> Flow<S, Z> map2(Flow<S, X> x, Flow<S, Y> y, BiFunction<X, Y, Z> f) {
        return Flows.bind(x, x1 -> Flows.bind(y, y1 -> Flows.pure(f.apply(x1, y1))));
    }

    static <S, X> Flow<S, X> pure(X obj) {
        return new Flow<>(s -> trace -> new FlowState<>(Optional.of(obj), s, trace));
    }

    // Note: for lack of a unit value other than null, we use use a boolean as the ignorable value output of putState()
    static <S> Flow<S, Boolean> putState(S snew) {
        return new Flow<>(s0 -> t0 -> new FlowState<>(Optional.of(true), snew, t0));
    }

    static <S, X> Flow<S, X> unexpected(String cat, Object obj) {
        return fail("expected " + cat + " but found: " + obj);
    }
}
