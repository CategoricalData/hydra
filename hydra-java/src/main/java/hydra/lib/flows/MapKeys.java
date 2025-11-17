package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;


/**
 * Maps a flow function over map keys.
 */
public class MapKeys extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.mapKeys");
    }

    @Override
    public TypeScheme type() {
        return new TypeScheme(
                Arrays.asList(new Name("s"), new Name("k1"), new Name("k2"), new Name("v")),
                Types.function(
                        Types.function("k1", Types.flow("s", "k2")),
                        Types.map("k1", "v"),
                        Types.flow("s", Types.map("k2", "v"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(1)), map -> {
            Map<Flow<Graph, Term>, Term> mappedKeys = new HashMap<>();
            for (Map.Entry<Term, Term> entry : map.entrySet()) {
                mappedKeys.put(pure(Terms.apply(args.get(0), entry.getKey())),
                    entry.getValue());
            }
            // Sequence the map
            Flow<Graph, Map<Term, Term>> result = pure(new HashMap<>());
            for (Map.Entry<Flow<Graph, Term>, Term> entry : mappedKeys.entrySet()) {
                result = bind(result, m -> bind(entry.getKey(), k -> {
                    m.put(k, entry.getValue());
                    return pure(m);
                }));
            }
            return bind(result, r -> pure(Terms.map(r)));
        });
    }

    /**
     * Transforms map keys with flow.
     * @param <S> the state type
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V> the value type
     * @param f the function
     * @return the flow of results
     */
    public static <S, K1, K2, V> Function<Map<K1, V>, Flow<S, Map<K2, V>>> apply(
            Function<K1, Flow<S, K2>> f) {
        return map -> apply(f, map);
    }

    /**
     * Transforms map keys with flow.
     * @param <S> the state type
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V> the value type
     * @param f the function
     * @param map the map
     * @return the flow of results
     */
    public static <S, K1, K2, V> Flow<S, Map<K2, V>> apply(
            Function<K1, Flow<S, K2>> f, Map<K1, V> map) {
        return Flows.mapM(map, f, v -> pure(v));
    }
}
