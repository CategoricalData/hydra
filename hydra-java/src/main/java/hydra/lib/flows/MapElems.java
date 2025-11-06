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
 * Maps a flow function over map values.
 */
public class MapElems extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.mapElems");
    }

    @Override
    public TypeScheme type() {
        return new TypeScheme(
                Arrays.asList(new Name("s"), new Name("k"), new Name("v1"), new Name("v2")),
                Types.function(
                        Types.function("v1", Types.flow("s", "v2")),
                        Types.map("k", "v1"),
                        Types.flow("s", Types.map("k", "v2"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(1)), map -> {
            Map<Term, Flow<Graph, Term>> mappedValues = new HashMap<>();
            for (Map.Entry<Term, Term> entry : map.entrySet()) {
                mappedValues.put(entry.getKey(),
                    pure(Terms.apply(args.get(0), entry.getValue())));
            }
            // Sequence the map
            Flow<Graph, Map<Term, Term>> result = pure(new HashMap<>());
            for (Map.Entry<Term, Flow<Graph, Term>> entry : mappedValues.entrySet()) {
                result = bind(result, m -> bind(entry.getValue(), v -> {
                    m.put(entry.getKey(), v);
                    return pure(m);
                }));
            }
            return bind(result, r -> pure(Terms.map(r)));
        });
    }

    /**
     * Transforms map values with flow.
     * @return the flow of results
     */
        public static <S, K, V1, V2> Function<Map<K, V1>, Flow<S, Map<K, V2>>> apply(
            Function<V1, Flow<S, V2>> f) {
        return map -> apply(f, map);
    }

    /**
     * Transforms map values with flow.
     * @return the flow of results
     */
        public static <S, K, V1, V2> Flow<S, Map<K, V2>> apply(
            Function<V1, Flow<S, V2>> f, Map<K, V1> map) {
        return Flows.mapM(map, k -> pure(k), f);
    }
}
