package hydra.lib.maps;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;


/**
 * Transforms both keys and values.
 */
public class Bimap extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maps.bimap");
    }

    @Override
    public TypeScheme type() {
        return new TypeScheme(
                Arrays.asList(new Name("k1"), new Name("k2"), new Name("v1"), new Name("v2")),
                function(function("k1", "k2"), function("v1", "v2"), map("k1", "v1"), map("k2", "v2")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(2)), mp -> {
            Map<Term, Term> result = new HashMap<>();
            for (Map.Entry<Term, Term> entry : mp.entrySet()) {
                Term newKey = Terms.apply(args.get(0), entry.getKey());
                Term newValue = Terms.apply(args.get(1), entry.getValue());
                result.put(newKey, newValue);
            }
            return pure(Terms.map(result));
        });
    }

    /**
     * Maps functions over keys and values.
     * @return the transformed map
     */
        public static <K1, K2, V1, V2> Function<Function<V1, V2>, Function<Map<K1, V1>, Map<K2, V2>>> apply(
            Function<K1, K2> kf) {
        return vf -> mp -> apply(kf, vf, mp);
    }

    /**
     * Maps functions over keys and values.
     * @return the transformed map
     */
        public static <K1, K2, V1, V2> Map<K2, V2> apply(
            Function<K1, K2> kf, Function<V1, V2> vf, Map<K1, V1> mp) {
        Map<K2, V2> result = new HashMap<>();
        for (Map.Entry<K1, V1> entry : mp.entrySet()) {
            result.put(kf.apply(entry.getKey()), vf.apply(entry.getValue()));
        }
        return result;
    }
}
