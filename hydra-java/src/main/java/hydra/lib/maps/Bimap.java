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
import hydra.util.Maybe;

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
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.bimap");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return new TypeScheme(
                Arrays.asList(new Name("k1"), new Name("k2"), new Name("v1"), new Name("v2")),
                function(function("k1", "k2"), function("v1", "v2"), map("k1", "v1"), map("k2", "v2")),
                Maybe.nothing());
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(2)), mp -> {
            Term kf = args.get(0);
            Term vf = args.get(1);
            Flow<Graph, Map<Term, Term>> resultFlow = pure(new HashMap<>());
            for (Map.Entry<Term, Term> entry : mp.entrySet()) {
                Term keyApp = Terms.apply(kf, entry.getKey());
                Term valApp = Terms.apply(vf, entry.getValue());
                resultFlow = bind(resultFlow, acc ->
                    bind(hydra.reduction.Reduction.reduceTerm(true, keyApp), newKey ->
                        Flows.map(hydra.reduction.Reduction.reduceTerm(true, valApp), newVal -> {
                            acc.put(newKey, newVal);
                            return acc;
                        })));
            }
            return Flows.map(resultFlow, Terms::map);
        });
    }

    /**
     * Maps functions over keys and values.
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V1> the input value type
     * @param <V2> the output value type
     * @param kf the key transformation function
     * @return a curried function that takes a value function, a map, and returns the transformed map
     */
    public static <K1, K2, V1, V2> Function<Function<V1, V2>, Function<Map<K1, V1>, Map<K2, V2>>> apply(
            Function<K1, K2> kf) {
        return vf -> mp -> apply(kf, vf, mp);
    }

    /**
     * Maps functions over keys and values.
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V1> the input value type
     * @param <V2> the output value type
     * @param kf the key transformation function
     * @param vf the value transformation function
     * @param mp the input map
     * @return the transformed map
     */
    public static <K1, K2, V1, V2> Map<K2, V2> apply(
            Function<K1, K2> kf, Function<V1, V2> vf, Map<K1, V1> mp) {
        Map<K2, V2> result = new java.util.LinkedHashMap<>();
        for (Map.Entry<K1, V1> entry : mp.entrySet()) {
            result.put(kf.apply(entry.getKey()), vf.apply(entry.getValue()));
        }
        return FromList.orderedMap(result);
    }
}
