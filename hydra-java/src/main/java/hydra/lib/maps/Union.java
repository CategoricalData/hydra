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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;


/**
 * Computes the union of two sets.
 */
public class Union extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.union");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(map("k", "v"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(0)), mp1 ->
            bind(Expect.map(Flows::pure, Flows::pure, args.get(1)), mp2 -> {
                Map<Term, Term> result = new HashMap<>(mp1);
                result.putAll(mp2);
                return pure(Terms.map(result));
            }));
    }

    /**
     * Combines two maps.
     * @param <K> the key type
     * @param <V> the value type
     * @param mp1 the first map
     * @return a function that takes a second map and returns the union
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(Map<K, V> mp1) {
        return mp2 -> apply(mp1, mp2);
    }

    /**
     * Combines two maps.
     * @param <K> the key type
     * @param <V> the value type
     * @param mp1 the first map
     * @param mp2 the second map
     * @return the union of the two maps
     */
    public static <K, V> Map<K, V> apply(Map<K, V> mp1, Map<K, V> mp2) {
        Map<K, V> result = new HashMap<>(mp1);
        result.putAll(mp2);
        return result;
    }
}
