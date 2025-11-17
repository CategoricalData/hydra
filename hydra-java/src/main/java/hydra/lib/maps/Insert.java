package hydra.lib.maps;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;

/**
 * Adds an element to a set.
 */
public class Insert extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.insert");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(Types.var("k"), Types.var("v"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term key = args.get(0);
            Term value = args.get(1);
            return Flows.map(Expect.map(Flows::pure, Flows::pure, args.get(2)),
                    before -> Terms.map(apply(key, value, before)));
        };
    }

    /**
     * Adds an element.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to insert
     * @return a curried function that takes a value, a map, and returns the updated map
     */
    public static <K, V> Function<V, Function<Map<K, V>, Map<K, V>>> apply(K k) {
        return v -> apply(k, v);
    }

    /**
     * Adds an element.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to insert
     * @param v the value to insert
     * @return a function that takes a map and returns the updated map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(K k, V v) {
        return before -> apply(k, v, before);
    }

    /**
     * Apply the function to all three arguments.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to insert
     * @param v the value to insert
     * @param before the map to insert into
     * @return the updated map
     */
    public static <K, V> Map<K, V> apply(K k, V v, Map<K, V> before) {
        Map<K, V> after = new HashMap<>(before);
        after.put(k, v);
        return after;
    }
}
