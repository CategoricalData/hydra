package hydra.lib.maps;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;


/**
 * Creates a singleton list.
 */
public class Singleton extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.singleton");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(Types.var("k"), Types.var("v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> pure(Terms.map(Collections.singletonMap(args.get(0), args.get(1))));
    }

    /**
     * Creates a map with one element.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key
     * @return a function that takes a value and returns the singleton map
     */
    public static <K, V> Function<V, Map<K, V>> apply(K key) {
        return value -> apply(key, value);
    }

    /**
     * Creates a map with one element.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key
     * @param value the value
     * @return the singleton map
     */
    public static <K, V> Map<K, V> apply(K key, V value) {
        return Collections.singletonMap(key, value);
    }
}
