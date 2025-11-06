package hydra.lib.maps;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
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
    public Name name() {
        return new Name("hydra.lib.maps.singleton");
    }

    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function("k", "v", map("k", "v")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> pure(Terms.map(Collections.singletonMap(args.get(0), args.get(1))));
    }

    /**
     * Creates a list with one element.
     * @param key the value
     * @return the singleton list
     */
        public static <K, V> Function<V, Map<K, V>> apply(K key) {
        return value -> apply(key, value);
    }

    /**
     * Creates a list with one element.
     * @param key the value
     * @param value the value
     * @return the singleton list
     */
        public static <K, V> Map<K, V> apply(K key, V value) {
        return Collections.singletonMap(key, value);
    }
}
