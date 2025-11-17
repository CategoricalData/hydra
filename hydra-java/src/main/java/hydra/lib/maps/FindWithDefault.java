package hydra.lib.maps;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Looks up with a default value.
 */
public class FindWithDefault extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.findWithDefault");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function("v", function("k", function(hydra.dsl.Types.map("k", "v"), "v"))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.map(Flows::pure, Flows::pure, args.get(2)),
                (Function<Map<Term, Term>, Term>) mp -> {
                    Opt<Term> result = Lookup.apply(args.get(1), mp);
                    return result.orElse(args.get(0));
                });
    }

    /**
     * Returns the value or default.
     * @param <K> the key type
     * @param <V> the value type
     * @param defaultValue the default value to return if key is not found
     * @return a curried function that takes a key, a map, and returns the value or default
     */
    public static <K, V> Function<K, Function<Map<K, V>, V>> apply(V defaultValue) {
        return key -> mp -> apply(defaultValue, key, mp);
    }

    /**
     * Returns the value or default.
     * @param <K> the key type
     * @param <V> the value type
     * @param defaultValue the default value to return if key is not found
     * @param key the key to look up
     * @param mp the map to search
     * @return the value or default
     */
    public static <K, V> V apply(V defaultValue, K key, Map<K, V> mp) {
        return mp.getOrDefault(key, defaultValue);
    }
}
