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
import hydra.util.Maybe;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;

/**
 * Looks up a value by key.
 */
public class Lookup extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.lookup");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(Types.var("k"), map("k", "v"), optional("v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.map(Flows::pure, Flows::pure, args.get(1)),
                (Function<Map<Term, Term>, Term>) mp -> Terms.optional(apply(args.get(0), mp)));
    }

    /**
     * Retrieves the value for the key.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to look up
     * @return a function that takes a map and returns an optional value
     */
    public static <K, V> Function<Map<K, V>, Maybe<V>> apply(K k) {
        return mp -> apply(k, mp);
    }

    /**
     * Retrieves the value for the key.
     * @param <K> the key type
     * @param <V> the value type
     * @param k the key to look up
     * @param mp the map to search
     * @return an optional containing the value if found, or empty if not found
     */
    public static <K, V> Maybe<V> apply(K k, Map<K, V> mp) {
        V v = mp.get(k);
        return v == null ? Maybe.nothing() : Maybe.just(v);
    }
}
