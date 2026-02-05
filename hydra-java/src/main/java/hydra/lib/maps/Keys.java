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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;

/**
 * Returns all keys.
 */
public class Keys extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.keys");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(
                        map("k", "v"),
                        list("k")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Flow<Graph, Map<Term, Term>> r = Expect.map(Flows::pure, Flows::pure, args.get(0));
            return Flows.map(r, map -> {
                List<Term> keys = new ArrayList<>(map.keySet());
                keys.sort(hydra.lib.equality.Compare::compareTerms);
                return Terms.list(keys);
            });
        };
    }

    /**
     * Returns the list of keys.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map
     * @return the keys
     */
    @SuppressWarnings("unchecked")
    public static <K, V> List<K> apply(Map<K, V> map) {
        List<K> keys = new ArrayList<>(map.keySet());
        if (!keys.isEmpty()) {
            if (keys.get(0) instanceof Comparable) {
                keys.sort((a, b) -> ((Comparable<K>) a).compareTo(b));
            } else if (keys.get(0) instanceof Term) {
                keys.sort((a, b) -> hydra.lib.equality.Compare.compareTerms((Term) a, (Term) b));
            }
        }
        return keys;
    }
}
