package hydra.core.overlay.java.lib.maps;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;

/**
 * Returns all keys.
 */
public class Keys extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Maps.keys().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return Types.constrained2("k", Types.ORD, "v", Types.NONE,
                function(
                        map("k", "v"),
                        list("k")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Either<Error_, Map<Term, Term>> r = hydra.core.overlay.java.lib.OrderKeys.map(graph, args.get(0));
            // Listed in the order of the keys' order keys, so union variants come out in declared order (#848)
            return hydra.core.overlay.java.lib.eithers.Bind.apply(r, m -> hydra.core.overlay.java.lib.eithers.Map.apply(entries -> {
                List<Term> result = new java.util.ArrayList<>(entries.size());
                for (Map.Entry<Term, Term> e : entries) {
                    result.add(e.getKey());
                }
                return Terms.list(result);
            }, hydra.core.overlay.java.lib.OrderKeys.sortedEntries(graph, m)));
        };
    }

    /**
     * Returns the list of keys.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map
     * @return the keys
     */
    public static <K, V> List<K> apply(Map<K, V> map) {
        ConsList<K> reversed = ConsList.empty();
        for (K k : map.keySet()) {
            reversed = ConsList.cons(k, reversed);
        }
        return reversed.reverse();
    }
}
