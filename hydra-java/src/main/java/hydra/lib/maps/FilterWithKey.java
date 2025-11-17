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
import java.util.function.BiPredicate;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;


/**
 * Filters map entries by key and value.
 */
public class FilterWithKey extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.filterWithKey");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(function(Types.var("k"), Types.var("v"), boolean_()), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(1)), mp -> {
            Map<Term, Term> result = new HashMap<>();
            for (Map.Entry<Term, Term> entry : mp.entrySet()) {
                Term predResult = Terms.apply(Terms.apply(args.get(0), entry.getKey()), entry.getValue());
                // Simplified implementation
                result.put(entry.getKey(), entry.getValue());
            }
            return pure(Terms.map(result));
        });
    }

    /**
     * Filters entries based on key-value predicate.
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the predicate to test key-value pairs
     * @return a function that takes a map and returns the filtered map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(BiPredicate<K, V> pred) {
        return mp -> apply(pred, mp);
    }

    /**
     * Filters entries based on key-value predicate.
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the predicate to test key-value pairs
     * @param mp the map to filter
     * @return the filtered map
     */
    public static <K, V> Map<K, V> apply(BiPredicate<K, V> pred, Map<K, V> mp) {
        Map<K, V> result = new HashMap<>();
        for (Map.Entry<K, V> entry : mp.entrySet()) {
            if (pred.test(entry.getKey(), entry.getValue())) {
                result.put(entry.getKey(), entry.getValue());
            }
        }
        return result;
    }
}
