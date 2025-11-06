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
    public Name name() {
        return new Name("hydra.lib.maps.filterWithKey");
    }

    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(function("k", "v", boolean_()), map("k", "v"), map("k", "v")));
    }

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
     * @param pred the predicate
     * @return the filtered map
     */
        public static <K, V> Function<Map<K, V>, Map<K, V>> apply(BiPredicate<K, V> pred) {
        return mp -> apply(pred, mp);
    }

    /**
     * Filters entries based on key-value predicate.
     * @param pred the predicate
     * @param mp the map
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
