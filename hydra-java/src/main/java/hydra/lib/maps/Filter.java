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
import java.util.function.Predicate;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;


/**
 * Filters map entries by value.
 */
public class Filter extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.filter");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(function("v", boolean_()), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term pred = args.get(0);
            return bind(Expect.map(Flows::pure, Flows::pure, args.get(1)), mp -> {
                // Evaluate predicate on each value, keeping entries where predicate returns true
                Flow<Graph, Map<Term, Term>> resultFlow = pure(new HashMap<>());
                for (Map.Entry<Term, Term> entry : mp.entrySet()) {
                    Term application = Terms.apply(pred, entry.getValue());
                    final Term key = entry.getKey();
                    final Term value = entry.getValue();
                    resultFlow = bind(resultFlow, acc ->
                        bind(hydra.reduction.Reduction.reduceTerm(true, application), reduced ->
                            bind(Expect.boolean_(reduced), b -> {
                                if (b) {
                                    acc.put(key, value);
                                }
                                return pure(acc);
                            })));
                }
                return Flows.map(resultFlow, Terms::map);
            });
        };
    }

    /**
     * Filters entries where values match predicate.
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the predicate to test values
     * @return a function that takes a map and returns the filtered map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(Predicate<V> pred) {
        return mp -> apply((Function<V, Boolean>) v -> pred.test(v), mp);
    }

    /**
     * Filters entries where values match predicate (as Function).
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the predicate as a Function (used by generated code)
     * @return a function that takes a map and returns the filtered map
     */
    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(Function<V, Boolean> pred) {
        return mp -> apply(pred, mp);
    }

    /**
     * Filters entries where values match predicate.
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the predicate to test values
     * @param mp the map to filter
     * @return the filtered map
     */
    public static <K, V> Map<K, V> apply(Predicate<V> pred, Map<K, V> mp) {
        return apply((Function<V, Boolean>) v -> pred.test(v), mp);
    }

    /**
     * Filters entries where values match predicate (as Function).
     * @param <K> the key type
     * @param <V> the value type
     * @param pred the predicate as a Function (used by generated code)
     * @param mp the map to filter
     * @return the filtered map
     */
    public static <K, V> Map<K, V> apply(Function<V, Boolean> pred, Map<K, V> mp) {
        Map<K, V> result = new java.util.LinkedHashMap<>();
        for (Map.Entry<K, V> entry : mp.entrySet()) {
            if (pred.apply(entry.getValue())) {
                result.put(entry.getKey(), entry.getValue());
            }
        }
        return FromList.orderedMap(result);
    }
}
