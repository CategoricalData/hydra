package hydra.lib.maps;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Tuple;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;

import static hydra.dsl.Expect.list;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;

/**
 * Creates a map from a list of pairs.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.fromList");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(list(pair(variable("k"), variable("v"))), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(list(term -> Expect.pair(Flows::pure, Flows::pure, term), args.get(0)),
                (Function<List<Tuple.Tuple2<Term, Term>>, Term>) pairs -> new Term.Map(apply(pairs)));
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param pairs the list of key-value pairs
     * @return a map constructed from the pairs
     */
    @SuppressWarnings("unchecked")
    public static <K, V> Map<K, V> apply(List<Tuple.Tuple2<K, V>> pairs) {
        if (pairs.isEmpty()) {
            return new LinkedHashMap<>();
        }
        // Check first key to determine map type
        K firstKey = pairs.get(0).object1;
        Map<K, V> mp;
        if (firstKey instanceof Comparable) {
            try {
                mp = new TreeMap<>((a, b) -> ((Comparable<K>) a).compareTo(b));
            } catch (ClassCastException e) {
                mp = new LinkedHashMap<>();
            }
        } else {
            mp = new LinkedHashMap<>();
        }
        for (Tuple.Tuple2<K, V> pair : pairs) {
            mp.put(pair.object1, pair.object2);
        }
        return mp;
    }

    /**
     * Creates an ordered map from an existing map.
     * Uses TreeMap when keys are Comparable (matching Haskell's Data.Map behavior).
     */
    @SuppressWarnings("unchecked")
    static <K, V> Map<K, V> orderedMap(Map<K, V> source) {
        if (source.isEmpty()) {
            return new LinkedHashMap<>();
        }
        for (K key : source.keySet()) {
            if (key != null) {
                if (key instanceof Comparable) {
                    try {
                        TreeMap<K, V> result = new TreeMap<>((a, b) -> ((Comparable<K>) a).compareTo(b));
                        result.putAll(source);
                        return result;
                    } catch (ClassCastException e) {
                        return new LinkedHashMap<>(source);
                    }
                } else {
                    return new LinkedHashMap<>(source);
                }
            }
        }
        return new LinkedHashMap<>(source);
    }
}
