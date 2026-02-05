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
import hydra.util.Tuple;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;


/**
 * Converts a map to a list of pairs.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.toList");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v", function(
                map("k", "v"),
                list(pair(variable("k"), variable("v")))));
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
                java.util.List<Map.Entry<Term, Term>> entries = new java.util.ArrayList<>(map.entrySet());
                entries.sort((a, b) -> hydra.lib.equality.Compare.compareTerms(a.getKey(), b.getKey()));
                return Terms.list(entries.stream().map(
                    e -> Terms.pair(e.getKey(), e.getValue())).collect(Collectors.toList()));
            });
        };
    }

    /**
     * Apply the function to its single argument.
     * @param <K> the key type
     * @param <V> the value type
     * @param map the map to convert
     * @return a list of key-value pairs
     */
    @SuppressWarnings("unchecked")
    public static <K, V> List<Tuple.Tuple2<K, V>> apply(Map<K, V> map) {
        List<Tuple.Tuple2<K, V>> pairs = new java.util.ArrayList<>(map.size());
        for (Map.Entry<K, V> entry : map.entrySet()) {
            pairs.add(new Tuple.Tuple2<>(entry.getKey(), entry.getValue()));
        }
        if (!pairs.isEmpty()) {
            if (pairs.get(0).object1 instanceof Comparable) {
                pairs.sort((a, b) -> ((Comparable<K>) a.object1).compareTo(b.object1));
            } else if (pairs.get(0).object1 instanceof Term) {
                pairs.sort((a, b) -> hydra.lib.equality.Compare.compareTerms((Term) a.object1, (Term) b.object1));
            }
        }
        return pairs;
    }
}
