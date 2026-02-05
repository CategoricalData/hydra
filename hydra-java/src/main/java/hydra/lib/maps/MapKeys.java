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
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;


/**
 * Maps a flow function over map keys.
 */
public class MapKeys extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.mapKeys");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k1", "k2", "v",
            function(function("k1", "k2"), map("k1", "v"), map("k2", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term f = args.get(0);
            return Flows.bind(Expect.map(Flows::pure, Flows::pure, args.get(1)), mp -> {
                Flow<Graph, java.util.Map<Term, Term>> resultFlow = pure(new HashMap<>());
                for (java.util.Map.Entry<Term, Term> entry : mp.entrySet()) {
                    Term application = Terms.apply(f, entry.getKey());
                    final Term value = entry.getValue();
                    resultFlow = Flows.bind(resultFlow, acc ->
                        Flows.map(hydra.reduction.Reduction.reduceTerm(true, application), newKey -> {
                            acc.put(newKey, value);
                            return acc;
                        }));
                }
                return Flows.map(resultFlow, Terms::map);
            });
        };
    }

    /**
     * Transforms map keys with flow.
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V> the value type
     * @param mapping the function to apply to each key
     * @return a function that takes a map and returns the map with transformed keys
     */
    public static <K1, K2, V> Function<java.util.Map<K1, V>, java.util.Map<K2, V>> apply(Function<K1, K2> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Apply the function to both arguments.
     * @param <K1> the input key type
     * @param <K2> the output key type
     * @param <V> the value type
     * @param mapping the function to apply to each key
     * @param arg the input map
     * @return the map with transformed keys
     */
    public static <K1, K2, V> java.util.Map<K2, V> apply(Function<K1, K2> mapping, java.util.Map<K1, V> arg) {
        java.util.Map<K2, V> result = new java.util.LinkedHashMap<>();
        for (java.util.Map.Entry<K1, V> e : arg.entrySet()) {
            result.put(mapping.apply(e.getKey()), e.getValue());
        }
        return FromList.orderedMap(result);
    }
}
