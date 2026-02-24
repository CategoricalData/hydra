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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Modifies a key's value.
 */
public class Alter extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.alter");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("v", "k",
                function(function(optional("v"), optional("v")), Types.var("k"), map("k", "v"), map("k", "v")));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Flows.<Graph>getState(), graph ->
            bind(Expect.map(Flows::pure, Flows::pure, args.get(2)), mp -> {
                Term f = args.get(0);
                Term key = args.get(1);
                Function<Maybe<Term>, Maybe<Term>> nativeF = opt -> {
                    Term reduced = Flows.fromFlow(graph,
                        hydra.reduction.Reduction.reduceTerm(true, Terms.apply(f, Terms.optional(opt))));
                    return Flows.fromFlow(graph, Expect.optional(Flows::pure, reduced));
                };
                return pure(Terms.map(Alter.apply(nativeF, key, mp)));
            }));
    }

    /**
     * Applies a function to modify the entry.
     * @param <K> the key type
     * @param <V> the value type
     * @param f the function to apply to the optional value
     * @return a curried function that takes a key and a map and returns the modified map
     */
    public static <K, V> Function<K, Function<Map<K, V>, Map<K, V>>> apply(
            Function<Maybe<V>, Maybe<V>> f) {
        return key -> mp -> apply(f, key, mp);
    }

    /**
     * Applies a function to modify the entry.
     * @param <K> the key type
     * @param <V> the value type
     * @param f the function to apply to the optional value
     * @param key the key to modify
     * @param mp the map to alter
     * @return the modified map
     */
    public static <K, V> Map<K, V> apply(Function<Maybe<V>, Maybe<V>> f, K key, Map<K, V> mp) {
        Map<K, V> result = FromList.orderedMap(mp);
        Maybe<V> currentValue = Lookup.apply(key, mp);
        Maybe<V> newValue = f.apply(currentValue);
        if (newValue.isJust()) {
            result.put(key, newValue.fromJust());
        } else {
            result.remove(key);
        }
        return result;
    }
}
