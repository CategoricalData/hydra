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
import hydra.util.Opt;

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
    public Name name() {
        return new Name("hydra.lib.maps.alter");
    }

    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(function(optional("v"), optional("v")), "k", map("k", "v"), map("k", "v")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.map(Flows::pure, Flows::pure, args.get(2)), mp -> {
            Term key = args.get(1);
            Term f = args.get(0);
            Opt<Term> currentValue = Lookup.apply(key, mp);
            Term newOptValue = Terms.apply(f, Terms.optional(currentValue));
            return bind(Expect.optional(Flows::pure, newOptValue), newValue -> {
                Map<Term, Term> result = new HashMap<>(mp);
                if (newValue.isPresent()) {
                    result.put(key, newValue.get());
                } else {
                    result.remove(key);
                }
                return pure(Terms.map(result));
            });
        });
    }

    /**
     * Applies a function to modify the entry.
     * @return the modified map
     */
        public static <K, V> Function<K, Function<Map<K, V>, Map<K, V>>> apply(
            Function<Opt<V>, Opt<V>> f) {
        return key -> mp -> apply(f, key, mp);
    }

    /**
     * Applies a function to modify the entry.
     * @param f the function
     * @param key the key
     * @param mp the map
     * @return the modified map
     */
        public static <K, V> Map<K, V> apply(Function<Opt<V>, Opt<V>> f, K key, Map<K, V> mp) {
        Map<K, V> result = new HashMap<>(mp);
        Opt<V> currentValue = Lookup.apply(key, mp);
        Opt<V> newValue = f.apply(currentValue);
        if (newValue.isPresent()) {
            result.put(key, newValue.get());
        } else {
            result.remove(key);
        }
        return result;
    }
}
