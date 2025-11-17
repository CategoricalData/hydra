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

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Checks if an element is in a set.
 */
public class Member extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.maps.member");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function(Types.var("k"), hydra.dsl.Types.map("k", "v"), boolean_()));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.map(Flows::pure, Flows::pure, args.get(1)),
                (Function<Map<Term, Term>, Term>) mp -> Terms.boolean_(mp.containsKey(args.get(0))));
    }

    /**
     * Checks if the element is present.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key to check
     * @return a function that takes a map and returns true if the key is present
     */
    public static <K, V> Function<Map<K, V>, Boolean> apply(K key) {
        return mp -> apply(key, mp);
    }

    /**
     * Checks if the element is present.
     * @param <K> the key type
     * @param <V> the value type
     * @param key the key to check
     * @param mp the map to search
     * @return true if present, false otherwise
     */
    public static <K, V> Boolean apply(K key, Map<K, V> mp) {
        return mp.containsKey(key);
    }
}
