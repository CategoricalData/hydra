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
    public Name name() {
        return new Name("hydra.lib.maps.member");
    }

    @Override
    public TypeScheme type() {
        return scheme("k", "v",
                function("k", hydra.dsl.Types.map("k", "v"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.map(Flows::pure, Flows::pure, args.get(1)),
                (Function<Map<Term, Term>, Term>) mp -> Terms.boolean_(mp.containsKey(args.get(0))));
    }

    /**
     * Checks if the element is present.
     * @param key the element
     * @return true if present, false otherwise
     */
        public static <K, V> Function<Map<K, V>, Boolean> apply(K key) {
        return mp -> apply(key, mp);
    }

    /**
     * Checks if the element is present.
     * @param key the element
     * @param mp the set
     * @return true if present, false otherwise
     */
        public static <K, V> Boolean apply(K key, Map<K, V> mp) {
        return mp.containsKey(key);
    }
}
