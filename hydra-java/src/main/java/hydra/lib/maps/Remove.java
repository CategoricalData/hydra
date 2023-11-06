package hydra.lib.maps;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.map;

public class Remove<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/maps.remove");
    }

    @Override
    public Type<A> type() {
        return lambda("k", "v",
                function("k", map("k", "v"), map("k", "v")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Term<A> key = args.get(0);
            return Flows.map(Expect.map(Flows::pure, Flows::pure, args.get(2)),
                    before -> Terms.map(apply(key, before)));
        };
    }

    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(K k) {
        return before -> apply(k, before);
    }

    /**
     * Apply the function to both arguments.
     */
    public static <K, V> Map<K, V> apply(K k, Map<K, V> before) {
        Map<K, V> after = new HashMap<>(before);
        after.remove(k);
        return after;
    }
}
