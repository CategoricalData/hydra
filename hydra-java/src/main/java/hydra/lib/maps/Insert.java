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

public class Insert<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/maps.insert");
    }

    @Override
    public Type<A> type() {
        return lambda("k", "v", function("k", "v", map("k", "v"), map("k", "v")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Term<A> key = args.get(0);
            Term<A> value = args.get(1);
            return Flows.map(Expect.map(Flows::pure, Flows::pure, args.get(2)), before -> Terms.map(apply(key, value, before)));
        };
    }

    public static <K, V> Function<V, Function<Map<K, V>, Map<K, V>>> apply(K k) {
        return v -> apply(k, v);
    }

    public static <K, V> Function<Map<K, V>, Map<K, V>> apply(K k, V v) {
        return before -> apply(k, v, before);
    }

    public static <K, V> Map<K, V> apply(K k, V v, Map<K, V> before) {
        Map<K, V> after = new HashMap<>(before);
        before.put(k, v);
        return after;
    }
}
