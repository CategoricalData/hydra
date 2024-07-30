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

import java.util.List;
import java.util.Map;
import hydra.util.Opt;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.optional;

public class Lookup extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra/lib/maps.lookup");
    }

    @Override
    public Type type() {
        return lambda("k", "v",
                function("k", map("k", "v"), optional("v")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.map(Flows::pure, Flows::pure, args.get(1)),
                (Function<Map<Term, Term>, Term>) mp -> Terms.optional(apply(args.get(0), mp)));
    }

    public static <K, V> Function<Map<K, V>, Opt<V>> apply(K k) {
        return mp -> apply(k, mp);
    }

    public static <K, V> Opt<V> apply(K k, Map<K, V> mp) {
        V v = mp.get(k);
        return v == null ? Opt.empty() : Opt.of(v);
    }
}
