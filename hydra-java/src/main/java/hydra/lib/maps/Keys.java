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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;

public class Keys<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/maps.keys");
    }

    @Override
    public Type<A> type() {
        return lambda("k", "v",
                function(
                        map("k", "v"),
                        list("k")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Flow<Graph<A>, Map<Term<A>, Term<A>>> r = Expect.map(Flows::pure, Flows::pure, args.get(0));
            return Flows.map(r, map -> Terms.list(apply(map)));
        };
    }

    public static <K, V> List<K> apply(Map<K, V> map) {
        return new ArrayList<K>(map.keySet());
    }
}
