package hydra.lib.maps;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.util.Tuple;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.variable;


public class ToList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/maps.toList");
    }

    @Override
    public Type<A> type() {
        return lambda("k", "v", function(
                map("k", "v"),
                list(pair(variable("k"), variable("v")))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Flow<Graph<A>, Map<Term<A>, Term<A>>> r = Expect.map(Flows::pure, Flows::pure, args.get(0));
            return Flows.map(r, map -> Terms.list(apply(map).stream().map(Terms::pair).collect(Collectors.toList())));
        };
    }

    /**
     * Apply the function to its single argument.
     */
    public static <K, V> List<Tuple.Tuple2<K, V>> apply(Map<K, V> map) {
        List<Tuple.Tuple2<K, V>> pairs = new java.util.ArrayList<>(map.size());
        for (Map.Entry<K, V> entry : map.entrySet()) {
            pairs.add(new Tuple.Tuple2<>(entry.getKey(), entry.getValue()));
        }
        return pairs;
    }
}
