package hydra.lib.maps;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.map;

public class Empty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/maps.empty");
    }

    @Override
    public Type<A> type() {
        return lambda("k", "v", map("k", "v"));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return ignored -> Flows.pure(Terms.map(apply()));
    }

    public static <K, V> Map<K, V> apply() {
        return Collections.emptyMap();
    }
}
