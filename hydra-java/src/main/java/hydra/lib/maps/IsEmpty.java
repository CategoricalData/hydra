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

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;

/**
 * Checks if a map is empty.
 */
public class IsEmpty extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maps.isEmpty");
    }

    @Override
    public TypeScheme type() {
        return scheme("k", "v", function(
                map("k", "v"),
                boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Flow<Graph, Map<Term, Term>> r = Expect.map(Flows::pure, Flows::pure, args.get(0));
            return Flows.map(r, map -> Terms.boolean_(apply(map)));
        };
    }

    /**
     * Checks if the map is empty.
     * @param map the map
     * @return true if empty, false otherwise
     */
        public static <K, V> boolean apply(Map<K, V> map) {
        return map.isEmpty();
    }
}
