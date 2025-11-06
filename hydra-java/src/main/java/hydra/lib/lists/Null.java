package hydra.lib.lists;

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
import java.util.function.Function;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;

/**
 * Checks if a list is empty.
 */
public class Null extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.null");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), int32()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.list(Flows::pure, args.get(0)), l -> Terms.boolean_(apply(l)));
    }

    /**
     * Checks if the list is empty.
     * @param list the list
     * @return true if empty, false otherwise
     */
        public static <X> boolean apply(List<X> list) {
        return list.isEmpty();
    }
}
