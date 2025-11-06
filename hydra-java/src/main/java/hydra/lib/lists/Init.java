package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Returns all elements except the last.
 */
public class Init extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.init");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)),
            (Function<List<Term>, Term>) lst -> Terms.list(apply(lst)));
    }

    /**
     * Returns all but the last element.
     * @param lst the list
     * @return the initial elements
     */
        public static <X> List<X> apply(List<X> lst) {
        if (lst.isEmpty()) {
            throw new IllegalArgumentException("init: empty list");
        }
        return lst.subList(0, lst.size() - 1);
    }
}
