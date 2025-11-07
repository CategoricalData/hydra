package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Checks if an element is in a list.
 */
public class Elem extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.elem");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(1)),
            (Function<List<Term>, Term>) lst -> Terms.boolean_(lst.contains(args.get(0))));
    }

    /**
     * Checks if the element is present.
     * @param elem the element
     * @return true if present, false otherwise
     */
        public static <X> Function<List<X>, Boolean> apply(X elem) {
        return lst -> apply(elem, lst);
    }

    /**
     * Checks if the element is present.
     * @param elem the element
     * @param lst the list
     * @return true if present, false otherwise
     */
        public static <X> Boolean apply(X elem, List<X> lst) {
        return lst.contains(elem);
    }
}
