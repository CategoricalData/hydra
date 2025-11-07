package hydra.lib.lists;

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
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Drops the first n elements.
 */
public class Drop extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.drop");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.int32(args.get(0)), n ->
            Flows.map(Expect.list(Flows::pure, args.get(1)),
                (Function<List<Term>, Term>) lst -> Terms.list(apply(n, lst))));
    }

    /**
     * Drops the first n elements.
     * @param n the n
     * @return the remaining list
     */
        public static <X> Function<List<X>, List<X>> apply(Integer n) {
        return lst -> apply(n, lst);
    }

    /**
     * Drops the first n elements.
     * @param n the n
     * @param lst the list
     * @return the remaining list
     */
        public static <X> List<X> apply(Integer n, List<X> lst) {
        if (n <= 0) {
            return lst;
        }
        if (n >= lst.size()) {
            return List.of();
        }
        return lst.subList(n, lst.size());
    }
}
