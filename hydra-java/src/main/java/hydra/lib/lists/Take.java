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

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Takes the first n elements.
 */
public class Take extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.take");
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
     * Takes the first n elements.
     * @param <X> the element type
     * @param n the number of elements to take
     * @return a function that takes the first n elements from a list
     */
    public static <X> Function<List<X>, List<X>> apply(Integer n) {
        return lst -> apply(n, lst);
    }

    /**
     * Takes the first n elements.
     * @param <X> the element type
     * @param n the number of elements to take
     * @param lst the list to take from
     * @return the sublist containing the first n elements
     */
    public static <X> List<X> apply(Integer n, List<X> lst) {
        if (n <= 0) {
            return List.of();
        }
        if (n >= lst.size()) {
            return lst;
        }
        return lst.subList(0, n);
    }
}
