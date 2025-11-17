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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Creates a list of n copies.
 */
public class Replicate extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.replicate");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), Types.var("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.int32(args.get(0)), n -> {
            List<Term> result = new ArrayList<>(Collections.nCopies(n, args.get(1)));
            return pure(Terms.list(result));
        });
    }

    /**
     * Replicates a value n times.
     * @param <X> the element type
     * @param n the number of copies to create
     * @return a function that replicates an element n times
     */
    public static <X> Function<X, List<X>> apply(Integer n) {
        return elem -> apply(n, elem);
    }

    /**
     * Replicates a value n times.
     * @param <X> the element type
     * @param n the number of copies to create
     * @param elem the element to replicate
     * @return a list containing n copies of the element
     */
    public static <X> List<X> apply(Integer n, X elem) {
        return new ArrayList<>(Collections.nCopies(n, elem));
    }
}
