package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Creates a list of n copies.
 */
public class Replicate extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.replicate().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), Types.var("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), n -> {
            return Either.right(Terms.list(apply(n, args.get(1))));
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
        ConsList<X> result = ConsList.empty();
        for (int i = 0; i < n; i++) {
            result = ConsList.cons(elem, result);
        }
        return result;
    }
}
