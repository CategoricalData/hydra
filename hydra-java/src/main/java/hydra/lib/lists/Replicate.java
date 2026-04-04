package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(cx, graph, args.get(0)), n -> {
            List<Term> result = new ArrayList<>(Collections.nCopies(n, args.get(1)));
            return Either.right(Terms.list(result));
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
