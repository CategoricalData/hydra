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
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Applies a function to each element of a list, returning a new list of results.
 */
public class Map extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.map().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(function("a", "b"), list("a"), list("b")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph ->
            hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(1)), lst -> {
                ConsList<Term> reversed = ConsList.empty();
                for (Term x : lst) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    reversed = ConsList.cons(((Either.Right<Error_, Term>) r).value, reversed);
                }
                return Either.right(Terms.list(reversed.reverse()));
            });
    }

    /**
     * Applies a function to each element of a list.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param mapping the function to apply to each element
     * @return a function that maps the function over a list
     */
    public static <X, Y> Function<List<X>, List<Y>> apply(Function<X, Y> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Applies a function to each element of a list, returning a new list of results.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param mapping the function to apply to each element
     * @param arg the list to map over
     * @return a new list containing the results of applying the function to each element
     */
    public static <X, Y> List<Y> apply(Function<X, Y> mapping, List<X> arg) {
        return ConsList.fromList(arg).map(mapping);
    }
}
