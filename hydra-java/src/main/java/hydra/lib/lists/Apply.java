package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Applies a function in a flow context.
 */
public class Apply extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.apply");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(list(function("a", "b")), list("a"), list("b")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(0)), functions ->
                hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(1)), arguments -> {
                    List<Term> results = new LinkedList<>();
                    for (Term f : functions) {
                        for (Term a : arguments) {
                            Either<InContext<OtherError>, Term> r = hydra.reduction.Reduction.reduceTerm(
                                new hydra.context.Context(java.util.List.of(), java.util.List.of(), java.util.Map.of()), graph, true, Terms.apply(f, a));
                            if (r.isLeft()) return (Either) r;
                            results.add(((Either.Right<InContext<OtherError>, Term>) r).value);
                        }
                    }
                    return Either.right(Terms.list(results));
                }));
    }

    /**
     * Applies a function within a flow.
     * @param <X> the input type
     * @param <Y> the output type
     * @param functions the list of functions to apply
     * @return a function that applies all functions to all arguments
     */
    public static <X, Y> Function<List<X>, List<Y>> apply(List<Function<X, Y>> functions) {
        return (args) -> apply(functions, args);
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the input type
     * @param <Y> the output type
     * @param functions the list of functions to apply
     * @param args the list of arguments
     * @return the list of results from applying each function to each argument
     */
    public static <X, Y> List<Y> apply(List<Function<X, Y>> functions, List<X> args) {
        List<Y> results = new LinkedList<>();
        for (Function<X, Y> f : functions) {
            for (X a : args) {
                results.add(f.apply(a));
            }
        }
        return results;
    }
}
