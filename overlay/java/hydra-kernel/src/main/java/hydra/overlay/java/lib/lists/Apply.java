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
 * Applies a function in a flow context.
 */
public class Apply extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.apply().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(list(function("a", "b")), list("a"), list("b")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(0)), functions ->
                hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(1)), arguments -> {
                    ConsList<Term> reversed = ConsList.empty();
                    for (Term f : functions) {
                        for (Term a : arguments) {
                            Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                                hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, a));
                            if (r.isLeft()) return (Either) r;
                            reversed = ConsList.cons(((Either.Right<Error_, Term>) r).value, reversed);
                        }
                    }
                    return Either.right(Terms.list(reversed.reverse()));
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
        ConsList<Y> reversed = ConsList.empty();
        for (Function<X, Y> f : functions) {
            for (X a : args) {
                reversed = ConsList.cons(f.apply(a), reversed);
            }
        }
        return reversed.reverse();
    }
}
