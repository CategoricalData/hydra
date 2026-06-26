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
 * Monadic bind for flows.
 */
public class Bind extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.bind().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list("a"), function(Types.var("a"), list("b")), list("b")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(0)), argsArg -> {
                Term mapping = args.get(1);
                ConsList<Term> reversed = ConsList.empty();
                for (Term a : argsArg) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(mapping, a));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, List<Term>> inner = hydra.extract.Core.list(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (inner.isLeft()) return (Either) inner;
                    for (Term y : ((Either.Right<Error_, List<Term>>) inner).value) {
                        reversed = ConsList.cons(y, reversed);
                    }
                }
                return Either.right(Terms.list(reversed.reverse()));
            });
    }

    /**
     * Chains flow computations.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param args the list of input values
     * @return a function that takes a mapping function and returns the result list
     */
    public static <X, Y> Function<Function<X, List<Y>>, List<Y>> apply(List<X> args) {
        return (mapping) -> apply(args, mapping);
    }

    /**
     * Chains flow computations.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param args the list of input values
     * @param mapping the function to apply to each element
     * @return the flattened result list
     */
    public static <X, Y> List<Y> apply(List<X> args, Function<X, List<Y>> mapping) {
        ConsList<Y> reversed = ConsList.empty();
        for (X x : args) {
            for (Y y : mapping.apply(x)) {
                reversed = ConsList.cons(y, reversed);
            }
        }
        return reversed.reverse();
    }
}
