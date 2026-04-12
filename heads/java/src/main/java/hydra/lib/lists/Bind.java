package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Monadic bind for flows.
 */
public class Bind extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.bind");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list("a"), function(Types.var("a"), list("b")), list("b")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(0)), argsArg -> {
                Term mapping = args.get(1);
                List<Term> allResults = new ArrayList<>();
                for (Term a : argsArg) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyContext(), graph, true, Terms.apply(mapping, a));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, List<Term>> inner = hydra.extract.Core.list(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (inner.isLeft()) return (Either) inner;
                    allResults.addAll(((Either.Right<Error_, List<Term>>) inner).value);
                }
                return Either.right(Terms.list(allResults));
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
        ArrayList<Y> all = new ArrayList<>();
        for (X x : args) {
            for (Y y : mapping.apply(x)) {
                all.add(y);
            }
        }
        return all;
    }
}
