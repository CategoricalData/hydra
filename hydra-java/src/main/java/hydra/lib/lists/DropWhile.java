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
import java.util.function.Predicate;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Drops elements while predicate is true.
 */
public class DropWhile extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.dropWhile");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph ->
            hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(cx, graph, args.get(1)), lst -> {
                ArrayList<Term> indexed = new ArrayList<>(lst);
                int dropCount = 0;
                for (Term x : indexed) {
                    Either<InContext<Error_>, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyContext(), graph, true, Terms.apply(args.get(0), x));
                    if (r.isLeft()) return (Either) r;
                    Either<InContext<Error_>, Boolean> b = hydra.extract.Core.boolean_(cx, graph,
                        ((Either.Right<InContext<Error_>, Term>) r).value);
                    if (b.isLeft()) return (Either) b;
                    if (!((Either.Right<InContext<Error_>, Boolean>) b).value) break;
                    dropCount++;
                }
                return Either.right(Terms.list(new ArrayList<>(indexed.subList(dropCount, indexed.size()))));
            });
    }

    /**
     * Drops elements while the predicate holds.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @return a function that drops elements while the predicate holds
     */
    public static <X> Function<List<X>, List<X>> apply(Predicate<X> pred) {
        return lst -> apply((Function<X, Boolean>) x -> pred.test(x), lst);
    }

    /**
     * Drops elements while the predicate (as Function) holds.
     * @param <X> the element type
     * @param pred the predicate as a Function (used by generated code)
     * @return a function that drops elements while the predicate holds
     */
    public static <X> Function<List<X>, List<X>> apply(Function<X, Boolean> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Drops elements while the predicate holds.
     * @param <X> the element type
     * @param pred the predicate to test elements
     * @param lst the list to drop from
     * @return the remaining list after dropping
     */
    public static <X> List<X> apply(Predicate<X> pred, List<X> lst) {
        return apply((Function<X, Boolean>) x -> pred.test(x), lst);
    }

    /**
     * Drops elements while the predicate (as Function) holds.
     * @param <X> the element type
     * @param pred the predicate as a Function (used by generated code)
     * @param lst the list to drop from
     * @return the remaining list after dropping
     */
    public static <X> List<X> apply(Function<X, Boolean> pred, List<X> lst) {
        int i = 0;
        while (i < lst.size() && pred.apply(lst.get(i))) {
            i++;
        }
        return new ArrayList<>(lst.subList(i, lst.size()));
    }
}
