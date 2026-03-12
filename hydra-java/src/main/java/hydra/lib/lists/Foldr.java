package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Right-associative fold of a list with a binary function and initial value.
 */
public class Foldr extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.foldr");
    }

    @Override
    public TypeScheme type() {
        // foldr :: (a -> b -> b) -> b -> [a] -> b
        // a appears first (in the function), then b
        return scheme("a","b",
                function(function(Types.var("a"), Types.var("b"), Types.var("b")), variable("b"), list("a"),
                        variable("b")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(2)), xs -> {
                Term acc = args.get(1);
                // Fold from the right by iterating in reverse
                for (int i = xs.size() - 1; i >= 0; i--) {
                    Either<InContext<OtherError>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.monads.Monads.emptyContext(), graph, true, Terms.apply(args.get(0), xs.get(i), acc));
                    if (r.isLeft()) return r;
                    acc = ((Either.Right<InContext<OtherError>, Term>) r).value;
                }
                return Either.right(acc);
            });
    }

    /**
     * Right-associative fold of a list.
     * @param <X> the list element type
     * @param <Y> the accumulator type
     * @param mapping the binary function (element -&gt; accumulator -&gt; accumulator)
     * @return a curried function for folding
     */
    public static <X, Y> Function<Y, Function<List<X>, Y>> apply(Function<X, Function<Y, Y>> mapping) {
        return y -> xs -> apply(mapping, y, xs);
    }

    /**
     * Right-associative fold of a list with an initial value.
     * @param <X> the list element type
     * @param <Y> the accumulator type
     * @param mapping the binary function (element -&gt; accumulator -&gt; accumulator)
     * @param init the initial accumulator value
     * @return a function that takes a list and returns the folded result
     */
    public static <X, Y> Function<List<X>, Y> apply(Function<X, Function<Y, Y>> mapping, Y init) {
        return xs -> apply(mapping, init, xs);
    }

    /**
     * Right-associative fold of a list with a binary function and initial value.
     * Processes elements from right to left: f(x1, f(x2, f(x3, init)))...
     * @param <X> the list element type
     * @param <Y> the accumulator type
     * @param mapping the binary function (element -&gt; accumulator -&gt; accumulator)
     * @param init the initial accumulator value
     * @param xs the list to fold
     * @return the final accumulated result
     */
    public static <X, Y> Y apply(Function<X, Function<Y, Y>> mapping, Y init, List<X> xs) {
        Y cur = init;
        for (int i = xs.size() - 1; i >= 0; i--) {
            cur = mapping.apply(xs.get(i)).apply(cur);
        }
        return cur;
    }
}
