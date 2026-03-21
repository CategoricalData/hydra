package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Left-associative fold of a list with a binary function and initial value.
 */
public class Foldl extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.foldl");
    }

    @Override
    public TypeScheme type() {
        // Variables listed in order of first appearance: (b -> a -> b) -> b -> list<a> -> b
        // b appears first, then a
        return scheme("b","a",
                function(function(Types.var("b"), Types.var("a"), Types.var("b")), variable("b"), list("a"),
                        variable("b")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(2)), xs -> {
                Term acc = args.get(1);
                for (Term x : xs) {
                    Either<InContext<Error_>, Term> r = hydra.reduction.Reduction.reduceTerm(
                        hydra.lexical.Lexical.emptyContext(), graph, true, Terms.apply(args.get(0), acc, x));
                    if (r.isLeft()) return r;
                    acc = ((Either.Right<InContext<Error_>, Term>) r).value;
                }
                return Either.right(acc);
            });
    }

    /**
     * Left-associative fold of a list.
     * @param <X> the list element type
     * @param <Y> the accumulator type
     * @param mapping the binary function (accumulator -&gt; element -&gt; accumulator)
     * @return a curried function for folding
     */
    public static <X, Y> Function<Y, Function<ConsList<X>, Y>> apply(Function<Y, Function<X, Y>> mapping) {
        return y -> xs -> apply(mapping, y, xs);
    }

    /**
     * Left-associative fold of a list with an initial value.
     * @param <X> the list element type
     * @param <Y> the accumulator type
     * @param mapping the binary function (accumulator -&gt; element -&gt; accumulator)
     * @param init the initial accumulator value
     * @return a function that takes a list and returns the folded result
     */
    public static <X, Y> Function<ConsList<X>, Y> apply(Function<Y, Function<X, Y>> mapping, Y init) {
        return xs -> apply(mapping, init, xs);
    }

    /**
     * Left-associative fold of a list with a binary function and initial value.
     * Processes elements from left to right: f(f(f(init, x1), x2), x3)...
     * @param <X> the list element type
     * @param <Y> the accumulator type
     * @param mapping the binary function (accumulator -&gt; element -&gt; accumulator)
     * @param init the initial accumulator value
     * @param xs the list to fold
     * @return the final accumulated result
     */
    public static <X, Y> Y apply(Function<Y, Function<X, Y>> mapping, Y init, ConsList<X> xs) {
        return xs.foldl((acc, x) -> mapping.apply(acc).apply(x), init);
    }
}
