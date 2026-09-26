package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Left-associative fold of a list with a binary function and initial value.
 */
public class Foldl extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.foldl().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(2)), xs -> {
                Term acc = args.get(1);
                for (Term x : xs) {
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), acc, x));
                    if (r.isLeft()) return r;
                    acc = ((Either.Right<Error_, Term>) r).value;
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
    public static <X, Y> Function<Y, Function<List<X>, Y>> apply(Function<Y, Function<X, Y>> mapping) {
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
    public static <X, Y> Function<List<X>, Y> apply(Function<Y, Function<X, Y>> mapping, Y init) {
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
    public static <X, Y> Y apply(Function<Y, Function<X, Y>> mapping, Y init, List<X> xs) {
        Y acc = init;
        for (X x : xs) {
            acc = mapping.apply(acc).apply(x);
        }
        return acc;
    }
}
