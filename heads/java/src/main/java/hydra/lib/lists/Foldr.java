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
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.ConsList;
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
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(2)), xs -> {
                Term acc = args.get(1);
                // Fold from the right: walk the (reversed) list right-to-left, prepending into a stack
                ConsList<Term> stack = ConsList.empty();
                for (Term x : xs) {
                    stack = ConsList.cons(x, stack);
                }
                // stack now has elements in reverse order; iterating it processes original list from the right
                for (Term x : stack) {
                    Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                        hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(args.get(0), x, acc));
                    if (r.isLeft()) return r;
                    acc = ((Either.Right<Error_, Term>) r).value;
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
        return ConsList.fromList(xs).foldr((x, acc) -> mapping.apply(x).apply(acc), init);
    }
}
