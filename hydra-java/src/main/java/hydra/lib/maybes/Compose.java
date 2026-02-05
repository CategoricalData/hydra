package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Composes two Maybe-returning functions.
 */
public class Compose extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.compose"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.compose");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for composing two optional-returning functions
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c",
                function(function("a", optional("b")),
                        function("b", optional("c")),
                        function("a", optional("c"))));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that composes two Maybe-returning functions
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term f = args.get(0);  // a -> Maybe b
            Term g = args.get(1);  // b -> Maybe c
            Term x = args.get(2);  // a
            // Apply f(x) and reduce to get Maybe b
            return Flows.bind(hydra.reduction.Reduction.reduceTerm(true, Terms.apply(f, x)), fResult ->
                Flows.bind(Expect.optional(Flows::pure, fResult), maybeFResult -> {
                    if (maybeFResult.isJust()) {
                        // Apply g(b) and reduce to get Maybe c
                        return hydra.reduction.Reduction.reduceTerm(true, Terms.apply(g, maybeFResult.fromJust()));
                    } else {
                        return Flows.pure(Terms.optional(Maybe.nothing()));
                    }
                }));
        };
    }

    /**
     * Composes two Maybe-returning functions. Curried version.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @return a function that takes the second function and returns the composed function
     */
    public static <A, B, C> Function<Function<B, Maybe<C>>, Function<A, Maybe<C>>> apply(Function<A, Maybe<B>> left) {
        return right -> apply(left, right);
    }

    /**
     * Composes two Maybe-returning functions.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @param right the second function to apply
     * @return a composed function that applies left then right, returning empty if either returns empty
     */
    public static <A, B, C> Function<A, Maybe<C>> apply(Function<A, Maybe<B>> left, Function<B, Maybe<C>> right) {
        return a -> {
            Maybe<B> ob = left.apply(a);
            return ob.isJust() ? right.apply(ob.fromJust()) : Maybe.nothing();
        };
    }

    /**
     * Composes two Maybe-returning functions and applies to a value.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @param right the second function to apply
     * @param a the value to apply the composed function to
     * @return the result of applying the composed function to the value
     */
    public static <A, B, C> Maybe<C> apply(Function<A, Maybe<B>> left, Function<B, Maybe<C>> right, A a) {
        return apply(left, right).apply(a);
    }
}
