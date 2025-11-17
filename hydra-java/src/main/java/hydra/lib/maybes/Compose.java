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
import hydra.util.Opt;

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
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)),
                arg -> arg.map(term -> Terms.optional(Opt.of(Terms.apply(args.get(1), term))))
                        .orElseGet(() -> Terms.optional(Opt.empty())));
    }

    /**
     * Composes two Maybe-returning functions. Curried version.
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the output type
     * @param left the first function to apply
     * @return a function that takes the second function and returns the composed function
     */
    public static <A, B, C> Function<Function<B, Opt<C>>, Function<A, Opt<C>>> apply(Function<A, Opt<B>> left) {
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
    public static <A, B, C> Function<A, Opt<C>> apply(Function<A, Opt<B>> left, Function<B, Opt<C>> right) {
        return a -> {
            Opt<B> ob = left.apply(a);
            return ob.isPresent() ? right.apply(ob.get()) : Opt.empty();
        };
    }
}
