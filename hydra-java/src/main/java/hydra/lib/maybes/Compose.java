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
    public Name name() {
        return new Name("hydra.lib.maybes.compose");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c",
                function(function("a", optional("b")),
                        function("b", optional("c")),
                        function("a", optional("c"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)),
                arg -> arg.map(term -> Terms.optional(Opt.of(Terms.apply(args.get(1), term))))
                        .orElseGet(() -> Terms.optional(Opt.empty())));
    }

    /**
     * Chains two functions.
     * @param left the function1
     * @return the composed function
     */
        public static <A, B, C> Function<Function<B, Opt<C>>, Function<A, Opt<C>>> apply(Function<A, Opt<B>> left) {
        return right -> apply(left, right);
    }

    /**
     * Chains two functions.
     * @param left the function1
     * @param right the function2
     * @return the composed function
     */
        public static <A, B, C> Function<A, Opt<C>> apply(Function<A, Opt<B>> left, Function<B, Opt<C>> right) {
        return a -> {
            Opt<B> ob = left.apply(a);
            return ob.isPresent() ? right.apply(ob.get()) : Opt.empty();
        };
    }
}
