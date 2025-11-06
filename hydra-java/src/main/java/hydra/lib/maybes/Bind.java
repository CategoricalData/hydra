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
 * Monadic bind for flows.
 */
public class Bind extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maybes.bind");
    }

    @Override
    public TypeScheme type() {
        return scheme("a","b",
                function(optional("a"), function("a", optional("b")), optional("b")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)),
            arg -> arg.map(term -> Terms.optional(Opt.of(Terms.apply(args.get(1), term))))
                .orElseGet(() -> Terms.optional(Opt.empty())));
    }

    /**
     * Chains flow computations.
     * @param optionalArg the flowValue
     * @return the result flow
     */
        public static <X, Y> Function<Function<X, Opt<Y>>, Opt<Y>> apply(Opt<X> optionalArg) {
        return (f) -> apply(optionalArg, f);
    }

    /**
     * Apply the function to both arguments.
     */
    public static <X, Y> Opt<Y> apply(Opt<X> optionalArg, Function<X, Opt<Y>> f) {
        return optionalArg.isPresent()
            ? f.apply(optionalArg.get())
            : Opt.empty();
    }
}
