package hydra.lib.optionals;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import hydra.util.Opt;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.optional;


public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.bind");
    }

    @Override
    public Type<A> type() {
        return lambda("a", lambda("b",
                function(optional("a"), function("a", optional("b")), optional("b"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)),
            arg -> arg.map(term -> Terms.optional(Opt.of(Terms.apply(args.get(1), term))))
                .orElseGet(() -> Terms.optional(Opt.empty())));
    }

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
