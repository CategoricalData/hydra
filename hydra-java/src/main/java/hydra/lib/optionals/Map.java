package hydra.lib.optionals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import hydra.util.Opt;
import java.util.function.Function;

import static hydra.Flows.bind;
import static hydra.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.optional;


public class Map extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra/lib/optionals.map");
    }

    @Override
    public Type type() {
        return lambda("a", lambda("b",
                function(function("a", "b"), optional("a"), optional("b"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(instance -> pure(Terms.apply(args.get(0), instance)), args.get(1)),
            opt -> pure(Terms.optional(opt)));
    }

    public static <X, Y> Function<Opt<X>, Opt<Y>> apply(Function<X, Y> f) {
        return (optionalArg) -> apply(f, optionalArg);
    }

    /**
     * Apply the function to both arguments.
     */
    public static <X, Y> Opt<Y> apply(Function<X, Y> f, Opt<X> optionalArg) {
        if (!optionalArg.isPresent()) {
            return Opt.empty();
        }

        X arg = optionalArg.get();

        return Opt.of(f.apply(arg));
    }
}
