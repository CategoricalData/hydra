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
import java.util.Optional;
import static hydra.dsl.Types.*;
import static hydra.Flows.*;

public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.map");
    }

    @Override
    public Type<A> type() {
        return lambda("x", lambda("y", function(function("x", "y"), optional("x"), optional("y"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> bind(Expect.optional(instance -> pure(Terms.apply(args.get(0), instance)), args.get(1)),
            opt -> pure(Terms.optional(opt)));
    }

    public static <X, Y> Function<Optional<X>, Optional<Y>> apply(Function<X, Y> f) {
        return (optionalArg) -> apply(f, optionalArg);
    }

    public static <X, Y> Optional<Y> apply(Function<X, Y> f, Optional<X> optionalArg) {
        if (!optionalArg.isPresent()) {
            return Optional.empty();
        }

        X arg = optionalArg.get();

        return Optional.of(f.apply(arg));
    }
}
