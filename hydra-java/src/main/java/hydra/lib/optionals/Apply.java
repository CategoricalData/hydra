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
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.Flows.*;
import static hydra.dsl.Types.*;


public class Apply<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.apply");
    }

    @Override
    public Type<A> type() {
        return lambda("x", lambda("y",
            function(optional(function("x", "y")), optional("x"), optional("y"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map2(Expect.optional(Flows::pure, args.get(0)), Expect.optional(Flows::pure, args.get(1)),
            (BiFunction<Optional<Term<A>>, Optional<Term<A>>, Term<A>>) (optionalF, optionalArg) ->
                (optionalF.isPresent() && optionalArg.isPresent())
                    ? Terms.optional(Optional.of(Terms.apply(optionalF.get(), optionalArg.get())))
                    : Terms.optional(Optional.empty()));
    }

    public static <X, Y> Function<Optional<X>, Optional<Y>> apply(Optional<Function<X, Y>> optionalF) {
        return (optionalArg) -> apply(optionalF, optionalArg);
    }

    public static <X, Y> Optional<Y> apply(Optional<Function<X, Y>> optionalF, Optional<X> optionalArg) {
        if (!optionalF.isPresent() || !optionalArg.isPresent()) {
            return Optional.empty();
        }

        Function<X, Y> f = optionalF.get();
        X arg = optionalArg.get();

        return Optional.of(f.apply(arg));
    }
}
