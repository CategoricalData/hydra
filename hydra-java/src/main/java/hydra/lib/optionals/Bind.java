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

public class Bind<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.bind");
    }

    @Override
    public Type<A> type() {
        return lambda("a", lambda("b", function(optional("a"), function("a", optional("b")), optional("b"))));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)),
            optionalArg -> optionalArg.map(aTerm -> Terms.optional(Optional.of(Terms.apply(args.get(1), aTerm))))
                .orElseGet(() -> Terms.optional(Optional.empty())));
    }

    public static <X, Y> Function<Function<X, Optional<Y>>, Optional<Y>> apply(Optional<X> optionalArg) {
        return (f) -> apply(optionalArg, f);
    }

    public static <X, Y> Optional<Y> apply(Optional<X> optionalArg, Function<X, Optional<Y>> f) {
        return optionalArg.isPresent()
            ? f.apply(optionalArg.get())
            : Optional.empty();
    }
}
