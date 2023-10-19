package hydra.lib.optionals;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.pure");
    }

    @Override
    public Type<A> type() {
        return lambda("a", function("a", optional("a")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.pure(Terms.optional(apply(args.get(0))));
    }

    public static <X> Optional<X> apply(X arg) {
        return Optional.of(arg);
    }
}
