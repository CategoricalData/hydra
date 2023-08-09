package hydra.lib.equality;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;

public class Identity<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/equality.identity");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", "x"));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.pure(args.get(0));
    }

    public static <X> X apply(X object) {
        return object;
    }
}
