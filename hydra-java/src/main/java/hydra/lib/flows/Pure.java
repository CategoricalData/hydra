package hydra.lib.flows;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.flow;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;


public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/flows.pure");
    }

    @Override
    public Type<A> type() {
        return lambda("s", "x", function("x", flow("s", "x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.pure(Terms.lambda("x", "s", "t", Terms.record(FlowState.NAME,
                Terms.field("value", Terms.just("x")),
                Terms.field("state", "s"),
                Terms.field("trace", "t"))));
    }

    public static <S, X> Flow<S, X> apply(X elem) {
        return Flows.pure(elem);
    }
}
