package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Terms.flowState;
import static hydra.dsl.Terms.just;


/**
 * Wraps a value in a flow.
 */
public class Pure extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.pure");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x", Types.function("x", Types.flow("s", "x")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.pure(flowState(just(args.get(0)), args.get(1), args.get(2)));
    }

    /**
     * Creates a successful flow.
     * @param <S> the state type
     * @param <X> the output type
     * @param elem the value
     * @return the flow
     */
    public static <S, X> Flow<S, X> apply(X elem) {
        return Flows.pure(elem);
    }
}
