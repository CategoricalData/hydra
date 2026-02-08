package hydra.lib.flows;

import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Flows;
import hydra.dsl.Types;
import hydra.dsl.prims.Maybes;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Terms.app;
import static hydra.dsl.Terms.flowState;
import static hydra.dsl.Terms.just;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.primitive;
import static hydra.dsl.Terms.project;
import static hydra.dsl.Terms.unwrap;
import static hydra.dsl.Terms.variable;
import static hydra.dsl.Terms.wrap;


/**
 * Try a flow and use a fallback value if it fails.
 */
public class WithDefault extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.withDefault");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x",
                Types.function("x", Types.function(Types.flow("s", "x"), Types.flow("s", "x"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term fallback = args.get(0);
            Term input = args.get(1);
            // Build: wrap(Flow, \s t ->
            //   let q = unwrap(Flow)(input)(s)(t)
            //   in maybe(FlowState{value=Just(fallback), state=s, trace=t})
            //           (\_ -> q)
            //           (project(FlowState, "value")(q)))
            Term output = wrap(Flow.TYPE_NAME, lambda("s", "t",
                    app(lambda("q",
                            app(primitive("hydra.lib.maybes.maybe"),
                                    flowState(just(fallback), variable("s"), variable("t")),
                                    lambda("_", variable("q")),
                                    app(project(FlowState.TYPE_NAME, "value"), variable("q")))),
                            app(unwrap(Flow.TYPE_NAME), input, variable("s"), variable("t")))));
            return Flows.pure(output);
        };
    }

    /**
     * Try a flow and use a fallback value if it fails.
     * @param <S> the state type
     * @param <X> the value type
     * @param fallback the default value to use if the flow fails
     * @return a function that takes a flow and returns a flow with the fallback
     */
    public static <S, X> Function<Flow<S, X>, Flow<S, X>> apply(X fallback) {
        return flow -> apply(fallback, flow);
    }

    /**
     * Try a flow and use a fallback value if it fails.
     * @param <S> the state type
     * @param <X> the value type
     * @param fallback the default value to use if the flow fails
     * @param flow the flow to try
     * @return a flow that succeeds with either the flow's result or the fallback
     */
    public static <S, X> Flow<S, X> apply(X fallback, Flow<S, X> flow) {
        return Flows.withDefault(fallback, flow);
    }
}
