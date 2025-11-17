package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
// import hydra.core.OptionalCases; // TODO: replaced by hydra.lib.maybes primitives
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Terms.app;
import static hydra.dsl.Terms.flowState;
import static hydra.dsl.Terms.flowStateState;
import static hydra.dsl.Terms.flowStateTrace;
// import static hydra.dsl.Terms.foldOpt; // TODO: use hydra.lib.maybes primitives
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.nothing;
import static hydra.dsl.Terms.project;
import static hydra.dsl.Terms.variable;


/**
 * Applies a function in a flow context.
 */
public class Apply extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.apply");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x", "y",
                Types.function(
                        Types.flow("s", Types.function("x", "y")),
                        Types.flow("s", "x"),
                        Types.flow("s", "y")));
    }

    // TODO: restore when OptionalCases / hydra.lib.maybes primitives are generated
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            throw new UnsupportedOperationException(
                    "Apply.implementation() not yet available - requires OptionalCases");
        };
    }
    // @Override
    // protected Function<List<Term>, Flow<Graph, Term>> implementation() {
    //     return args -> {
    //         Term mapping = args.get(0);
    //         Term input = args.get(1);
    //
    //         Term ifNothing = flowState(nothing(),
    //                 app("fs1", flowStateState()), app("fs1", flowStateTrace()));
    //         Term ifJust = lambda("f",
    //                 app(variable("f"), input, app("fs1", flowStateState()), app("fs2", flowStateTrace())));
    //         Term output = lambda("s0", "t0", app(
    //                 lambda("fs1", app(foldOpt(
    //                         new OptionalCases(ifNothing, ifJust)),
    //                         project(FlowState.TYPE_NAME, "value"))),
    //                 app(mapping, variable("s0"), variable("s1"))));
    //         return Flows.pure(output);
    //     };
    // }

    /**
     * Applies a function within a flow.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param mapping the flowFunction
     * @return the result flow
     */
    public static <S, X, Y> Function<Flow<S, X>, Flow<S, Y>> apply(Flow<S, Function<X, Y>> mapping) {
        return input -> apply(mapping, input);
    }

    /**
     * Applies a function within a flow.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param mapping the flowFunction
     * @param input the flowValue
     * @return the result flow
     */
    public static <S, X, Y> Flow<S, Y> apply(Flow<S, Function<X, Y>> mapping, Flow<S, X> input) {
        return Flows.apply(mapping, input);
    }
}
