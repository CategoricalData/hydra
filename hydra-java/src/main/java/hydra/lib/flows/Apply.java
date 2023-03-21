package hydra.lib.flows;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.OptionalCases;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Terms.app;
import static hydra.dsl.Terms.flowState;
import static hydra.dsl.Terms.flowStateState;
import static hydra.dsl.Terms.flowStateTrace;
import static hydra.dsl.Terms.foldOpt;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.nothing;
import static hydra.dsl.Terms.projection;
import static hydra.dsl.Terms.variable;


public class Apply<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/flows.apply");
    }

    @Override
    public Type<A> type() {
        return Types.lambda("s", "x", "y",
                Types.function(
                        Types.flow("s", Types.function("x", "y")),
                        Types.flow("s", "x"),
                        Types.flow("s", "y")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Term<A> mapping = args.get(0);
            Term<A> input = args.get(1);

            Term<A> ifNothing = flowState(nothing(), app("fs1", flowStateState()), app("fs1", flowStateTrace()));
            Term<A> ifJust = lambda("f", app(variable("f"), input, app("fs1", flowStateState()), app("fs2", flowStateTrace())));
            Term<A> output = lambda("s0", "t0", app(
                    lambda("fs1", app(foldOpt(
                            new OptionalCases<>(ifNothing, ifJust)),
                            projection(FlowState.NAME, "value"))),
                    app(mapping, variable("s0"), variable("s1"))));
            return Flows.pure(output);
        };
    }

    public static <S, X, Y> Function<Flow<S, X>, Flow<S, Y>> apply(Flow<S, Function<X, Y>> mapping) {
        return input -> apply(mapping, input);
    }

    public static <S, X, Y> Flow<S, Y> apply(Flow<S, Function<X, Y>> mapping, Flow<S, X> input) {
        return Flows.apply(mapping, input);
    }
}
