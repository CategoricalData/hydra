package hydra.lib.flows;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Types;
import hydra.dsl.prims.Optionals;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Terms.app;
import static hydra.dsl.Terms.field;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.projection;
import static hydra.dsl.Terms.record;
import static hydra.dsl.Terms.unwrap;
import static hydra.dsl.Terms.variable;


public class Map<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/flows.map");
    }

    @Override
    public Type<A> type() {
        return Types.lambda("s", "x", "y",
                Types.function(Types.function("x", "y"), Types.flow("s", "x"), Types.flow("s", "y")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Term<A> mapping = args.get(0);
            Term<A> input = args.get(1);
            Term<A> output = lambda("s", "t",
                    app(lambda("q", record(FlowState.NAME,
                                    field("value", app(Optionals.map(), mapping, app(projection(FlowState.NAME, "value"), variable("q")))),
                                    field("state", app(projection(FlowState.NAME, "state"), variable("q"))),
                                    field("trace", app(projection(FlowState.NAME, "trace"), variable("q"))))),
                            (app(unwrap(Flow.NAME), input, variable("s"), variable("t")))));
            return Flows.pure(output);
        };
    }

    public static <S, X, Y> Function<Flow<S, X>, Flow<S, Y>> apply(Function<X, Y> mapping) {
        return input -> apply(mapping, input);
    }

    public static <S, X, Y> Flow<S, Y> apply(Function<X, Y> mapping, Flow<S, X> input) {
        return Flows.map(mapping, input);
    }
}
