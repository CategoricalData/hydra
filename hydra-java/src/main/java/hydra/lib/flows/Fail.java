package hydra.lib.flows;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.FlowState;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.lib.lists.Cons;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Terms.app;
import static hydra.dsl.Terms.flowState;
import static hydra.dsl.Terms.lambda;
import static hydra.dsl.Terms.project;
import static hydra.dsl.Terms.unwrap;
import static hydra.dsl.Terms.variable;


public class Fail<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/flows.fail");
    }

    @Override
    public Type<A> type() {
        return Types.lambda("s", "x", Types.function(Types.string(), Types.flow("s", "x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.pure(app(unwrap(Flow.NAME), lambda("q", flowState(
                Terms.nothing(),
                app(project(FlowState.NAME, "state"), variable("q")),
                app((new Cons().term()), args.get(0), app(project(FlowState.NAME, "trace"), variable("q")))))));
    }

    public static <S, X> Flow<S, X> apply(String msg) {
        return Flows.fail(msg);
    }
}
