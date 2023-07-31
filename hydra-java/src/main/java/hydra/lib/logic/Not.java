package hydra.lib.logic;

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

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;


public class Not<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/logic.not");
    }

    @Override
    public Type<A> type() {
        return function(boolean_(), boolean_());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(
                Expect.boolean_(args.get(0)),
                b1 -> Terms.boolean_(Not.apply(b1)));
    }

    public static Boolean apply(Boolean b1) {
        return !b1;
    }
}
