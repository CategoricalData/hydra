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


public class Or<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/logic.or");
    }

    @Override
    public Type<A> type() {
        return function(boolean_(), boolean_(), boolean_());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map2(
                Expect.boolean_(args.get(0)),
                Expect.boolean_(args.get(1)),
                (b1, b2) -> Terms.boolean_(Or.apply(b1, b2)));
    }

    public static Function<Boolean, Boolean> apply(Boolean b1) {
        return b2 -> apply(b1, b2);
    }

    public static Boolean apply(Boolean b1, Boolean b2) {
        return b1 || b2;
    }
}
