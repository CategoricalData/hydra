package hydra.lib.literals;

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

import static hydra.dsl.Types.*;

public class ShowInt32<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/literals.showInt32");
    }

    @Override
    public Type<A> type() {
        return function(int32(), string());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.int32(args.get(0)), (Function<Integer, Term<A>>) s -> Terms.string(apply(s)));
    }

    public static String apply(Integer value) {
        return Integer.toString(value);
    }
}
