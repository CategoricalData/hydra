package hydra.lib.strings;

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

import static hydra.Flows.map;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.string;

public class IsEmpty<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.isEmpty");
    }

    @Override
    public Type<A> type() {
        return function(string(), boolean_());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map(Expect.string(args.get(0)), s -> Terms.boolean_(apply(s)));
    }

    public static boolean apply(String s) {
        return s.isEmpty();
    }
}
