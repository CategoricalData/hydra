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
import static hydra.dsl.Types.*;

public class Length<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.length");
    }

    @Override
    public Type<A> type() {
        return function(string(), int32());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map(Expect.string(args.get(0)), s -> Terms.int32(apply(s)));
    }

    public static int apply(String s) {
        return s.length();
    }
}
