package hydra.lib.strings;

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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.string;

public class Cat2<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.cat2");
    }

    @Override
    public Type<A> type() {
        return function(string(), string(), string());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map2(
                Expect.string(args.get(0)),
                Expect.string(args.get(1)),
            (l, r) -> Terms.string(Cat2.apply(l, r)));
    }

    public static String apply(String left, String right) {
        return left + right;
    }
}
