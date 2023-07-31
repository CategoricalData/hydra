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
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.string;

public class FromList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.fromList");
    }

    @Override
    public Type<A> type() {
        return function(list(int32()), string());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> {
            Flow<Graph<A>, List<Integer>> list = Expect.list(Expect::int32, args.get(0));
            return Flows.map(list, l -> Terms.string(FromList.apply(l)));
        };
    }

    public static String apply(List<Integer> list) {
        StringBuilder sb = new StringBuilder();
        for (Integer i : list) {
            sb.append((char) i.intValue());
        }
        return sb.toString();
    }
}
