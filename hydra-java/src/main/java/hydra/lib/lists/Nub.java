package hydra.lib.lists;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;

public class Nub<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.nub");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list("x"), list("x")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
      return args -> Flows.map(Expect.list(Flows::pure, args.get(0)), l -> Terms.list(apply(l)));
    }

    public static <X> List<X> apply(List<X> arg) {
        Set<X> visited = new HashSet<>();
        List<X> result = new ArrayList<>(arg.size());
        for (X x : arg) {
            if (!visited.contains(x)) {
                visited.add(x);
                result.add(x);
            }
        }
        return result;
    }
}
