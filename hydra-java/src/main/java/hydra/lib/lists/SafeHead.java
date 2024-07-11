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

import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static hydra.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;


public class SafeHead<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.safeHead");
    }

    @Override
    public Type<A> type() {
        return lambda("a", function(list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map(Expect.list(Flows::pure, args.get(0)), l -> Terms.optional(apply(l)));
    }

    /**
     * Apply the function to it single argument.
     */
    public static <X> Optional<X> apply(List<X> list) {
        if (list.isEmpty()) {
            return Optional.empty();
        } else {
            return Optional.of(list.get(0));
        }
    }
}
