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
import hydra.util.Opt;
import java.util.function.Function;

import static hydra.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;


public class SafeHead extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra/lib/lists.safeHead");
    }

    @Override
    public Type type() {
        return lambda("a", function(list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.list(Flows::pure, args.get(0)), l -> Terms.optional(apply(l)));
    }

    /**
     * Apply the function to it single argument.
     */
    public static <X> Opt<X> apply(List<X> list) {
        if (list.isEmpty()) {
            return Opt.empty();
        } else {
            return Opt.of(list.get(0));
        }
    }
}
