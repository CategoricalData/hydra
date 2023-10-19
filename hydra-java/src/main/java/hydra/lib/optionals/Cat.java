package hydra.lib.optionals;

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
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;


public class Cat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/optionals.cat");
    }

    @Override
    public Type<A> type() {
        return lambda("a", function(list(optional("a")), list("a")));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.list(x -> Expect.optional(Flows::pure, x), args.get(0)),
                (Function<List<Optional<Term<A>>>, Term<A>>) optionals -> Terms.list(apply(optionals)));
    }

    public static <X> List<X> apply(List<Optional<X>> opt) {
        List<X> result = new ArrayList<>();
        for (Optional<X> x : opt) {
            x.ifPresent(result::add);
        }
        return result;
    }
}
