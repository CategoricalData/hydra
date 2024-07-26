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

import java.util.List;
import hydra.util.Opt;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.lambda;
import static hydra.dsl.Types.optional;


public class IsNothing extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra/lib/optionals.isNothing");
    }

    @Override
    public Type type() {
        return lambda("a", function(optional("a"), "a"));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)), x -> Terms.boolean_(IsNothing.apply(x)));
    }

    public static <X> boolean apply(Opt<X> opt) {
        return !opt.isPresent();
    }
}
