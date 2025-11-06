package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Extracts the value from Just.
 */
public class FromJust extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maybes.fromJust");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(optional("a"), "a"));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(Flows::pure, args.get(0)),
            opt -> opt.isPresent() ? pure(opt.get()) : Flows.fail("fromJust: Nothing"));
    }

    /**
     * Unwraps Just (unsafe).
     * @param opt the maybeValue
     * @return the value
     */
        public static <X> X apply(Opt<X> opt) {
        if (!opt.isPresent()) {
            throw new RuntimeException("fromJust: Nothing");
        }
        return opt.get();
    }
}
