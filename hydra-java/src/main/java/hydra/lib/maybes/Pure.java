package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Wraps a value in a flow.
 */
public class Pure extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.pure"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.pure");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for wrapping a value in an optional
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function("a", optional("a")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that wraps a value in an optional
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.pure(Terms.optional(apply(args.get(0))));
    }

    /**
     * Wraps a value in an optional (Just constructor).
     * @param <X> the value type
     * @param arg the value to wrap
     * @return an optional containing the value
     */
    public static <X> Opt<X> apply(X arg) {
        return Opt.of(arg);
    }
}
