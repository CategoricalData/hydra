package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Checks if value is Nothing.
 */
public class IsNothing extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.isNothing"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.isNothing");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for checking if an optional is empty
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(optional("a"), "a"));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that checks if an optional value is empty
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)), x -> Terms.boolean_(IsNothing.apply(x)));
    }

    /**
     * Checks if an optional value is empty (Nothing).
     * @param <X> the value type
     * @param opt the optional value to check
     * @return true if the optional is empty, false otherwise
     */
    public static <X> boolean apply(Maybe<X> opt) {
        return !opt.isJust();
    }
}
