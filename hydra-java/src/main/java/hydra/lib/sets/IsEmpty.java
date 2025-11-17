package hydra.lib.sets;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Checks if a set is empty.
 */
public class IsEmpty extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.isEmpty"
     */
    public Name name() {
        return new Name("hydra.lib.sets.isEmpty");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that checks if a set is empty
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(set("x"), boolean_()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(0)), arg -> Terms.boolean_(apply(arg)));
    }

    /**
     * Checks if a set is empty.
     * @param <X> the type of elements in the set
     * @param arg the set to check
     * @return true if the set is empty, false otherwise
     */
    public static <X> Boolean apply(Set<X> arg) {
        return arg.isEmpty();
    }
}
