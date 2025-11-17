package hydra.lib.sets;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Creates an empty set.
 */
public class Empty extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.empty"
     */
    public Name name() {
        return new Name("hydra.lib.sets.empty");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that creates an empty set
     */
    @Override
    public TypeScheme type() {
        return scheme("x", set("x"));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return ignored -> Flows.pure(Terms.set(apply()));
    }

    /**
     * Creates an empty set.
     * @param <X> the type of elements in the set
     * @return an empty set
     */
    public static <X> Set<X> apply() {
        return Collections.emptySet();
    }
}
