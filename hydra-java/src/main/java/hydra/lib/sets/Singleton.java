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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Creates a singleton set containing a single element.
 */
public class Singleton extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.singleton"
     */
    public Name name() {
        return new Name("hydra.lib.sets.singleton");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that creates a singleton set
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function("x", set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.pure(Terms.set(apply(args.get(0))));
    }

    /**
     * Creates a singleton set containing a single element.
     * @param <X> the type of the element
     * @param elem the element to put in the set
     * @return a new set containing only the specified element
     */
    public static <X> Set<X> apply(X elem) {
        return FromList.orderedSet(java.util.List.of(elem));
    }
}
