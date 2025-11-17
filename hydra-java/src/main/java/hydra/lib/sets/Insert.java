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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Adds an element to a set.
 */
public class Insert extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.insert"
     */
    public Name name() {
        return new Name("hydra.lib.sets.insert");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that inserts an element into a set
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(Types.var("x"), set("x"), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(1)), arg -> Terms.set(apply(args.get(0), arg)));
    }

    /**
     * Adds an element to a set.
     * @param <X> the type of elements in the set
     * @param elem the element to add
     * @return a function that takes a set and returns a new set with the element added
     */
    public static <X> Function<Set<X>, Set<X>> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    /**
     * Adds an element to a set.
     * @param <X> the type of elements in the set
     * @param elem the element to add
     * @param arg the set to add to
     * @return a new set with the element added
     */
    public static <X> Set<X> apply(X elem, Set<X> arg) {
        Set<X> newSet = new HashSet<>(arg);
        newSet.add(elem);
        return newSet;
    }
}
