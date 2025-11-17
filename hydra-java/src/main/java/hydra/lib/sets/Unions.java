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
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Computes the union of multiple sets.
 */
public class Unions extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.unions"
     */
    public Name name() {
        return new Name("hydra.lib.sets.unions");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that computes the union of multiple sets
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(list(set("x")), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.list(t -> Expect.set(Flows::pure, t), args.get(0)),
                sets -> Terms.set(apply(sets)));
    }

    /**
     * Computes the union of multiple sets.
     * @param <X> the type of elements in the sets
     * @param sets the list of sets to combine
     * @return a new set containing all elements from all input sets
     */
    public static <X> Set<X> apply(List<Set<X>> sets) {
        Set<X> result = new HashSet<>();
        for (Set<X> s : sets) {
            result.addAll(s);
        }
        return result;
    }
}
