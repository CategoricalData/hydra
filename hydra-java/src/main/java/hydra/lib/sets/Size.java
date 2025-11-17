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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Returns the number of elements in a set.
 */
public class Size extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.size"
     */
    public Name name() {
        return new Name("hydra.lib.sets.size");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that returns the size of a set
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(set("x"), int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(0)), arg -> Terms.int32(apply(arg)));
    }

    /**
     * Returns the number of elements in a set.
     * @param <X> the type of elements in the set
     * @param arg the set to measure
     * @return the number of elements in the set
     */
    public static <X> Integer apply(Set<X> arg) {
        return arg.size();
    }
}
