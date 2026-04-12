package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.TreeSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeOrd;
import static hydra.dsl.Types.set;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Creates a set from a list of elements.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.fromList"
     */
    public Name name() {
        return new Name("hydra.lib.sets.fromList");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that creates a set from a list
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(list("x"), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(arg -> Terms.set(apply(arg)), hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Creates a set from a list of elements.
     * @param <X> the type of elements in the list and set
     * @param arg the list of elements
     * @return a set containing all unique elements from the list
     */
    public static <X> Set<X> apply(List<X> arg) {
        return new TreeSet<>(arg);
    }

    /**
     * Creates a set from elements, preserving a consistent iteration order where possible.
     */
    static <X> Set<X> orderedSet(java.util.Collection<X> elements) {
        return new TreeSet<>(elements);
    }
}
