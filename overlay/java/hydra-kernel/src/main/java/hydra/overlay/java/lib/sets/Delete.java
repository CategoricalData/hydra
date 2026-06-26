package hydra.overlay.java.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import static hydra.overlay.java.dsl.Types.set;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.PersistentSet;


/**
 * Removes an element from a set.
 */
public class Delete extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.delete"
     */
    public Name name() {
        return hydra.lib.Sets.delete().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that removes an element from a set
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), set("x"), set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(arg -> Terms.set(apply(args.get(0), arg)), hydra.extract.Core.set(graph, args.get(1)));
    }

    /**
     * Removes an element from a set.
     * @param <X> the type of elements in the set
     * @param elem the element to remove
     * @return a function that takes a set and returns a new set without the element
     */
    public static <X> Function<Set<X>, Set<X>> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    /**
     * Removes an element from a set.
     * @param <X> the type of elements in the set
     * @param elem the element to remove
     * @param arg the set to remove from
     * @return a new set with the element removed
     */
    public static <X> Set<X> apply(X elem, Set<X> arg) {
        return PersistentSet.<X>coerce(arg).delete(elem);
    }
}
