package hydra.core.overlay.java.lib.sets;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import static hydra.core.overlay.java.dsl.Types.set;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentSet;


/**
 * Adds an element to a set.
 */
public class Insert extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.sets.insert"
     */
    public Name name() {
        return hydra.lib.Sets.insert().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that inserts an element into a set
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(arg -> Terms.set(apply(args.get(0), arg)), hydra.core.extract.Core.set(graph, args.get(1)));
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
        return PersistentSet.<X>coerce(arg).insert(elem);
    }
}
