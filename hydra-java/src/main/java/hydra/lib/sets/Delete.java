package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import hydra.util.PersistentSet;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeOrd;
import static hydra.dsl.Types.set;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Removes an element from a set.
 */
public class Delete extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.delete"
     */
    public Name name() {
        return new Name("hydra.lib.sets.delete");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(arg -> Terms.set(apply(args.get(0), arg)), hydra.extract.core.Core.set(cx, graph, args.get(1)));
    }

    /**
     * Removes an element from a set.
     * @param <X> the type of elements in the set
     * @param elem the element to remove
     * @return a function that takes a set and returns a new set without the element
     */
    public static <X> Function<PersistentSet<X>, PersistentSet<X>> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    /**
     * Removes an element from a set.
     * @param <X> the type of elements in the set
     * @param elem the element to remove
     * @param arg the set to remove from
     * @return a new set with the element removed
     */
    public static <X> PersistentSet<X> apply(X elem, PersistentSet<X> arg) {
        return arg.delete(elem);
    }
}
