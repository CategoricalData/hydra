package hydra.lib.sets;

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
import static hydra.dsl.Types.schemeOrd;
import static hydra.dsl.Types.set;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


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
        java.util.LinkedHashSet<X> combined = new java.util.LinkedHashSet<>(arg);
        combined.add(elem);
        return FromList.orderedSet(combined);
    }
}
