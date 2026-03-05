package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(sets -> Terms.set(apply(sets)), hydra.extract.core.Core.listOf(cx, t -> hydra.extract.core.Core.set(cx, graph, t), graph, args.get(0)));
    }

    /**
     * Computes the union of multiple sets.
     * @param <X> the type of elements in the sets
     * @param sets the list of sets to combine
     * @return a new set containing all elements from all input sets
     */
    public static <X> Set<X> apply(List<Set<X>> sets) {
        Set<X> combined = new LinkedHashSet<>();
        for (Set<X> s : sets) {
            combined.addAll(s);
        }
        return FromList.orderedSet(combined);
    }
}
