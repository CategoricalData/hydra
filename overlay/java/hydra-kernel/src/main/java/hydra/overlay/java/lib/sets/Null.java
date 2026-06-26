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

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import static hydra.overlay.java.dsl.Types.set;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Checks if a set is empty (null check).
 */
public class Null extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.null"
     */
    public Name name() {
        return hydra.lib.Sets.null_().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that checks if a set is null/empty
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(set("x"), boolean_()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(arg -> Terms.boolean_(apply(arg)), hydra.extract.Core.set(graph, args.get(0)));
    }

    /**
     * Checks if a set is empty.
     * @param <X> the type of elements in the set
     * @param arg the set to check
     * @return true if the set is empty, false otherwise
     */
    public static <X> Boolean apply(Set<X> arg) {
        return arg.isEmpty();
    }
}
