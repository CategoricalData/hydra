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

import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import static hydra.overlay.java.dsl.Types.set;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;
import hydra.overlay.java.util.PersistentSet;


/**
 * Creates an empty set.
 */
public class Empty extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.empty"
     */
    public Name name() {
        return hydra.lib.Sets.empty().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that creates an empty set
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", set("x"));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return ignored -> graph -> Either.right(Terms.set(apply()));
    }

    /**
     * Creates an empty set.
     * @param <X> the type of elements in the set
     * @return an empty set
     */
    public static <X> Set<X> apply() {
        return PersistentSet.empty();
    }
}
