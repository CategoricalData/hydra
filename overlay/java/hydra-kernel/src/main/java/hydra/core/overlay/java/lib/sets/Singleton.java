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
 * Creates a singleton set containing a single element.
 */
public class Singleton extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.sets.singleton"
     */
    public Name name() {
        return hydra.lib.Sets.singleton().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that creates a singleton set
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function("x", set("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.set(apply(args.get(0))));
    }

    /**
     * Creates a singleton set containing a single element.
     * @param <X> the type of the element
     * @param elem the element to put in the set
     * @return a new set containing only the specified element
     */
    public static <X> Set<X> apply(X elem) {
        return PersistentSet.singleton(elem);
    }
}
