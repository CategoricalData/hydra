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
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import static hydra.core.overlay.java.dsl.Types.set;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Returns the number of elements in a set.
 */
public class Size extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.sets.size"
     */
    public Name name() {
        return hydra.lib.Sets.size().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that returns the size of a set
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(set("x"), int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(arg -> Terms.int32(apply(arg)), hydra.core.extract.Core.set(graph, args.get(0)));
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
