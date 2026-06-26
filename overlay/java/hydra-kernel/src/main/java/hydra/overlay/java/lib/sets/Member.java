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
 * Checks if an element is in a set.
 */
public class Member extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.contains"
     */
    public Name name() {
        return hydra.lib.Sets.member().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that checks set membership
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(Types.var("x"), set("x"), boolean_()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(terms -> Terms.boolean_(apply(args.get(0), terms)), hydra.extract.Core.set(graph, args.get(1)));
    }

    /**
     * Checks if an element is in a set.
     * @param <X> the type of elements in the set
     * @param elem the element to check for
     * @return a function that takes a set and returns true if the element is present
     */
    public static <X> Function<Set<X>, Boolean> apply(X elem) {
        return (arg) -> apply(elem, arg);
    }

    /**
     * Checks if an element is in a set.
     * @param <X> the type of elements in the set
     * @param elem the element to check for
     * @param arg the set to check
     * @return true if the element is present in the set, false otherwise
     */
    public static <X> Boolean apply(X elem, Set<X> arg) {
        return arg.contains(elem);
    }
}
