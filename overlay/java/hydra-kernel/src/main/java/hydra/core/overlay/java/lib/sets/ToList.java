package hydra.core.overlay.java.lib.sets;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import static hydra.core.overlay.java.dsl.Types.set;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Converts a set to a list of elements.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.sets.toList"
     */
    public Name name() {
        return hydra.core.lib.Sets.toList().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a set to a list
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(set("x"), list("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        // Elements are listed in the order of their order keys, so union variants come out in declared order (#848)
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(
            hydra.core.overlay.java.lib.OrderKeys.set(graph, args.get(0)),
            terms -> hydra.core.overlay.java.lib.eithers.Map.apply(
                sorted -> Terms.list(ConsList.fromList(sorted)),
                hydra.core.overlay.java.lib.OrderKeys.sort(graph, new ArrayList<>(terms))));
    }

    /**
     * Converts a set to a list of elements.
     * @param <X> the type of elements in the set and list
     * @param arg the set to convert
     * @return a list containing all elements from the set
     */
    public static <X> List<X> apply(Set<X> arg) {
        ConsList<X> reversed = ConsList.empty();
        for (X x : arg) {
            reversed = ConsList.cons(x, reversed);
        }
        return reversed.reverse();
    }
}
