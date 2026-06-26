package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeEq;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Checks if an element is in a list.
 */
public class Elem extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.elem().name;
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(Types.var("a"), list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> Terms.boolean_(lst.contains(args.get(0))), hydra.extract.Core.list(graph, args.get(1)));
    }

    /**
     * Checks if the element is present.
     * @param <X> the element type
     * @param elem the element to search for
     * @return a function that checks if the element is present in a list
     */
    public static <X> Function<List<X>, Boolean> apply(X elem) {
        return lst -> apply(elem, lst);
    }

    /**
     * Checks if the element is present.
     * @param <X> the element type
     * @param elem the element to search for
     * @param lst the list to search in
     * @return true if present, false otherwise
     */
    public static <X> Boolean apply(X elem, List<X> lst) {
        return lst.contains(elem);
    }
}
