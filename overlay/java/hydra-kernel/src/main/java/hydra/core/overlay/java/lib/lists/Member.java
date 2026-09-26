package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeEq;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Checks if an element is in a list.
 */
public class Member extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.member().name;
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(Types.var("a"), list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> Terms.boolean_(lst.contains(args.get(0))), hydra.core.extract.Model.list(graph, args.get(1)));
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
