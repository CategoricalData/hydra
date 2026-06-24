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
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Checks if a list is empty.
 */
public class Null extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.null_().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.boolean_(apply(l)), hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Checks if the list is empty.
     * @param <X> the element type
     * @param list the list to check
     * @return true if empty, false otherwise
     */
    public static <X> boolean apply(List<X> list) {
        return list.isEmpty();
    }
}
