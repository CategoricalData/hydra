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
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Checks if a list is empty.
 */
public class IsEmpty extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.isEmpty().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.boolean_(apply(l)), hydra.core.extract.Core.list(graph, args.get(0)));
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
