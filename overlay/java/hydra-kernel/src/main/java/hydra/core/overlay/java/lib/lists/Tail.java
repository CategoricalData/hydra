package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Get all elements except the first, returning Nothing if the list is empty.
 */
public class Tail extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.tail().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), optional(list("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.optional(Tail.apply(l).map(Terms::list)), hydra.core.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <X> the element type
     * @param list the list to get the tail from
     * @return a Optional containing all elements except the first, or empty if the list is empty
     */
    public static <X> Optional<List<X>> apply(List<X> list) {
        if (list.isEmpty()) {
            return Optional.none();
        } else {
            return Optional.given(ConsList.fromList(list).tail());
        }
    }
}
