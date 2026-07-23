package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Get all elements except the last, returning Nothing if the list is empty.
 */
public class Init extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.init().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), optional(list("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.optional(Init.apply(l).map(Terms::list)), hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <X> the element type
     * @param list the list to get the init from
     * @return a Optional containing all elements except the last, or empty if the list is empty
     */
    public static <X> Optional<List<X>> apply(List<X> list) {
        if (list.isEmpty()) {
            return Optional.none();
        } else {
            return Optional.given(ConsList.fromList(list).init());
        }
    }
}
