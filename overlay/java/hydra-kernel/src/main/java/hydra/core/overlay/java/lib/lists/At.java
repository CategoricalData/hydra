package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Get the element at a given index in a list, returning Nothing if the index is out of bounds.
 */
public class At extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.at().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.int32(graph, args.get(0)), i -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.optional(At.apply(i, l)), hydra.core.extract.Core.list(graph, args.get(1))));
    }

    /**
     * Apply the function to its arguments.
     * @param <X> the element type
     * @param index the zero-based index
     * @param list the list to get the element from
     * @return a Optional containing the element at the index, or empty if out of bounds
     */
    public static <X> Optional<X> apply(int index, List<X> list) {
        if (index < 0 || index >= list.size()) {
            return Optional.none();
        } else {
            return Optional.given(list.get(index));
        }
    }
}
