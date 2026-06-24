package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Get the element at a given index in a list, returning Nothing if the index is out of bounds.
 */
public class MaybeAt extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.maybeAt().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), i -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.optional(MaybeAt.apply(i, l)), hydra.extract.Core.list(graph, args.get(1))));
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
