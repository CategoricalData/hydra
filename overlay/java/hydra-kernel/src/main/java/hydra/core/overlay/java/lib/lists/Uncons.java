package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Optional;
import hydra.core.overlay.java.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.pair;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Split a list into its head and tail, returning Nothing if the list is empty.
 */
public class Uncons extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.uncons().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), optional(pair(variable("a"), list("a")))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(
            (Function<List<Term>, Term>) l -> Terms.optional(
                Uncons.apply(l).map(p -> Terms.pair(p.first, Terms.list(p.second)))),
            hydra.core.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <X> the element type
     * @param list the list to split
     * @return a Optional containing a pair (head, tail), or empty if the list is empty
     */
    public static <X> Optional<Pair<X, List<X>>> apply(List<X> list) {
        if (list.isEmpty()) {
            return Optional.none();
        }
        ConsList<X> cl = ConsList.fromList(list);
        return Optional.given(new Pair<>(cl.head(), cl.tail()));
    }
}
