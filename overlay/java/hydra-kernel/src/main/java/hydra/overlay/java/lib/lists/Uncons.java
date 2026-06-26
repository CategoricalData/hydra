package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Optional;
import hydra.overlay.java.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.pair;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


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
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(
            (Function<List<Term>, Term>) l -> Terms.optional(
                Uncons.apply(l).map(p -> Terms.pair(p.first, Terms.list(p.second)))),
            hydra.extract.Core.list(graph, args.get(0)));
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
