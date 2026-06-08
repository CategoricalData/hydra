package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Optional;
import hydra.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Split a list into its head and tail, returning Nothing if the list is empty.
 */
public class Uncons extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.uncons");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), optional(pair(variable("a"), list("a")))));
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
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
