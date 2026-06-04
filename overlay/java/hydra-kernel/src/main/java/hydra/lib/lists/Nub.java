package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeEq;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.ConsList;
import hydra.util.Either;

/**
 * Removes duplicate elements.
 */
public class Nub extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.nub");
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
      return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.list(Nub.apply(l)), hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Apply the function to the single argument.
     * @param <X> the element type
     * @param arg the list to remove duplicates from
     * @return the list with duplicates removed
     */
    public static <X> List<X> apply(List<X> arg) {
        Set<X> visited = new HashSet<>();
        ConsList<X> reversed = ConsList.empty();
        for (X x : arg) {
            if (visited.add(x)) {
                reversed = ConsList.cons(x, reversed);
            }
        }
        return reversed.reverse();
    }
}
