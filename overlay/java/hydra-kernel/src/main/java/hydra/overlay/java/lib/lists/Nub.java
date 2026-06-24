package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeEq;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;

/**
 * Removes duplicate elements.
 */
public class Nub extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.nub().name;
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
      return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.list(Nub.apply(l)), hydra.extract.Core.list(graph, args.get(0)));
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
