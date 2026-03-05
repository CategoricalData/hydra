package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
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
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
      return args -> cx -> graph -> hydra.lib.eithers.Map.apply(l -> Terms.list(apply(l)), hydra.extract.core.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Apply the function to the single argument.
     * @param <X> the element type
     * @param arg the list to remove duplicates from
     * @return the list with duplicates removed
     */
    public static <X> List<X> apply(List<X> arg) {
        Set<X> visited = new HashSet<>();
        List<X> result = new ArrayList<>(arg.size());
        for (X x : arg) {
            if (!visited.contains(x)) {
                visited.add(x);
                result.add(x);
            }
        }
        return result;
    }
}
