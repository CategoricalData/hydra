package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Returns the last element of a list.
 */
public class Last extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.last");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), "a"));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(Last::apply, hydra.extract.core.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Returns the last element.
     * @param <X> the element type
     * @param list the list to get the last element from
     * @return the last element
     */
    public static <X> X apply(List<X> list) {
        return list.get(list.size() - 1);
    }
}
