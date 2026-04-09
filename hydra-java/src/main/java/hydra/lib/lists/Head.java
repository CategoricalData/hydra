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
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Returns the first element of a list.
 */
public class Head extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.head");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), "a"));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) Head::apply, hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Returns the first element of a list. Throws if the list is empty.
     * @param <X> the element type
     * @param list the list to get the head from
     * @return the first element
     * @throws IllegalArgumentException if the list is empty
     */
    public static <X> X apply(List<X> list) {
        if (list.isEmpty()) {
            throw new IllegalArgumentException("Cannot get head of empty list");
        }
        return list.get(0);
    }
}
