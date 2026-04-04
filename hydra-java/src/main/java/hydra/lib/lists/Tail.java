package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Returns all elements except the first.
 */
public class Tail extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.tail");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
                (Function<List<Term>, Term>) terms -> Terms.list(apply(terms)),
                hydra.extract.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Returns the tail of the list.
     * @param <X> the element type
     * @param list the list to get the tail from
     * @return the tail (all elements except the first)
     */
    public static <X> List<X> apply(List<X> list) {
        return new ArrayList<>(list.subList(1, list.size()));
    }
}
