package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Checks if a list is empty.
 */
public class Null extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.null");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.boolean_(apply(l)), hydra.extract.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Checks if the list is empty.
     * @param <X> the element type
     * @param list the list to check
     * @return true if empty, false otherwise
     */
    public static <X> boolean apply(List<X> list) {
        return list.isEmpty();
    }
}
