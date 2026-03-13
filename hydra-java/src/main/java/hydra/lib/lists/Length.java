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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Returns the length of a list.
 */
public class Length extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.length");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), int32()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(l -> Terms.int32(apply(l)), hydra.extract.core.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Returns the number of elements.
     * @param <X> the element type
     * @param list the list to get the length of
     * @return the number of elements
     */
    public static <X> int apply(List<X> list) {
        return list.size();
    }
}
