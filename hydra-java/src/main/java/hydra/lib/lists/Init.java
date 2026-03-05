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
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Returns all elements except the last.
 */
public class Init extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.init");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> Terms.list(apply(lst)), hydra.extract.core.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Returns all but the last element.
     * @param <X> the element type
     * @param lst the list to get the initial elements from
     * @return the initial elements (all except the last)
     */
    public static <X> List<X> apply(List<X> lst) {
        if (lst.isEmpty()) {
            throw new IllegalArgumentException("init: empty list");
        }
        return lst.subList(0, lst.size() - 1);
    }
}
