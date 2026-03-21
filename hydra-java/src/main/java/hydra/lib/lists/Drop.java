package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Drops the first n elements.
 */
public class Drop extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.drop");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.int32(cx, graph, args.get(0)), n ->
            hydra.lib.eithers.Map.apply((Function<ConsList<Term>, Term>) lst -> Terms.list(apply(n, lst)), hydra.extract.core.Core.list(cx, graph, args.get(1))));
    }

    /**
     * Drops the first n elements.
     * @param <X> the element type
     * @param n the number of elements to drop
     * @return a function that drops n elements from a list
     */
    public static <X> Function<ConsList<X>, ConsList<X>> apply(Integer n) {
        return lst -> apply(n, lst);
    }

    /**
     * Drops the first n elements.
     * @param <X> the element type
     * @param n the number of elements to drop
     * @param lst the list to drop from
     * @return the remaining list
     */
    public static <X> ConsList<X> apply(Integer n, ConsList<X> lst) {
        if (n <= 0) {
            return lst;
        }
        if (n >= lst.size()) {
            return ConsList.empty();
        }
        return lst.drop(n);
    }
}
