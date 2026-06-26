package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Takes the first n elements.
 */
public class Take extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.take().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), n ->
            hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> Terms.list(apply(n, lst)), hydra.extract.Core.list(graph, args.get(1))));
    }

    /**
     * Takes the first n elements.
     * @param <X> the element type
     * @param n the number of elements to take
     * @return a function that takes the first n elements from a list
     */
    public static <X> Function<List<X>, List<X>> apply(Integer n) {
        return lst -> apply(n, lst);
    }

    /**
     * Takes the first n elements.
     * @param <X> the element type
     * @param n the number of elements to take
     * @param lst the list to take from
     * @return the sublist containing the first n elements
     */
    public static <X> List<X> apply(Integer n, List<X> lst) {
        return ConsList.fromList(lst).take(n);
    }
}
