package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Takes the first n elements.
 */
public class Take extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.take().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.int32(graph, args.get(0)), n ->
            hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> Terms.list(apply(n, lst)), hydra.core.extract.Model.list(graph, args.get(1))));
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
