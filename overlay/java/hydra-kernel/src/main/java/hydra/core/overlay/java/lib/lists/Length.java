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
import hydra.core.overlay.java.util.Either;

/**
 * Returns the length of a list.
 */
public class Length extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.length().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), int32()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.int32(apply(l)), hydra.core.extract.Model.list(graph, args.get(0)));
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
