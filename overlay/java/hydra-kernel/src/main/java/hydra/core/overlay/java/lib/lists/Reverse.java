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
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;

/**
 * Reverses a list.
 */
public class Reverse extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.lists.reverse");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.list(Reverse.apply(l)), hydra.core.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Reverses the order of elements in a list.
     * @param <X> the element type
     * @param list the list to reverse
     * @return a new list with elements in reverse order
     */
    public static <X> List<X> apply(List<X> list) {
        return ConsList.fromList(list).reverse();
    }
}
