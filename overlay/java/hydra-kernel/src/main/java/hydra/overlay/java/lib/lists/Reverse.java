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
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;

/**
 * Reverses a list.
 */
public class Reverse extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.lists.reverse");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.list(Reverse.apply(l)), hydra.extract.Core.list(graph, args.get(0)));
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
