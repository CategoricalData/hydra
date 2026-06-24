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
 * Concatenates a list of lists.
 */
public class Concat extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.lists.concat");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list(list("a")), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(l -> Terms.list(apply(l)), hydra.extract.Core.listOf(t -> hydra.extract.Core.list(graph, t), graph, args.get(0)));
    }

    /**
     * Concatenates a list of lists into a single list.
     * @param <X> the element type
     * @param sublists the list of lists to concatenate
     * @return a single list containing all elements from all sublists in order
     */
    public static <X> List<X> apply(List<List<X>> sublists) {
        ConsList<X> reversed = ConsList.empty();
        for (List<X> sublist : sublists) {
            for (X elem : sublist) {
                reversed = ConsList.cons(elem, reversed);
            }
        }
        return reversed.reverse();
    }
}
