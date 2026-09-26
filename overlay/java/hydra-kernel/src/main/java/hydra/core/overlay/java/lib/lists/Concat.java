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
 * Concatenates a list of lists.
 */
public class Concat extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.lists.concat");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list(list("a")), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(l -> Terms.list(apply(l)), hydra.core.extract.Model.listOf(t -> hydra.core.extract.Model.list(graph, t), graph, args.get(0)));
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
