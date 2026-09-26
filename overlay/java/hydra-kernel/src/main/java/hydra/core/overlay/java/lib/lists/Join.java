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
 * Joins lists with a separator.
 */
public class Join extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.join().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a",
                function(list("a"), list(list("a")), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(0)), delim1 -> hydra.core.overlay.java.lib.eithers.Map.apply(sublists1 -> Terms.list(apply(delim1, sublists1)), hydra.core.extract.Model.listOf(t -> hydra.core.extract.Model.list(graph, t), graph, args.get(1))));
    }

    /**
     * Joins lists with a separator.
     * @param <X> the element type
     * @param delim the separator list to insert between lists
     * @return a function that joins lists with the separator
     */
    public static <X> Function<List<List<X>>, List<X>> apply(List<X> delim) {
        return (sublists) -> apply(delim, sublists);
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the element type
     * @param delim the separator list to insert between lists
     * @param sublists the list of lists to join
     * @return the joined list
     */
    public static <X> List<X> apply(List<X> delim, List<List<X>> sublists) {
        ConsList<X> reversed = ConsList.empty();
        boolean first = true;
        for (List<X> sublist : sublists) {
            if (first) {
                first = false;
            } else {
                for (X d : delim) {
                    reversed = ConsList.cons(d, reversed);
                }
            }
            for (X x : sublist) {
                reversed = ConsList.cons(x, reversed);
            }
        }
        return reversed.reverse();
    }
}
