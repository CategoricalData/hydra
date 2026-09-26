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
 * Inserts an element between list elements.
 */
public class Intersperse extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.intersperse().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) list -> Terms.list(Intersperse.apply(args.get(0), list)), hydra.core.extract.Model.list(graph, args.get(1)));
    }

    /**
     * Inserts an element between elements.
     * @param <X> the element type
     * @param delim the separator element to insert
     * @return a function that intersperses the separator into a list
     */
    public static <X> Function<List<X>, List<X>> apply(X delim) {
        return (list) -> apply(delim, list);
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the element type
     * @param delim the separator element to insert
     * @param list the list to intersperse
     * @return the list with the separator inserted between elements
     */
    public static <X> List<X> apply(X delim, List<X> list) {
        ConsList<X> reversed = ConsList.empty();
        boolean first = true;
        for (X a : list) {
            if (first) {
                first = false;
            } else {
                reversed = ConsList.cons(delim, reversed);
            }
            reversed = ConsList.cons(a, reversed);
        }
        return reversed.reverse();
    }
}
