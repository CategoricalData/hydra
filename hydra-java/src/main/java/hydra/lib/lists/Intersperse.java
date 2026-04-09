package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Inserts an element between list elements.
 */
public class Intersperse extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.intersperse");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) list -> Terms.list(Intersperse.apply(args.get(0), list)), hydra.extract.Core.list(graph, args.get(1)));
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
        ArrayList<X> result = new ArrayList<>();
        boolean first = true;
        for (X a : list) {
            if (first) {
                first = false;
            } else {
                result.add(delim);
            }
            result.add(a);
        }
        return result;
    }
}
