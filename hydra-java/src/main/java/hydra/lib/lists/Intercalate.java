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
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Joins lists with a separator.
 */
public class Intercalate extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.intercalate");
    }

    @Override
    public TypeScheme type() {
        return scheme("a",
                function(list("a"), list(list("a")), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(0)), delim1 -> hydra.lib.eithers.Map.apply(sublists1 -> Terms.list(apply(delim1, sublists1)), hydra.extract.core.Core.listOf(cx, t -> hydra.extract.core.Core.list(cx, graph, t), graph, args.get(1))));
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
        List<X> result = new ArrayList<>();
        boolean first = true;
        for (List<X> sublist : sublists) {
            if (first) {
                first = false;
            } else {
                result.addAll(delim);
            }
            result.addAll(sublist);
        }
        return result;
    }
}
