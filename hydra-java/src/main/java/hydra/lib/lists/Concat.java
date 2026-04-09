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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(l -> Terms.list(apply(l)), hydra.extract.Core.listOf(t -> hydra.extract.Core.list(graph, t), graph, args.get(0)));
    }

    /**
     * Concatenates a list of lists into a single list.
     * @param <X> the element type
     * @param sublists the list of lists to concatenate
     * @return a single list containing all elements from all sublists in order
     */
    public static <X> List<X> apply(List<List<X>> sublists) {
        ArrayList<X> result = new ArrayList<>();
        for (List<X> sublist : sublists) {
            result.addAll(sublist);
        }
        return result;
    }
}
