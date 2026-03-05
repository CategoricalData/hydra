package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Collections;
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(l -> Terms.list(Reverse.apply(l)), hydra.extract.core.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Reverses the order of elements in a list.
     * @param <X> the element type
     * @param list the list to reverse
     * @return a new list with elements in reverse order
     */
    public static <X> List<X> apply(List<X> list) {
        List<X> newList = new ArrayList<>(list);
        Collections.reverse(newList);
        return newList;
    }
}
