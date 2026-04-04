package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeEq;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Checks if an element is in a list.
 */
public class Elem extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.elem");
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(Types.var("a"), list("a"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> Terms.boolean_(lst.contains(args.get(0))), hydra.extract.Core.list(cx, graph, args.get(1)));
    }

    /**
     * Checks if the element is present.
     * @param <X> the element type
     * @param elem the element to search for
     * @return a function that checks if the element is present in a list
     */
    public static <X> Function<List<X>, Boolean> apply(X elem) {
        return lst -> apply(elem, lst);
    }

    /**
     * Checks if the element is present.
     * @param <X> the element type
     * @param elem the element to search for
     * @param lst the list to search in
     * @return true if present, false otherwise
     */
    public static <X> Boolean apply(X elem, List<X> lst) {
        return lst.contains(elem);
    }
}
