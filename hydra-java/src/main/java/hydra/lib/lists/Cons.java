package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Prepends an element to a list.
 */
public class Cons extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.lists.cons");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(l -> Terms.list(Cons.apply(args.get(0), l)), hydra.extract.Core.list(cx, graph, args.get(1)));
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the element type
     * @param el the element to prepend
     * @param l the list to prepend to
     * @return the list with the element prepended
     */
    public static <X> ConsList<X> apply(X el, ConsList<X> l) {
        return ConsList.cons(el, l);
    }
}
