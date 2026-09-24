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
 * Prepends an element to a list.
 */
public class Cons extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.lists.cons");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(l -> Terms.list(Cons.apply(args.get(0), l)), hydra.core.extract.Core.list(graph, args.get(1)));
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the element type
     * @param el the element to prepend
     * @param l the list to prepend to
     * @return the list with the element prepended
     */
    public static <X> List<X> apply(X el, List<X> l) {
        return ConsList.cons(el, ConsList.fromList(l));
    }
}
