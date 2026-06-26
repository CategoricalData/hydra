package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Wraps a value in a flow.
 */
public class Pure extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.pure().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.list(apply(args.get(0))));
    }

    /**
     * Apply the function to the single argument.
     * @param <X> the element type
     * @param single the element to wrap in a list
     * @return a singleton list containing the element
     */
    public static <X> List<X> apply(X single) {
        return ConsList.singleton(single);
    }
}
