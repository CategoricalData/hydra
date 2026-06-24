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
 * Creates a singleton list.
 */
public class Singleton extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.singleton().name;
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
     * Creates a list with one element.
     * @param <X> the element type
     * @param elem the element to wrap in a list
     * @return the singleton list
     */
    public static <X> List<X> apply(X elem) {
        return ConsList.singleton(elem);
    }
}
