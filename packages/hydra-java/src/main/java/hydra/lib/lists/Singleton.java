package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Creates a singleton list.
 */
public class Singleton extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.singleton");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> Either.right(Terms.list(apply(args.get(0))));
    }

    /**
     * Creates a list with one element.
     * @param <X> the element type
     * @param elem the element to wrap in a list
     * @return the singleton list
     */
    public static <X> List<X> apply(X elem) {
        return Collections.singletonList(elem);
    }
}
