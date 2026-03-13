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
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Wraps a value in a flow.
 */
public class Pure extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.pure");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(Types.var("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> Either.right(Terms.list(apply(args.get(0))));
    }

    /**
     * Apply the function to the single argument.
     * @param <X> the element type
     * @param single the element to wrap in a list
     * @return a singleton list containing the element
     */
    public static <X> List<X> apply(X single) {
        return Collections.singletonList(single);
    }
}
