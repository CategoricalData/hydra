package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Wraps a value in a flow.
 */
public class Pure extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.pure"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.pure");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for wrapping a value in an optional
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function("a", optional("a")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that wraps a value in an optional
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> Either.right(Terms.optional(apply(args.get(0))));
    }

    /**
     * Wraps a value in an optional (Just constructor).
     * @param <X> the value type
     * @param arg the value to wrap
     * @return an optional containing the value
     */
    public static <X> Maybe<X> apply(X arg) {
        return Maybe.just(arg);
    }
}
