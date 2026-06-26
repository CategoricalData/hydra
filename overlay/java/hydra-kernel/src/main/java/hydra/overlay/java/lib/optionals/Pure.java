package hydra.overlay.java.lib.optionals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Wraps a value in a flow.
 */
public class Pure extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.optionals.pure"
     */
    public Name name() {
        return hydra.lib.Optionals.pure().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.optional(apply(args.get(0))));
    }

    /**
     * Wraps a value in an optional (Just constructor).
     * @param <X> the value type
     * @param arg the value to wrap
     * @return an optional containing the value
     */
    public static <X> Optional<X> apply(X arg) {
        return Optional.given(arg);
    }
}
