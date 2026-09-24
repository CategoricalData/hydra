package hydra.core.overlay.java.lib.pairs;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.pair;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Construct a pair from two values.
 */
public class Pair extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.pairs.pair");

    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return NAME;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(variable("a"), variable("b"), pair(variable("a"), variable("b"))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.pair(args.get(0), args.get(1)));
    }

    /**
     * Construct a pair from two values.
     * @param <A> the first element type
     * @param <B> the second element type
     * @param x the first element
     * @param y the second element
     * @return the pair (x, y)
     */
    public static <A, B> hydra.core.overlay.java.util.Pair<A, B> apply(A x, B y) {
        return new hydra.core.overlay.java.util.Pair<>(x, y);
    }
}
