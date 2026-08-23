package hydra.overlay.java.lib.pairs;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.pair;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Construct a pair from two values.
 */
public class Pair extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.pairs.pair");

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
    public static <A, B> hydra.overlay.java.util.Pair<A, B> apply(A x, B y) {
        return new hydra.overlay.java.util.Pair<>(x, y);
    }
}
