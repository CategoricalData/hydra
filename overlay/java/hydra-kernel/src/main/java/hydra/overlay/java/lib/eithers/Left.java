package hydra.overlay.java.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Construct a Left (error/exceptional) Either value.
 */
public class Left extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.left");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(var("a"), either(var("a"), var("b"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.left(args.get(0)));
    }

    /**
     * Construct a Left (error/exceptional) Either value.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param value the value to wrap as a Left
     * @return an Either containing the Left value
     */
    public static <A, B> hydra.overlay.java.util.Either<A, B> apply(A value) {
        return hydra.overlay.java.util.Either.left(value);
    }
}
