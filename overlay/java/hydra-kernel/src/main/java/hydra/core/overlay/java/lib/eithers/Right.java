package hydra.core.overlay.java.lib.eithers;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Construct a Right (success/normal) Either value.
 */
public class Right extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.eithers.right");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(var("b"), either(var("a"), var("b"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(Terms.right(args.get(0)));
    }

    /**
     * Construct a Right (success/normal) Either value.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param value the value to wrap as a Right
     * @return an Either containing the Right value
     */
    public static <A, B> hydra.core.overlay.java.util.Either<A, B> apply(B value) {
        return hydra.core.overlay.java.util.Either.right(value);
    }
}
