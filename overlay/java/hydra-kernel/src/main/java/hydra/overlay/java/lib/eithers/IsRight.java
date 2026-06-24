package hydra.overlay.java.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Check if an Either value is a Right.
 */
public class IsRight extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.isRight");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(either(var("a"), var("b")), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(e -> Terms.boolean_(apply(e)), hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Check if an Either value is a Right.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param either the Either value to check
     * @return true if the Either is a Right, false otherwise
     */
    public static <A, B> Boolean apply(hydra.overlay.java.util.Either<A, B> either) {
        return either.isRight();
    }
}
