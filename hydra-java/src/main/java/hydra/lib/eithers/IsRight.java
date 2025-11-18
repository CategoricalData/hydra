package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
            Expect.<Graph, Term, Term>either(args.get(0)),
            e -> Terms.boolean_(apply(e)));
    }

    /**
     * Check if an Either value is a Right.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param either the Either value to check
     * @return true if the Either is a Right, false otherwise
     */
    public static <A, B> Boolean apply(hydra.util.Either<A, B> either) {
        return either.isRight();
    }
}
