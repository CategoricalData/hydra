package hydra.core.overlay.java.lib.equality;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeEq;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Tests equality.
 */
public class Equal extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Equality.equal().name;
    }

    @Override
    public TypeScheme type() {
        return schemeEq("x", function(Types.var("x"), Types.var("x"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        // Term equality is sufficient here, as we assume that type inference has already succeeded.
        return args -> graph -> Either.right(Terms.boolean_(args.get(0).equals(args.get(1))));
    }

    /**
     * Checks if two values are equal.
     * @param <A> the type
     * @param left the value1
     * @return true if equal, false otherwise
     */
    public static <A> Function<A, Boolean> apply(A left) {
        return right -> apply(left, right);
    }

    /**
     * Checks if two values are equal.
     * @param <A> the type
     * @param left the value1
     * @param right the value2
     * @return true if equal, false otherwise
     */
    public static <A> boolean apply(A left, A right) {
        return left.equals(right);
    }
}
