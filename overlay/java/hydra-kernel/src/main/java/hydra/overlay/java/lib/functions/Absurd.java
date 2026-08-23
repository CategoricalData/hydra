package hydra.overlay.java.lib.functions;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.void_;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Eliminates a value of the uninhabited void type, producing any type. Unreachable in any
 * well-typed program.
 */
public class Absurd extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Functions.absurd().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(void_(), "x"));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            throw new IllegalStateException("hydra.lib.functions.absurd: void has no inhabitants");
        };
    }

    /**
     * Eliminate a value of the uninhabited void type.
     * @param <X> the value's static (void) type
     * @param <Y> the result type
     * @param voidValue a value of the void type; unreachable in any well-typed program
     * @return never returns
     */
    public static <X, Y> Y apply(X voidValue) {
        throw new IllegalStateException("hydra.lib.functions.absurd: void has no inhabitants");
    }
}
