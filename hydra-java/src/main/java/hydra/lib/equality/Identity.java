package hydra.lib.equality;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Returns its argument unchanged.
 */
public class Identity extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.equality.identity");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function("x", "x"));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> Either.right(args.get(0));
    }

    /**
     * Identity function.
     * @param <X> the type
     * @param object the value
     * @return the same value
     */
    public static <X> X apply(X object) {
        return object;
    }
}
