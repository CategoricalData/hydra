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
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Returns its argument unchanged.
 */
public class Identity extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Functions.identity().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function("x", "x"));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.right(args.get(0));
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
