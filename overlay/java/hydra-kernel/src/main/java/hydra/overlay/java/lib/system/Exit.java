package hydra.overlay.java.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.system.StatusCode;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.unit;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Terminate the current process with a status code.
 */
public class Exit extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.exit().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.system.StatusCode"),
            new Type.Effect(unit())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    /**
     * Terminate the current process immediately with the given status. Does not return.
     * @param code the exit status
     * @return never returns
     */
    public static Void apply(StatusCode code) {
        System.exit(code.value);
        return null;
    }
}
