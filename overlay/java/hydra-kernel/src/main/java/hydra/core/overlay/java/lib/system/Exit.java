package hydra.core.overlay.java.lib.system;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.system.StatusCode;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.unit;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Terminate the current process with a status code.
 */
public class Exit extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.System_.exit().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.core.system.StatusCode"),
            new Type.Effect(unit())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.core.errors.Error_.Other(new hydra.core.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    @Override
    protected boolean isPure() {
        return false;
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
