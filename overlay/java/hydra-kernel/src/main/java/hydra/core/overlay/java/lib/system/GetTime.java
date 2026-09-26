package hydra.core.overlay.java.lib.system;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.time.Timespec;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.time.Instant;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Get the current wall-clock time.
 */
public class GetTime extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.System_.getTime().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(new Type.Effect(variable("hydra.core.time.Timespec")));
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
     * Return the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix epoch).
     * @return the current time
     */
    public static Timespec apply() {
        Instant now = Instant.now();
        return new Timespec((long) now.getEpochSecond(), (long) now.getNano());
    }
}
