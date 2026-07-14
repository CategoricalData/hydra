package hydra.overlay.java.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.time.Timespec;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.time.Instant;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Get the current wall-clock time.
 */
public class GetTime extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.getTime().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(new Type.Effect(variable("hydra.time.Timespec")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
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
