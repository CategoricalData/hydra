package hydra.core.overlay.java.lib.system;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.system.EnvironmentVariable;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Look up a single environment variable by name.
 */
public class GetEnvironmentVariable extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.System_.getEnvironmentVariable().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.core.system.EnvironmentVariable"),
            new Type.Effect(optional(string()))));
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
     * Return the value of the named environment variable, or none if it is not set.
     * @param name the variable name
     * @return given(value) if set, otherwise none
     */
    public static hydra.core.overlay.java.util.Optional<String> apply(EnvironmentVariable name) {
        String value = System.getenv(name.value);
        return value == null ? hydra.core.overlay.java.util.Optional.none() : hydra.core.overlay.java.util.Optional.given(value);
    }
}
