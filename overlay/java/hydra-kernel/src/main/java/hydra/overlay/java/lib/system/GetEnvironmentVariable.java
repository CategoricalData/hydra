package hydra.overlay.java.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.system.EnvironmentVariable;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Look up a single environment variable by name.
 */
public class GetEnvironmentVariable extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.getEnvironmentVariable().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.system.EnvironmentVariable"),
            new Type.Effect(optional(string()))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    /**
     * Return the value of the named environment variable, or none if it is not set.
     * @param name the variable name
     * @return given(value) if set, otherwise none
     */
    public static hydra.overlay.java.util.Optional<String> apply(EnvironmentVariable name) {
        String value = System.getenv(name.value);
        return value == null ? hydra.overlay.java.util.Optional.none() : hydra.overlay.java.util.Optional.given(value);
    }
}
