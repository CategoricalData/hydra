package hydra.core.overlay.java.lib.system;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.system.EnvironmentVariable;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.map;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Get the full set of environment variables.
 */
public class GetEnvironment extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.System_.getEnvironment().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(new Type.Effect(map(
            variable("hydra.core.system.EnvironmentVariable"), string())));
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
     * Return the entire environment of the current process as a map from variable name to value.
     * @return the environment map
     */
    public static Map<EnvironmentVariable, String> apply() {
        Map<EnvironmentVariable, String> result = new LinkedHashMap<>();
        for (Map.Entry<String, String> e : System.getenv().entrySet()) {
            result.put(new EnvironmentVariable(e.getKey()), e.getValue());
        }
        return result;
    }
}
