package hydra.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.system.EnvironmentVariable;
import hydra.tools.PrimitiveFunction;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.map;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Get the full set of environment variables.
 */
public class GetEnvironment extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.getEnvironment().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(new Type.Effect(map(
            variable("hydra.system.EnvironmentVariable"), string())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
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
