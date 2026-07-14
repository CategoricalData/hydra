package hydra.overlay.java.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.error.system.SystemError;
import hydra.file.FilePath;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Get the current working directory.
 */
public class GetWorkingDirectory extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.getWorkingDirectory().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(new Type.Effect(either(
            variable("hydra.error.system.SystemError"),
            variable("hydra.file.FilePath"))));
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
     * Return the current working directory as a FilePath.
     * @return right(path) on success, or left(error) on failure
     */
    public static Either<SystemError, FilePath> apply() {
        try {
            String cwd = System.getProperty("user.dir");
            if (cwd == null) {
                return Either.left(new SystemError.Other("user.dir is not set"));
            }
            return Either.right(new FilePath(cwd));
        } catch (SecurityException e) {
            return Either.left(new SystemError.Other(
                e.getMessage() == null ? e.getClass().getSimpleName() : e.getMessage()));
        }
    }
}
