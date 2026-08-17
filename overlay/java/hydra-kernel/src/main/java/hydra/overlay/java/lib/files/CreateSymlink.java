package hydra.overlay.java.lib.files;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.error.file.FileError;
import hydra.file.FilePath;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.unit;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Create a symbolic link.
 */
public class CreateSymlink extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.files.createSymlink"
     */
    public Name name() {
        return hydra.lib.Files.createSymlink().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme FilePath -&gt; FilePath -&gt; effect&lt;either&lt;FileError, unit&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.file.FilePath"),
            variable("hydra.file.FilePath"),
            new Type.Effect(either(variable("hydra.error.file.FileError"), unit()))));
    }

    /**
     * Provides the implementation of this primitive function.
     * Effectful primitives are evaluated through the native (host) path; the
     * term-level interpreter cannot reduce them, so this returns a function that
     * yields a deferred error when applied.
     * @return a function that yields a deferred error on reduction
     */
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
     * Create a symbolic link at link, pointing to target. target is stored verbatim (may be
     * relative, need not exist). No force flag: an occupied link path (including a dangling
     * symbolic link) fails with alreadyExists.
     * @param target the path the new symbolic link will point to
     * @param link the path of the symbolic link to create
     * @return right(unit) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, Void> apply(FilePath target, FilePath link) {
        return FileErrors.withFileError(link, () -> {
            Files.createSymbolicLink(Paths.get(link.value), Paths.get(target.value));
            return (Void) null;
        });
    }
}
