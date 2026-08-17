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
import java.nio.file.InvalidPathException;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Read the target of a symbolic link.
 */
public class ReadSymlink extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.files.readSymlink"
     */
    public Name name() {
        return hydra.lib.Files.readSymlink().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme FilePath -&gt; effect&lt;either&lt;FileError, FilePath&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.file.FilePath"),
            new Type.Effect(either(variable("hydra.error.file.FileError"), variable("hydra.file.FilePath")))));
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
     * Read the target of the symbolic link at path, verbatim and unresolved: a relative target
     * is returned relative, and a dangling target is returned as-is. Fails with invalidPath if
     * path exists but is not a symbolic link.
     * @param path the path of the symbolic link to read
     * @return right(target) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, FilePath> apply(FilePath path) {
        return FileErrors.withFileError(path, () -> {
            Path p = Paths.get(path.value);
            if (!Files.exists(p, LinkOption.NOFOLLOW_LINKS)) {
                throw new NoSuchFileException(path.value);
            }
            if (!Files.isSymbolicLink(p)) {
                throw new InvalidPathException(path.value, "not a symbolic link");
            }
            return new FilePath(Files.readSymbolicLink(p).toString());
        });
    }
}
