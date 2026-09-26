package hydra.core.overlay.java.lib.files;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.error.file.FileError;
import hydra.core.file.FilePath;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.unit;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Remove a directory.
 */
public class RemoveDirectory extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.files.removeDirectory"
     */
    public Name name() {
        return hydra.core.lib.Files.removeDirectory().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme boolean -&gt; FilePath -&gt; effect&lt;either&lt;FileError, unit&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            boolean_(),
            variable("hydra.core.file.FilePath"),
            new Type.Effect(either(variable("hydra.core.error.file.FileError"), unit()))));
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
            new hydra.core.errors.Error_.Other(new hydra.core.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    @Override
    protected boolean isPure() {
        return false;
    }

    /**
     * Remove the directory at path. When recursive is false this corresponds to POSIX rmdir: it
     * fails unless the directory is empty. When recursive is true, the directory and its entire
     * contents are removed.
     * @param recursive whether to remove the directory's entire contents (rm -r)
     * @param path the directory to remove
     * @return right(unit) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, Void> apply(Boolean recursive, FilePath path) {
        return FileErrors.withFileError(path, () -> {
            if (recursive) {
                removeDirectoryRecursive(Paths.get(path.value));
            } else {
                Files.delete(Paths.get(path.value));
            }
            return (Void) null;
        });
    }

    private static void removeDirectoryRecursive(Path path) throws IOException {
        Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.delete(file);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                Files.delete(dir);
                return FileVisitResult.CONTINUE;
            }
        });
    }
}
