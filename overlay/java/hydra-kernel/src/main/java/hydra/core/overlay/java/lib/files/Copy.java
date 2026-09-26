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
import java.nio.file.StandardCopyOption;
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
 * Copy a file, or a directory tree, to a destination path.
 */
public class Copy extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.files.copy"
     */
    public Name name() {
        return hydra.core.lib.Files.copy().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme boolean -&gt; FilePath -&gt; FilePath -&gt; effect&lt;either&lt;FileError, unit&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            boolean_(),
            variable("hydra.core.file.FilePath"),
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
     * Copy source to destination. When recursive is false, source must be a single file. When
     * recursive is true, source may be a directory, whose entire tree is copied.
     * @param recursive whether source may be a directory whose tree is copied
     * @param source the path to copy from
     * @param destination the path to copy to
     * @return right(unit) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, Void> apply(Boolean recursive, FilePath source, FilePath destination) {
        return FileErrors.withFileError(source, () -> {
            Path sourcePath = Paths.get(source.value);
            Path destinationPath = Paths.get(destination.value);
            if (recursive && Files.isDirectory(sourcePath)) {
                copyDirectoryRecursive(sourcePath, destinationPath);
            } else {
                if (Files.isDirectory(sourcePath)) {
                    throw new java.nio.file.FileSystemException(source.value, null,
                        "is a directory, but recursive is false");
                }
                Files.copy(sourcePath, destinationPath, StandardCopyOption.REPLACE_EXISTING);
            }
            return (Void) null;
        });
    }

    private static void copyDirectoryRecursive(Path source, Path destination) throws IOException {
        Files.walkFileTree(source, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                Files.createDirectories(destination.resolve(source.relativize(dir)));
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.copy(file, destination.resolve(source.relativize(file)), StandardCopyOption.REPLACE_EXISTING);
                return FileVisitResult.CONTINUE;
            }
        });
    }
}
