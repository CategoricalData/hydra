package hydra.overlay.java.lib.files;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.error.file.FileError;
import hydra.file.FilePath;
import hydra.file.FileStatus;
import hydra.file.FileType;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;
import hydra.time.Timespec;

import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Retrieve metadata about a file or directory.
 */
public class Status extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.files.status"
     */
    public Name name() {
        return hydra.lib.Files.status().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme FilePath -&gt; effect&lt;either&lt;FileError, FileStatus&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.file.FilePath"),
            new hydra.core.Type.Effect(either(variable("hydra.error.file.FileError"), variable("hydra.file.FileStatus")))));
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
     * Retrieve metadata about the file at path (POSIX stat). Symbolic links are followed.
     * @param path the path to inspect
     * @return right(status) on success, or left(notFound) if path does not exist, or
     *   left(error) on another recoverable file-system failure
     */
    public static Either<FileError, FileStatus> apply(FilePath path) {
        return FileErrors.withFileError(path, () -> {
            BasicFileAttributes attrs = Files.readAttributes(
                Paths.get(path.value), BasicFileAttributes.class, new LinkOption[0]);
            return new FileStatus(
                fileType(attrs),
                attrs.size(),
                timespec(attrs.lastModifiedTime()),
                Optional.given(timespec(attrs.lastAccessTime())),
                Optional.<Timespec>none());
        });
    }

    private static FileType fileType(BasicFileAttributes attrs) {
        if (attrs.isDirectory()) {
            return new FileType.Directory();
        } else if (attrs.isSymbolicLink()) {
            return new FileType.Link();
        } else if (attrs.isRegularFile()) {
            return new FileType.Regular();
        } else {
            return new FileType.Regular();
        }
    }

    private static Timespec timespec(FileTime time) {
        java.time.Instant instant = time.toInstant();
        return new Timespec(instant.getEpochSecond(), (long) instant.getNano());
    }
}
