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
import java.nio.file.attribute.FileTime;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
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
     * @return the type scheme boolean -&gt; FilePath -&gt; effect&lt;either&lt;FileError, FileStatus&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            boolean_(),
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

    // POSIX st_mode file-type bits (<sys/stat.h> S_IFMT and the individual S_IF* macros).
    private static final int S_IFMT   = 0170000;
    private static final int S_IFSOCK = 0140000;
    private static final int S_IFLNK  = 0120000;
    private static final int S_IFREG  = 0100000;
    private static final int S_IFBLK  = 0060000;
    private static final int S_IFDIR  = 0040000;
    private static final int S_IFCHR  = 0020000;
    private static final int S_IFIFO  = 0010000;

    /**
     * Retrieve metadata about the file at path. When followLinks is true, this corresponds to
     * POSIX stat: a symbolic link's metadata is that of its target, and a dangling link yields
     * notFound. When false, this corresponds to POSIX lstat: a symbolic link's own metadata is
     * reported, with fileType link, and a dangling link is not an error.
     * @param followLinks whether to resolve symbolic links
     * @param path the path to inspect
     * @return right(status) on success, or left(notFound) if path does not exist, or
     *   left(error) on another recoverable file-system failure
     */
    public static Either<FileError, FileStatus> apply(Boolean followLinks, FilePath path) {
        return FileErrors.withFileError(path, () -> {
            LinkOption[] options = followLinks ? new LinkOption[0]
                : new LinkOption[] { LinkOption.NOFOLLOW_LINKS };
            Map<String, Object> attrs = Files.readAttributes(Paths.get(path.value), "unix:*", options);
            int mode = (Integer) attrs.get("mode");
            long size = (Long) attrs.get("size");
            FileTime mtime = (FileTime) attrs.get("lastModifiedTime");
            FileTime atime = (FileTime) attrs.get("lastAccessTime");
            FileTime ctime = (FileTime) attrs.get("ctime");
            return new FileStatus(
                fileType(mode),
                size,
                timespec(mtime),
                Optional.given(timespec(atime)),
                Optional.given(timespec(ctime)));
        });
    }

    private static FileType fileType(int mode) {
        switch (mode & S_IFMT) {
            case S_IFDIR:  return new FileType.Directory();
            case S_IFLNK:  return new FileType.Link();
            case S_IFBLK:  return new FileType.Block();
            case S_IFCHR:  return new FileType.Character_();
            case S_IFIFO:  return new FileType.Fifo();
            case S_IFSOCK: return new FileType.Socket();
            case S_IFREG:
            default:       return new FileType.Regular();
        }
    }

    private static Timespec timespec(FileTime time) {
        java.time.Instant instant = time.toInstant();
        return new Timespec(instant.getEpochSecond(), (long) instant.getNano());
    }
}
