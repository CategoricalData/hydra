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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * List the immediate entries of a directory.
 */
public class ListDirectory extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.files.listDirectory"
     */
    public Name name() {
        return hydra.lib.Files.listDirectory().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme FilePath -&gt; effect&lt;either&lt;FileError, [FilePath]&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.file.FilePath"),
            new Type.Effect(either(
                variable("hydra.error.file.FileError"),
                list(variable("hydra.file.FilePath"))))));
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

    /**
     * Return the immediate entries of a directory, excluding "." and "..". The result is unordered.
     * @param path the directory to list
     * @return right(entries) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, List<FilePath>> apply(FilePath path) {
        return FileErrors.withFileError(path, () -> {
            List<FilePath> entries = new ArrayList<FilePath>();
            try (Stream<Path> stream = Files.list(Paths.get(path.value))) {
                stream.forEach(p -> entries.add(new FilePath(p.getFileName().toString())));
            }
            return entries;
        });
    }
}
