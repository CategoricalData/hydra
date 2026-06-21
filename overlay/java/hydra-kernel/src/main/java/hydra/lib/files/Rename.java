package hydra.lib.files;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.error.file.FileError;
import hydra.file.FilePath;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.unit;
import static hydra.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Rename or move a file or directory.
 */
public class Rename extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.files.rename"
     */
    public Name name() {
        return hydra.lib.Files.rename().name;
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

    /**
     * Rename or move a file or directory from source to destination.
     * @param source the path to rename
     * @param destination the new path
     * @return right(unit) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, Void> apply(FilePath source, FilePath destination) {
        return FileErrors.withFileError(source, () -> {
            Files.move(Paths.get(source.value), Paths.get(destination.value));
            return (Void) null;
        });
    }
}
