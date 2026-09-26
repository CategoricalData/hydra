package hydra.core.overlay.java.lib.files;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.error.file.FileError;
import hydra.core.file.FilePath;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.binary;
import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Read the complete contents of a file as raw bytes.
 */
public class ReadFile extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.files.readFile"
     */
    public Name name() {
        return hydra.core.lib.Files.readFile().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme FilePath -&gt; effect&lt;either&lt;FileError, binary&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.core.file.FilePath"),
            new Type.Effect(either(variable("hydra.core.error.file.FileError"), binary()))));
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
     * Read the entire contents of a file as raw bytes.
     * @param path the path to read
     * @return right(contents) on success, or left(error) on a recoverable file-system failure
     */
    public static Either<FileError, byte[]> apply(FilePath path) {
        return FileErrors.withFileError(path, () -> Files.readAllBytes(Paths.get(path.value)));
    }
}
