package hydra.core.overlay.java.lib.system;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.error.system.SystemError;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.binary;
import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.model.Type;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Read the complete contents of standard input as raw bytes.
 */
public class ReadStdin extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.System_.readStdin().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(new Type.Effect(either(
            variable("hydra.core.error.system.SystemError"),
            binary())));
    }

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
     * Read standard input until end-of-file, returning the complete contents as raw bytes.
     * @return right(contents) on success, or left(error) on a recoverable I/O failure
     */
    public static Either<SystemError, byte[]> apply() {
        try {
            ByteArrayOutputStream buffer = new ByteArrayOutputStream();
            byte[] chunk = new byte[8192];
            int n;
            while ((n = System.in.read(chunk)) != -1) {
                buffer.write(chunk, 0, n);
            }
            return Either.right(buffer.toByteArray());
        } catch (IOException e) {
            return Either.left(new SystemError.Other(
                e.getMessage() == null ? e.getClass().getSimpleName() : e.getMessage()));
        }
    }
}
