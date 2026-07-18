package hydra.overlay.java.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.error.system.SystemError;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.io.IOException;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.binary;
import static hydra.overlay.java.dsl.Types.either;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.unit;
import static hydra.overlay.java.dsl.Types.variable;
import hydra.core.Type;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Write raw bytes to standard output.
 */
public class WriteStdout extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.writeStdout().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(
            binary(),
            new Type.Effect(either(variable("hydra.error.system.SystemError"), unit()))));
    }

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
     * Write bytes to standard output.
     * @param bytes the bytes to write
     * @return right(unit) on success, or left(error) on a recoverable I/O failure
     */
    public static Either<SystemError, Void> apply(byte[] bytes) {
        try {
            System.out.write(bytes);
            System.out.flush();
            return Either.right(null);
        } catch (IOException e) {
            return Either.left(new SystemError.Other(
                e.getMessage() == null ? e.getClass().getSimpleName() : e.getMessage()));
        }
    }
}
