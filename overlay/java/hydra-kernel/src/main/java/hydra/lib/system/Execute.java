package hydra.lib.system;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.error.system.SystemError;
import hydra.graph.Graph;
import hydra.system.Command;
import hydra.system.ProcessResult;
import hydra.system.StatusCode;
import hydra.tools.PrimitiveFunction;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Run a program to completion and capture its result.
 */
public class Execute extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.System_.execute().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(
            variable("hydra.system.Command"),
            new Type.Effect(either(
                variable("hydra.error.system.SystemError"),
                variable("hydra.system.ProcessResult")))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    /**
     * Run the program described by command to completion, capturing stdout, stderr, and the exit code.
     * A child that runs and exits non-zero is returned as right(result); only a failure to launch is
     * left(error). No shell is invoked; the program is executed directly.
     * @param command the command to run
     * @return right(result) on launch+completion, or left(error) on a launch failure
     */
    public static Either<SystemError, ProcessResult> apply(Command command) {
        String program = command.program.value;
        java.util.List<String> commandLine = new java.util.ArrayList<>();
        commandLine.add(program);
        commandLine.addAll(command.arguments);
        ProcessBuilder pb = new ProcessBuilder(commandLine);
        if (command.workingDirectory.isGiven()) {
            pb.directory(new File(command.workingDirectory.fromGiven().value));
        }
        if (command.environment.isGiven()) {
            Map<String, String> env = pb.environment();
            env.clear();
            for (Map.Entry<hydra.system.EnvironmentVariable, String> e : command.environment.fromGiven().entrySet()) {
                env.put(e.getKey().value, e.getValue());
            }
        }
        try {
            Process process = pb.start();
            byte[] out = readAll(process.getInputStream());
            byte[] err = readAll(process.getErrorStream());
            int code = process.waitFor();
            return Either.right(new ProcessResult(new StatusCode(code), out, err));
        } catch (IOException e) {
            return Either.left(SystemErrors.classify(command.program, e));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return Either.left(new SystemError.Interrupted());
        }
    }

    private static byte[] readAll(InputStream in) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        byte[] chunk = new byte[8192];
        int n;
        while ((n = in.read(chunk)) != -1) {
            buffer.write(chunk, 0, n);
        }
        return buffer.toByteArray();
    }
}
