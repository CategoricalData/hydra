package hydra.lib.system;

import hydra.error.system.SystemError;
import hydra.file.FilePath;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.NoSuchFileException;
import java.nio.file.NotDirectoryException;

/**
 * Shared helpers for the hydra.lib.system primitives. Not a primitive itself.
 * Mirrors the {@code classify} helper of the Haskell host.
 */
final class SystemErrors {
    private SystemErrors() {
    }

    /**
     * Classify a process-launch IOException into a SystemError, mirroring the Haskell host's
     * {@code classify}. Only launch failures reach here; a child that runs and exits non-zero is a
     * ProcessResult, not a SystemError.
     * @param program the program whose launch failed
     * @param e the IOException thrown by ProcessBuilder.start()
     * @return the corresponding SystemError
     */
    static SystemError classify(FilePath program, IOException e) {
        if (e instanceof NoSuchFileException || e instanceof FileNotFoundException) {
            return new SystemError.CommandNotFound(program);
        }
        if (e instanceof NotDirectoryException) {
            return new SystemError.InvalidWorkingDirectory(program);
        }
        if (e instanceof AccessDeniedException) {
            return new SystemError.PermissionDenied(program);
        }
        // ProcessBuilder.start() typically throws a plain IOException whose message carries the POSIX
        // errno (e.g. "error=2, No such file or directory" / "error=13, Permission denied") rather than
        // an nio exception subclass; inspect the message to recover the launch-failure variant.
        String msg = message(e);
        String lower = msg.toLowerCase();
        if (lower.contains("error=2") || lower.contains("no such file")) {
            return new SystemError.CommandNotFound(program);
        }
        if (lower.contains("error=13") || lower.contains("permission denied")) {
            return new SystemError.PermissionDenied(program);
        }
        if (lower.contains("error=20") || lower.contains("not a directory")) {
            return new SystemError.InvalidWorkingDirectory(program);
        }
        return new SystemError.Other(msg);
    }

    private static String message(Exception e) {
        String msg = e.getMessage();
        return msg == null ? e.getClass().getSimpleName() : msg;
    }
}
