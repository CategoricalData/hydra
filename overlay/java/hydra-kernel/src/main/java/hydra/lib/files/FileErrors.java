package hydra.lib.files;

import hydra.error.file.FileError;
import hydra.file.FilePath;
import hydra.util.Either;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.InvalidPathException;
import java.nio.file.NoSuchFileException;
import java.util.concurrent.Callable;

/**
 * Shared helpers for the hydra.lib.files primitives. Not a primitive itself.
 * Mirrors the {@code withFileError}/{@code classify} helpers of the Haskell host.
 */
final class FileErrors {
    private FileErrors() {
    }

    /**
     * Run a file-system action, translating any IOException into the appropriate FileError.
     * @param <T> the success type
     * @param path the path the action operates on (used to build path-carrying errors)
     * @param action the I/O action
     * @return right(result) on success, or left(error) on a recoverable file-system failure
     */
    static <T> Either<FileError, T> withFileError(FilePath path, Callable<T> action) {
        try {
            return Either.right(action.call());
        } catch (InvalidPathException e) {
            return Either.left(new FileError.InvalidPath(message(e)));
        } catch (IOException e) {
            return Either.left(classify(path, e));
        } catch (Exception e) {
            return Either.left(new FileError.Other(message(e)));
        }
    }

    /**
     * Classify an IOException into a FileError, mirroring the Haskell host's {@code classify}.
     * @param path the path the failing action operated on
     * @param e the IOException to classify
     * @return the corresponding FileError
     */
    static FileError classify(FilePath path, IOException e) {
        if (e instanceof FileAlreadyExistsException) {
            return new FileError.AlreadyExists(path);
        }
        if (e instanceof NoSuchFileException || e instanceof FileNotFoundException) {
            return new FileError.NotFound(path);
        }
        if (e instanceof AccessDeniedException) {
            return new FileError.PermissionDenied(path);
        }
        return new FileError.Other(message(e));
    }

    private static String message(Exception e) {
        String msg = e.getMessage();
        return msg == null ? e.getClass().getSimpleName() : msg;
    }
}
