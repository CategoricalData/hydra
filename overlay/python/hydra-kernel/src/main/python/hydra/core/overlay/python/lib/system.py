"""Python implementations of hydra.core.lib.system primitives.

These are real system-interface primitives. In Python the Hydra type ``effect<t>`` is
transparent (effect<t> = t), so each primitive performs its effect eagerly. Fallible
primitives return an ``Either[SystemError, T]``: a failure to launch (or perform the call)
is returned as ``Left(error)``; success is returned as ``Right(value)``. Infallible
primitives (``get_environment``, ``get_environment_variable``, ``get_time``) return their
value directly. This mirrors the Haskell reference implementation in
Hydra.Haskell.Lib.System. For #498.
"""

from __future__ import annotations
import os
import subprocess
import sys
import time

from hydra.core.overlay.python.dsl.python import Either, Left, Right, Given, None_
from hydra.core.overlay.python.util import PersistentMap
import hydra.core.system
import hydra.core.error.system as system_error
import hydra.core.file
import hydra.core.time

Command = hydra.core.system.Command
ProcessResult = hydra.core.system.ProcessResult
StatusCode = hydra.core.system.StatusCode
EnvironmentVariable = hydra.core.system.EnvironmentVariable
FilePath = hydra.core.file.FilePath
Timespec = hydra.core.time.Timespec


def execute(command: "hydra.core.system.Command") -> Either["system_error.SystemError", "hydra.core.system.ProcessResult"]:
    """Run a program to completion and capture its result.

    A child that runs and exits non-zero is returned as Right(result) with that exit code; only a
    failure to launch is Left(error). No shell is invoked; the program is executed directly.
    """
    program = command.program.value
    args = list(command.arguments)
    # working_directory and environment are Hydra optionals: Given(x) or None_().
    cwd = command.working_directory.value.value if isinstance(command.working_directory, Given) else None
    env = None
    if isinstance(command.environment, Given):
        env = {k.value: v for k, v in command.environment.value.items()}
    try:
        completed = subprocess.run(
            [program, *args],
            cwd=cwd,
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False)
    except FileNotFoundError:
        return Left(system_error.SystemErrorCommandNotFound(command.program))
    except NotADirectoryError:
        return Left(system_error.SystemErrorInvalidWorkingDirectory(command.program))
    except PermissionError:
        return Left(system_error.SystemErrorPermissionDenied(command.program))
    except OSError as e:
        return Left(system_error.SystemErrorOther(e.strerror or str(e)))
    return Right(ProcessResult(
        exit_code=StatusCode(completed.returncode),
        stdout=completed.stdout,
        stderr=completed.stderr))


def exit(code: "hydra.core.system.StatusCode") -> None:
    """Terminate the current process with the given status code. Does not return."""
    import sys
    sys.exit(code.value)


def get_environment():
    """Get the full set of environment variables, as a map from variable name to value."""
    return PersistentMap.from_pairs((EnvironmentVariable(k), v) for k, v in os.environ.items())


def get_environment_variable(name: "hydra.core.system.EnvironmentVariable"):
    """Look up a single environment variable by name; none if it is not set.

    Returns a Hydra optional (Given(value) or None_()).
    """
    value = os.environ.get(name.value)
    return None_() if value is None else Given(value)


def get_time() -> "hydra.core.time.Timespec":
    """Get the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix epoch)."""
    nanos_total = time.time_ns()
    return Timespec(seconds=nanos_total // 1_000_000_000, nanoseconds=nanos_total % 1_000_000_000)


def get_working_directory() -> Either["system_error.SystemError", "hydra.core.file.FilePath"]:
    """Get the current working directory as a FilePath."""
    try:
        return Right(FilePath(os.getcwd()))
    except OSError as e:
        return Left(system_error.SystemErrorOther(e.strerror or str(e)))


def read_stdin() -> Either["system_error.SystemError", bytes]:
    """Read standard input until end-of-file, returning the complete contents as raw bytes."""
    try:
        return Right(sys.stdin.buffer.read())
    except OSError as e:
        return Left(system_error.SystemErrorOther(e.strerror or str(e)))


def write_stderr(data: bytes) -> Either["system_error.SystemError", None]:
    """Write bytes to standard error."""
    try:
        sys.stderr.buffer.write(data)
        sys.stderr.buffer.flush()
        return Right(None)
    except OSError as e:
        return Left(system_error.SystemErrorOther(e.strerror or str(e)))


def write_stdout(data: bytes) -> Either["system_error.SystemError", None]:
    """Write bytes to standard output."""
    try:
        sys.stdout.buffer.write(data)
        sys.stdout.buffer.flush()
        return Right(None)
    except OSError as e:
        return Left(system_error.SystemErrorOther(e.strerror or str(e)))
