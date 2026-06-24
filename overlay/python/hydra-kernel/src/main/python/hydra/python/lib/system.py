"""Python implementations of hydra.lib.system primitives.

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
import time

from hydra.dsl.python import Either, Left, Right, Given, None_
from hydra.python.util import PersistentMap
import hydra.system
import hydra.error.system as system_error
import hydra.file
import hydra.time

Command = hydra.system.Command
ProcessResult = hydra.system.ProcessResult
StatusCode = hydra.system.StatusCode
EnvironmentVariable = hydra.system.EnvironmentVariable
FilePath = hydra.file.FilePath
Timespec = hydra.time.Timespec


def execute(command: "hydra.system.Command") -> Either["system_error.SystemError", "hydra.system.ProcessResult"]:
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


def exit(code: "hydra.system.StatusCode") -> None:
    """Terminate the current process with the given status code. Does not return."""
    import sys
    sys.exit(code.value)


def get_environment():
    """Get the full set of environment variables, as a map from variable name to value."""
    return PersistentMap.from_pairs((EnvironmentVariable(k), v) for k, v in os.environ.items())


def get_environment_variable(name: "hydra.system.EnvironmentVariable"):
    """Look up a single environment variable by name; none if it is not set.

    Returns a Hydra optional (Given(value) or None_()).
    """
    value = os.environ.get(name.value)
    return None_() if value is None else Given(value)


def get_time() -> "hydra.time.Timespec":
    """Get the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix epoch)."""
    nanos_total = time.time_ns()
    return Timespec(seconds=nanos_total // 1_000_000_000, nanoseconds=nanos_total % 1_000_000_000)


def get_working_directory() -> Either["system_error.SystemError", "hydra.file.FilePath"]:
    """Get the current working directory as a FilePath."""
    try:
        return Right(FilePath(os.getcwd()))
    except OSError as e:
        return Left(system_error.SystemErrorOther(e.strerror or str(e)))
