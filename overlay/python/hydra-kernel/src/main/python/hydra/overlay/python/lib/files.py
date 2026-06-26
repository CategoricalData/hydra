"""Python implementations of hydra.lib.files primitives.

These are real file-system primitives. In Python the Hydra type ``effect<t>`` is
transparent (effect<t> = t), so each primitive performs its I/O eagerly and returns
an ``Either[FileError, T]``: a recoverable file-system failure is returned as
``Left(error)``; success is returned as ``Right(value)``. This mirrors the Haskell
reference implementation in Hydra.Haskell.Lib.Files, including the ``classify`` mapping
of OS errors to FileError variants. For #494.
"""

from __future__ import annotations
from collections.abc import Callable
from typing import TypeVar
import errno
import os

from hydra.overlay.python.dsl.python import Either, Left, Right
import hydra.file
import hydra.error.file as file_error

A = TypeVar("A")

FilePath = hydra.file.FilePath


def _classify(path: "hydra.file.FilePath", e: OSError) -> "file_error.FileError":
    """Map an OSError to a hydra.error.file.FileError variant (mirrors Haskell classify)."""
    if e.errno == errno.EEXIST:
        return file_error.FileErrorAlreadyExists(path)
    if e.errno in (errno.ENOENT, errno.ENOTDIR):
        return file_error.FileErrorNotFound(path)
    if e.errno in (errno.EACCES, errno.EPERM):
        return file_error.FileErrorPermissionDenied(path)
    if e.errno in (errno.ENAMETOOLONG, errno.EINVAL, errno.EISDIR):
        return file_error.FileErrorInvalidPath(e.strerror or str(e))
    return file_error.FileErrorOther(e.strerror or str(e))


def _with_file_error(path: "hydra.file.FilePath", action: Callable[[], A]) -> Either["file_error.FileError", A]:
    """Run an I/O action, returning Right on success or Left(classified error) on OSError."""
    try:
        return Right(action())
    except OSError as e:
        return Left(_classify(path, e))


def append_file(path: "hydra.file.FilePath", contents: bytes) -> Either["file_error.FileError", None]:
    """Append raw bytes to the end of a file, creating it if it does not exist."""
    def go() -> None:
        with open(path.value, "ab") as f:
            f.write(contents)
        return None
    return _with_file_error(path, go)


def create_directory(recursive: bool, path: "hydra.file.FilePath") -> Either["file_error.FileError", None]:
    """Create a directory. If recursive, create any missing parent directories as well."""
    def go() -> None:
        if recursive:
            os.makedirs(path.value, exist_ok=True)
        else:
            os.mkdir(path.value)
        return None
    return _with_file_error(path, go)


def exists(path: "hydra.file.FilePath") -> Either["file_error.FileError", bool]:
    """Test whether anything exists at the given path."""
    return _with_file_error(path, lambda: os.path.exists(path.value))


def list_directory(path: "hydra.file.FilePath") -> Either["file_error.FileError", tuple["hydra.file.FilePath", ...]]:
    """List the bare entry names of a directory (not full paths), mirroring Haskell listDirectory."""
    return _with_file_error(
        path, lambda: tuple(FilePath(name) for name in os.listdir(path.value)))


def read_file(path: "hydra.file.FilePath") -> Either["file_error.FileError", bytes]:
    """Read the entire contents of a file as raw bytes."""
    def go() -> bytes:
        with open(path.value, "rb") as f:
            return f.read()
    return _with_file_error(path, go)


def remove_file(path: "hydra.file.FilePath") -> Either["file_error.FileError", None]:
    """Remove a file."""
    def go() -> None:
        os.remove(path.value)
        return None
    return _with_file_error(path, go)


def rename(source: "hydra.file.FilePath", destination: "hydra.file.FilePath") -> Either["file_error.FileError", None]:
    """Rename (move) a file or directory from source to destination."""
    def go() -> None:
        os.rename(source.value, destination.value)
        return None
    return _with_file_error(source, go)


def write_file(path: "hydra.file.FilePath", contents: bytes) -> Either["file_error.FileError", None]:
    """Replace the file at path with the raw bytes contents, creating it if necessary."""
    def go() -> None:
        with open(path.value, "wb") as f:
            f.write(contents)
        return None
    return _with_file_error(path, go)
