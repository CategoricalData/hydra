"""Python implementations of hydra.lib.text primitives.

UTF-8 codec primitives bridging Hydra strings and raw bytes (binary). These pair with
hydra.lib.files.readFile / writeFile, which are byte-oriented. For #494.
"""

from __future__ import annotations
from hydra.dsl.python import Either, Left, Right


def decode_utf8(data: bytes) -> Either[str, str]:
    """Decode a sequence of bytes as UTF-8 text.

    Returns Right(text) on success, or Left(message) if the bytes are not valid UTF-8,
    where message is a host-provided description of the decoding failure.
    """
    try:
        return Right(data.decode("utf-8"))
    except UnicodeDecodeError as e:
        return Left(str(e))


def encode_utf8(text: str) -> bytes:
    """Encode text as a sequence of UTF-8 bytes. Total: every Hydra string is valid Unicode."""
    return text.encode("utf-8")
