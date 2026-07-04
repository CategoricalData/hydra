"""Python implementations of hydra.lib.hashing primitives.

SHA-256 over raw bytes (binary). These pair with hydra.lib.files.readFile, which is
byte-oriented, to hash file contents. Pure and total. For #524.
"""

from __future__ import annotations
import hashlib


def sha256(data: bytes) -> bytes:
    """Compute the 32-byte SHA-256 digest of a sequence of bytes."""
    return hashlib.sha256(data).digest()


def sha256_hex(data: bytes) -> str:
    """Compute the SHA-256 digest of a sequence of bytes as a 64-character lowercase hex string."""
    return hashlib.sha256(data).hexdigest()
