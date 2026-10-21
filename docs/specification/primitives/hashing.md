<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.hashing

<!-- Note: this is an automatically generated file. Do not edit. -->

Digests are computed with SHA-256 as defined in FIPS 180-4; results are deterministic and bit-identical across hosts.

Hashing is pure: both primitives are ordinary total functions of their input bytes, and hashing never fails.

## hydra.core.lib.hashing.sha256

Compute the SHA-256 digest of a sequence of bytes.

`∀.binary → binary`

*Since: 0.17.*

sha256(bytes) returns the 32-byte SHA-256 digest of bytes. Total: hashing is pure and never fails.

Pairs with hydra.core.lib.files.readFile, which returns raw bytes, to hash file contents.

**[HYDRA-CORE-LIB-HASHING-SHA256-MATCHES-KNOWN-ANSWER-VECTORS] (requirement) **sha256 returns the correct 32-byte SHA-256 digest for known-answer input vectors, including the empty byte sequence.

## hydra.core.lib.hashing.sha256Hex

Compute the SHA-256 digest of a sequence of bytes as a lowercase hex string.

`∀.binary → string`

*Since: 0.17.*

sha256Hex(bytes) returns the SHA-256 digest of bytes as a 64-character lowercase hexadecimal string. Equivalent to hex-encoding the output of sha256. Total: hashing is pure and never fails.

sha256Hex(bytes) is the lowercase hexadecimal rendering of the 32 bytes of sha256(bytes); this equivalence is the specification.

Pairs with hydra.core.lib.files.readFile, which returns raw bytes, to hash file contents.

**[HYDRA-CORE-LIB-HASHING-SHA256-HEX-MATCHES-KNOWN-ANSWER-VECTORS] (requirement) **sha256Hex returns the correct 64-character lowercase hexadecimal SHA-256 digest for known-answer input vectors, including the empty byte sequence and a multi-block input.

**[HYDRA-CORE-LIB-HASHING-SHA256-HEX-HANDLES-PADDING-BOUNDARIES-CORRECTLY] (requirement) **sha256Hex returns the correct digest at every SHA-256 message-padding boundary length (55 bytes, the largest message whose padding still fits one 64-byte block; 56 bytes, which forces a second block; and 63 and 64 bytes, straddling the block size), guarding against a padding-logic regression that would only surface at these specific lengths.
