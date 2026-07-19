<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Hashing.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.hashing

Cryptographic hashing.
Digests are computed with SHA-256 as defined in FIPS 180-4; results are deterministic and
bit-identical across hosts.
Hashing is pure: both primitives are ordinary total functions of their input bytes, and hashing
never fails.
To hash the contents of a file, pair these primitives with `hydra.lib.files.readFile`, which
returns raw bytes.

#### sha256 — **Draft**

`binary → binary`

Usage: `sha256 bytes`

Compute the SHA-256 digest of a sequence of bytes.
Returns the 32-byte SHA-256 digest of `bytes`.
Every byte sequence, including the empty one, has a digest.
Pairs with `hydra.lib.files.readFile`, which returns raw bytes, to hash file contents.

Since: 0.17

#### sha256Hex — **Draft**

`binary → string`

Usage: `sha256Hex bytes`

Compute the SHA-256 digest of a sequence of bytes, as a lowercase hexadecimal string.
Returns the SHA-256 digest of `bytes` as a 64-character lowercase hexadecimal string.
`sha256Hex bytes` is the lowercase hexadecimal rendering of the 32 bytes of `sha256 bytes`;
this equivalence is the specification.
Pairs with `hydra.lib.files.readFile`, which returns raw bytes, to hash file contents.

Since: 0.17
