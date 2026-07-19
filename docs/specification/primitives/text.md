<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Text.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.text

Text encoding.
These primitives are the canonical conversions between Hydra's `binary` and `string` types;
the older `hydra.lib.literals.binaryToString` and `hydra.lib.literals.stringToBinary` are
deprecated in their favor.
Both use UTF-8, and both are pure: file and other byte-oriented I/O is performed separately
(see `hydra.lib.files`), with encoding and decoding applied before or after.

#### decodeUtf8 — **Draft**

`binary → either<string, string>`

Usage: `decodeUtf8 bytes`

Decode a sequence of bytes as UTF-8 text.
Attempts to interpret `bytes` as a UTF-8 encoded string.
A successful decode yields `right text`.
A byte sequence which is not valid UTF-8 — an ill-formed or truncated multi-byte sequence, an
overlong encoding, or an encoded surrogate or out-of-range code point — yields `left message`,
where `message` is a host-provided description of the decoding failure.
Only the fact of failure is normative; the message text itself is host-specific.
Decoding inverts encoding: `decodeUtf8 (encodeUtf8 text)` is `right text` for every string
`text`.
Pairs with `hydra.lib.files.readFile`, which returns raw bytes.

Since: 0.17

#### encodeUtf8 — **Draft**

`string → binary`

Usage: `encodeUtf8 text`

Encode text as a sequence of UTF-8 bytes.
Returns the UTF-8 encoding of `text` as raw bytes.
Total: every Hydra string is valid Unicode, and therefore always encodes.
Pairs with `hydra.lib.files.writeFile`, which expects raw bytes.

Since: 0.17
