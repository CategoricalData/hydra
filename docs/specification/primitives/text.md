<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.text

<!-- Note: this is an automatically generated file. Do not edit. -->

Text encoding.

These primitives are the canonical UTF-8 conversions between Hydra's binary and string types, for interpreting bytes as text. They are distinct from hydra.lib.literals.binaryToBase64 / base64ToBinary, which are total, round-trippable base64 conversions for embedding arbitrary binary in a string context (e.g. a JSON value) -- use those when the bytes are not necessarily text.

Both text primitives are pure: file and other byte-oriented I/O is performed separately (see hydra.lib.files), with encoding and decoding applied before or after.

## hydra.core.lib.text.decodeUtf8

Decode a sequence of bytes as UTF-8 text.

`∀.binary → either<string, string>`

*Since: 0.17.*

decodeUtf8(bytes) attempts to interpret bytes as a UTF-8 encoded string. A byte sequence which is not valid UTF-8 yields left(message), where message is a host-provided description of the decoding failure; a successful decode yields right(text).

Pairs with hydra.core.lib.files.readFile, which returns raw bytes.

**[HYDRA-CORE-LIB-TEXT-DECODE-UTF8-DECODES-VALID-UTF8] (requirement) **decodeUtf8 decodes a valid UTF-8 byte sequence to its text, wrapped in right, including the empty byte sequence, ASCII-only text, and multi-byte (non-ASCII) text.

**[HYDRA-CORE-LIB-TEXT-DECODE-UTF8-LEFT-ON-INVALID-UTF8] (requirement) **decodeUtf8 returns left(message) when the byte sequence is not valid UTF-8, such as a lone continuation byte.

## hydra.core.lib.text.encodeUtf8

Encode text as a sequence of UTF-8 bytes.

`∀.string → binary`

*Since: 0.17.*

encodeUtf8(text) returns the UTF-8 encoding of text as raw bytes. Total: every Hydra string is valid Unicode and therefore always encodes.

Pairs with hydra.core.lib.files.writeFile, which expects raw bytes.

**[HYDRA-CORE-LIB-TEXT-ENCODE-UTF8-ENCODES-EXACT-UTF8-BYTES] (requirement) **encodeUtf8 returns the exact UTF-8 byte sequence for the given text, for the empty string, ASCII-only text, and multi-byte (non-ASCII) text.
