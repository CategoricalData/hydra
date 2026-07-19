<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Strings.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.strings

Strings as sequences of Unicode code points.
A Hydra string is a sequence of Unicode code points — not UTF-16 code units and not grapheme
clusters — and `length` and index positions count code points.
The general split/join pair (`splitOn` and `join`) is the primary line-and-token machinery;
the newline-specific `lines` and `unlines` were removed in its favor (see the deprecated entries
at the end of this page).

#### charAt — **Draft**

`int32 → string → optional<int32>`

Usage: `charAt i s`

Get the Unicode code point at a given index of a string.
Returns `given c`, where `c` is the code point at zero-based position `i` of `s`, or `none` if
`i` is negative or `i` is greater than or equal to `length s`.
Positions count code points, not bytes and not grapheme clusters.

Since: 0.18 (renamed from `hydra.lib.strings.maybeCharAt`)

#### concat — **Draft**

`list<string> → string`

Usage: `concat xs`

Concatenate a list of strings into a single string.
Returns the string formed by concatenating every string in `xs`, in order.
`concat []` is the empty string.

Since: 0.18 (renamed from `hydra.lib.strings.cat`)

#### concat2 — **Draft**

`string → string → string`

Usage: `concat2 s t`

Concatenate two strings.
Returns the concatenation of `s` and `t`: every code point of `s` followed by every code point
of `t`.
The binary form of `concat`: `concat2 s t` is `concat [s, t]`.

Since: 0.18 (renamed from `hydra.lib.strings.cat2`)

#### fromList — **Draft**

`list<int32> → string`

Usage: `fromList cs`

Convert a list of Unicode code points to a string.
Returns the string whose characters are the code points in `cs`, in order.
The inverse of `toList`: `fromList (toList s)` is `s` for every string `s`.
Values in `cs` that are not valid Unicode scalar values — values outside the range
[0, 0x10FFFF], and surrogate code points — are substituted with the replacement character
U+FFFD.

Since: 0.15

#### join — **Draft**

`string → list<string> → string`

Usage: `join sep xs`

Join a list of strings with a separator between each element.
Returns the strings in `xs` concatenated with `sep` inserted between each pair of adjacent
elements.
For the empty list the result is the empty string, and for a singleton list the result is the
single string.
For a non-empty `sep`, `join` inverts `splitOn`: `join sep (splitOn sep s)` is `s` for every
string `s`.
To join lines, use a newline separator: `join "\n" xs` (this does not append a trailing
newline; the removed `unlines` did).

Since: 0.18 (renamed from `hydra.lib.strings.intercalate`)

#### length — **Draft**

`string → int32`

Usage: `length s`

Return the length of a string.
Returns the number of Unicode code points in `s`, as an int32.
This is the code-point count, not the byte count and not the grapheme-cluster count: a character
requiring four bytes in UTF-8 counts as one, and an emoji built from multiple code points counts
as the number of code points it uses.
Defined for every string whose code-point count is representable as an int32
(fewer than 2^31 - 1 code points).

Since: 0.15

#### null — **Draft**

`string → boolean`

Usage: `null s`

Check whether a string is empty.
Returns `true` if `s` is the empty string, and `false` otherwise; equivalently, `null s` is
`true` exactly when `length s` is `0`.

Since: 0.15

#### splitOn — **Draft**

`string → string → list<string>`

Usage: `splitOn sep s`

Split a string on a delimiter string.
Returns the list of substrings of `s` obtained by splitting on every occurrence of the
non-empty delimiter `sep`.
Adjacent and boundary delimiters produce empty-string elements in the result:
`splitOn "," ",a,,b,"` is `["", "a", "", "b", ""]`.
The result is never the empty list, and for a non-empty `sep`, `join sep (splitOn sep s)` is `s`.
When `sep` is the empty string, no splitting occurs: `splitOn "" s` is the single-element
list `[s]`.
To split into lines, use a newline separator: `splitOn "\n" s`; note that an input ending in a
newline yields a trailing empty-string element (the removed `lines` did not).

Since: 0.15

#### toList — **Draft**

`string → list<int32>`

Usage: `toList s`

Convert a string to a list of Unicode code points.
Returns the list of code points making up `s`, in order, each represented as an int32.
The inverse of `fromList`.

Since: 0.15

#### toLower — **Draft**

`string → string`

Usage: `toLower s`

Convert a string to lowercase.
Returns `s` with each code point replaced by its simple (one-to-one) Unicode lowercase mapping,
or left unchanged if it has no lowercase mapping; it agrees with `hydra.lib.chars.toLower`
applied to each code point.
This is a code-point-by-code-point operation, so it does not handle the string-changing cases
of full Unicode case conversion (e.g. `"ß"` (U+00DF) does not lowercase to `"ss"`; it is
returned unchanged).
For text intended for human-readable display in locales with non-trivial case mappings, prefer
a host-specific full case-mapping API.

Since: 0.15

#### toUpper — **Draft**

`string → string`

Usage: `toUpper s`

Convert a string to uppercase.
Returns `s` with each code point replaced by its simple (one-to-one) Unicode uppercase mapping,
or left unchanged if it has no uppercase mapping; it agrees with `hydra.lib.chars.toUpper`
applied to each code point.
This is a code-point-by-code-point operation, so it does not handle the string-changing cases
of full Unicode case conversion (e.g. `"ß"` (U+00DF) does not uppercase to `"SS"`; it is
returned unchanged).
For text intended for human-readable display in locales with non-trivial case mappings, prefer
a host-specific full case-mapping API.

Since: 0.15

#### cat — **Deprecated**

`list<string> → string`

Deprecated since: 0.18. Use: `concat`.

#### cat2 — **Deprecated**

`string → string → string`

Deprecated since: 0.18. Use: `concat2`.

#### intercalate — **Deprecated**

`string → list<string> → string`

Deprecated since: 0.18. Use: `join`.

#### lines — **Deprecated**

`string → list<string>`

Deprecated since: 0.18. Use `splitOn` with a newline separator; note that `splitOn` yields a
trailing empty string when the input ends with a newline, where `lines` did not.

#### maybeCharAt — **Deprecated**

`int32 → string → optional<int32>`

Deprecated since: 0.18. Use: `charAt`.

#### unlines — **Deprecated**

`list<string> → string`

Deprecated since: 0.18. Use `join` with a newline separator; note that `unlines` also appended
a trailing newline.
