<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.strings

<!-- Note: this is an automatically generated file. Do not edit. -->

Strings as sequences of Unicode code points: a Hydra string is a sequence of Unicode code points -- not UTF-16 code units and not grapheme clusters -- and length and index positions count code points.

The general split/join pair (splitOn and join) is the primary line-and-token machinery; the newline-specific lines and unlines were removed as primitives in its favor, their exact semantics preserved as the non-primitive term helpers hydra.formatting.lines and hydra.formatting.unlines.

## hydra.core.lib.strings.charAt

Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds.

`∀.int32 → string → optional<int32>`

*Since: 0.17.6.*

charAt(i, s) returns Just(c) where c is the Unicode code point at position i in s, or Nothing if i is negative or i >= length(s).

Total.

Renamed from hydra.lib.strings.maybeCharAt.

**[HYDRA-CORE-LIB-STRINGS-CHAR-AT-RETURNS-CODE-POINT-AT-INDEX] (requirement) **charAt returns the Unicode code point at the given zero-based index, wrapped, indexing by code point (not UTF-16 code unit), so an index after an astral (non-BMP) character is unaffected by that character's surrogate-pair width in UTF-16-based representations.

**[HYDRA-CORE-LIB-STRINGS-CHAR-AT-NONE-WHEN-OUT-OF-RANGE] (requirement) **charAt returns none when the index is negative, at or beyond the string's code-point length, or when the string is empty.

## hydra.core.lib.strings.concat

Concatenate a list of strings into a single string.

`∀.list<string> → string`

*Since: 0.17.6.*

concat(xs) returns the string formed by concatenating every string in xs in order.

Total. Corresponds to Haskell's concat :: [String] -> String.

Renamed from hydra.lib.strings.cat.

**[HYDRA-CORE-LIB-STRINGS-CONCAT-FLATTENS-PRESERVING-ORDER-AND-CONTENT] (requirement) **concat concatenates every string in the list in order, preserving all content exactly including control characters, null characters, and multi-code-point Unicode sequences (combining characters) without alteration.

**[HYDRA-CORE-LIB-STRINGS-CONCAT-EMPTY-INPUTS] (requirement) **concat of a list containing only empty strings, or of an empty list of strings, is the empty string; empty strings elsewhere in the list contribute nothing to the result.

## hydra.core.lib.strings.concat2

Concatenate two strings.

`∀.string → string → string`

*Since: 0.17.6.*

concat2(s, t) returns the concatenation of s and t.

Total. Corresponds to Haskell's (++) :: String -> String -> String.

Renamed from hydra.lib.strings.cat2.

**[HYDRA-CORE-LIB-STRINGS-CONCAT2-APPENDS-PRESERVING-CONTENT] (requirement) **concat2 appends its second string argument to its first, preserving all content exactly including Unicode, control, and null characters.

**[HYDRA-CORE-LIB-STRINGS-CONCAT2-IDENTITY-ON-EMPTY] (requirement) **concat2 with either or both arguments empty returns the other argument unchanged (the empty string is the identity element).

## hydra.core.lib.strings.fromList

Convert a list of Unicode code points to a string.

`∀.list<int32> → string`

*Since: 0.15.*

fromList(cs) returns the string whose characters are the Unicode code points in cs, in order.

Code points outside the valid Unicode range [0, 0x10FFFF] yield a host-defined result (typically substitution with U+FFFD or truncation of the bits).

Total. The inverse of toList.

**[HYDRA-CORE-LIB-STRINGS-FROM-LIST-BUILDS-STRING-FROM-CODE-POINTS] (requirement) **fromList constructs a string whose characters are exactly the given Unicode code points, in order, including multi-code-point sequences (combining characters), control characters, and null characters.

**[HYDRA-CORE-LIB-STRINGS-FROM-LIST-EMPTY-INPUT] (requirement) **fromList of an empty list of code points is the empty string.

## hydra.core.lib.strings.isEmpty

Check whether a string is empty.

`∀.string → boolean`

*Since: 0.15.*

isEmpty(s) returns true iff s is the empty string.

Total. Corresponds to Haskell's null :: String -> Bool.

**[HYDRA-CORE-LIB-STRINGS-IS-EMPTY-TESTS-ZERO-LENGTH] (requirement) **isEmpty returns true exactly when the string has no code points; any non-empty content — including whitespace, control characters, or a null character — makes it false.

## hydra.core.lib.strings.join

Join a list of strings with a separator between each element.

`∀.string → list<string> → string`

*Since: 0.17.6.*

join(sep, xs) returns the strings in xs concatenated with sep inserted between each pair of adjacent strings; for the empty list the result is the empty string, and for a singleton list the result is the single string.

Total. Corresponds to Haskell's Data.List.intercalate :: String -> [String] -> String.

Renamed from hydra.lib.strings.intercalate.

**[HYDRA-CORE-LIB-STRINGS-JOIN-INSERTS-SEPARATOR-BETWEEN-STRINGS] (requirement) **join concatenates a list of strings with a copy of the separator inserted between each pair of adjacent strings (not before the first or after the last), for separators of any content including empty, multi-character, and Unicode.

**[HYDRA-CORE-LIB-STRINGS-JOIN-BOUNDARY-INPUTS] (requirement) **join of an empty list of strings is the empty string, regardless of separator; join of a single-element list returns that element unchanged, regardless of separator.

## hydra.core.lib.strings.length

Return the length of a string.

`∀.string → int32`

*Since: 0.15.*

length(s) returns the number of Unicode code points in s as an int32.

Note: this is the code-point count, not the byte count or the grapheme-cluster count, so a four-byte UTF-8 character counts as one and an emoji built from multiple code points counts as the number of code points it uses.

Total on strings shorter than 2^31-1 code points.

**[HYDRA-CORE-LIB-STRINGS-LENGTH-COUNTS-CODE-POINTS] (requirement) **length returns the number of Unicode code points in the string, not the byte count or grapheme-cluster count — so a multi-byte character counts as one code point and a combining-character sequence counts as multiple.

## hydra.core.lib.strings.splitOn

Split a string on a delimiter string.

`∀.string → string → list<string>`

*Since: 0.15.*

splitOn(sep, s) returns the list of substrings of s obtained by splitting on every occurrence of the non-empty delimiter sep. Adjacent or boundary delimiters produce empty-string elements in the result.

Behavior when sep is empty is host-defined and should not be relied upon.

Total. Corresponds to Haskell's Data.List.Split.splitOn :: String -> String -> [String].

**[HYDRA-CORE-LIB-STRINGS-SPLIT-ON-SPLITS-ON-EVERY-OCCURRENCE] (requirement) **splitOn splits the string into the list of substrings between every occurrence of a non-empty delimiter, including when occurrences are consecutive, overlapping, at a boundary, or span the whole string — boundary and adjacent delimiters produce empty-string elements in the result.

**[HYDRA-CORE-LIB-STRINGS-SPLIT-ON-NO-SPLIT-WHEN-ABSENT] (requirement) **splitOn returns the single-element list containing the original string when the delimiter does not occur in it.

**[HYDRA-CORE-LIB-STRINGS-SPLIT-ON-EMPTY-STRING-OPERANDS] (requirement) **splitOn on an empty string (with a non-empty separator) returns the single-element list containing the empty string; splitOn with an empty separator splits between every code point, including around an astral (non-BMP) character without breaking its surrogate-pair encoding apart.

## hydra.core.lib.strings.toList

Convert a string to a list of Unicode code points.

`∀.string → list<int32>`

*Since: 0.15.*

toList(s) returns the list of Unicode code points making up s, in order. Each code point is represented as an int32.

Total. The inverse of fromList.

**[HYDRA-CORE-LIB-STRINGS-TO-LIST-DECOMPOSES-INTO-CODE-POINTS] (requirement) **toList returns the list of Unicode code points making up the string, in order, as the exact inverse of fromList, including for multi-code-point sequences and control/null characters.

**[HYDRA-CORE-LIB-STRINGS-TO-LIST-EMPTY-INPUT] (requirement) **toList of the empty string is the empty list.

## hydra.core.lib.strings.toLower

Convert a string to lowercase.

`∀.string → string`

*Since: 0.15.*

toLower(s) returns s with each character replaced by its Unicode simple (one-to-one) lowercase mapping, or itself if it has no lowercase mapping.

This is a code-point-by-code-point operation, so it does not handle the string-changing match of full Unicode case folding (e.g. U+00DF "ß" does not lowercase to "ss"; it returns itself).

For text intended for human-readable display in locales with non-trivial case mappings, prefer a host-specific full case-folding API.

Total.

**[HYDRA-CORE-LIB-STRINGS-TO-LOWER-APPLIES-SIMPLE-LOWERCASE-MAPPING] (requirement) **toLower replaces each character with its Unicode simple (one-to-one) lowercase mapping, leaving a character with no lowercase mapping unchanged — including letters, accented Unicode characters, non-letter characters (numbers, punctuation, control characters), and the empty string.

**[HYDRA-CORE-LIB-STRINGS-TO-LOWER-NO-FULL-CASE-FOLDING-EXPANSION] (requirement) **toLower does not perform full Unicode case folding: a character whose full lowercase mapping would expand to multiple code points (e.g. U+0130) is still mapped to its simple, single-code-point lowercase form, not the multi-code-point expansion.

## hydra.core.lib.strings.toUpper

Convert a string to uppercase.

`∀.string → string`

*Since: 0.15.*

toUpper(s) returns s with each character replaced by its Unicode simple (one-to-one) uppercase mapping, or itself if it has no uppercase mapping.

This is a code-point-by-code-point operation, so it does not handle the string-changing match of full Unicode case folding (e.g. U+00DF "ß" does not uppercase to "SS"; it returns itself).

For text intended for human-readable display in locales with non-trivial case mappings, prefer a host-specific full case-folding API.

Total.

**[HYDRA-CORE-LIB-STRINGS-TO-UPPER-APPLIES-SIMPLE-UPPERCASE-MAPPING] (requirement) **toUpper replaces each character with its Unicode simple (one-to-one) uppercase mapping, leaving a character with no uppercase mapping unchanged — including letters, accented Unicode characters, non-letter characters, and the empty string.

**[HYDRA-CORE-LIB-STRINGS-TO-UPPER-NO-FULL-CASE-FOLDING-EXPANSION] (requirement) **toUpper does not perform full Unicode case folding: a character whose full uppercase mapping would expand to multiple code points, or whose simple mapping targets a single precomposed code point rather than a decomposed sequence, is mapped only to its simple, single-code-point uppercase form.
