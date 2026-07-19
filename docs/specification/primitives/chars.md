<!-- NOTE: this page will be automatically generated from the primitive definitions in
     packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/Chars.hs (generator not yet
     built). Hand-authored draft under #417; the specifications here are the normative TARGET
     and may deliberately diverge from the currently-frozen definitions.
     Conventions (notation, laziness, badges, floating point) are defined in index.md. -->

# hydra.lib.chars

Unicode code-point predicates and case mapping.
Characters are represented as Unicode code points carried in an int32; each primitive interprets
its argument as a code point.
Classification follows the Unicode general categories — not a locale- or ASCII-only notion of
letter, digit, or whitespace — with the deliberate exception of `isDigit`, which is ASCII-only.
Case mapping is the simple (one-to-one) single-code-point Unicode case mapping.
For arguments outside the valid code-point range [0, 0x10FFFF], and for surrogate code
points, the predicates return `false` and `toLower`/`toUpper` return the input unchanged.

#### isAlpha — **Draft**

`int32 → boolean`

Usage: `isAlpha c`

Check whether a character is alphabetic.
Returns `true` if `c` is a Unicode letter — any of the general categories L* (Lu, Ll, Lt, Lm,
Lo) — and `false` otherwise.
Alphabetic is broader than cased: letters in scripts without a case distinction are included.
`isAlphaNum c` is `true` whenever `isAlpha c` is `true`.

Since: 0.18

#### isAlphaNum — **Draft**

`int32 → boolean`

Usage: `isAlphaNum c`

Check whether a character is alphanumeric.
Returns `true` if `c` is a Unicode letter or digit, and `false` otherwise.
The classification is by Unicode general category: any of L* (Lu, Ll, Lt, Lm, Lo), Nd, Nl,
or No.

Since: 0.15

#### isDigit — **Draft**

`int32 → boolean`

Usage: `isDigit c`

Check whether a character is an ASCII decimal digit.
Returns `true` if `c` is one of the ten code points `'0'` through `'9'` (U+0030 to U+0039), and
`false` otherwise.
Unlike the other predicates in this module, this one is deliberately ASCII-only: decimal digits
outside the ASCII range (general category Nd) are rejected here, though `isAlphaNum` accepts
them.

Since: 0.18

#### isLower — **Draft**

`int32 → boolean`

Usage: `isLower c`

Check whether a character is a lowercase letter.
Returns `true` if `c` is a Unicode lowercase letter (general category Ll), and `false`
otherwise.
Note that not every letter is classified as uppercase or lowercase: titlecase letters, modifier
letters, and letters in scripts without a case distinction are neither.

Since: 0.15

#### isSpace — **Draft**

`int32 → boolean`

Usage: `isSpace c`

Check whether a character is a whitespace character.
Returns `true` if `c` is a whitespace character, and `false` otherwise.
The whitespace set comprises U+0020 (space), U+0009 (tab), U+000A (line feed), U+000B (vertical
tab), U+000C (form feed), U+000D (carriage return), and every character with general category
Zs, Zl, or Zp (which includes U+00A0, no-break space).

Since: 0.15

#### isUpper — **Draft**

`int32 → boolean`

Usage: `isUpper c`

Check whether a character is an uppercase letter.
Returns `true` if `c` is a Unicode uppercase letter (general category Lu), and `false`
otherwise.
Note that titlecase letters (Lt) are not classified as uppercase by this predicate.

Since: 0.15

#### toLower — **Draft**

`int32 → int32`

Usage: `toLower c`

Convert a character to lowercase.
Returns the simple (one-to-one) Unicode lowercase mapping of `c`, or `c` itself if it has no
lowercase mapping.
This is a code-point-to-code-point mapping, so it does not handle the string-changing cases of
full Unicode case conversion (e.g. U+00DF `"ß"` does not lowercase to `"ss"`; it is returned
unchanged).

Since: 0.15

#### toUpper — **Draft**

`int32 → int32`

Usage: `toUpper c`

Convert a character to uppercase.
Returns the simple (one-to-one) Unicode uppercase mapping of `c`, or `c` itself if it has no
uppercase mapping.
This is a code-point-to-code-point mapping, so it does not handle the string-changing cases of
full Unicode case conversion (e.g. U+00DF `"ß"` does not uppercase to `"SS"`; it is returned
unchanged).

Since: 0.15
