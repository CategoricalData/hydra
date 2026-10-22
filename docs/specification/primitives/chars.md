<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.lib.chars

<!-- Note: this is an automatically generated file. Do not edit. -->

Characters are represented as Unicode code points carried in an int32. Each primitive interprets its argument as a code point; arguments outside the valid range [0, 0x10FFFF] yield an implementation-defined result (typically false for the predicates).

All classification predicates in this module are total functions: every int32 argument maps to a defined Boolean result, with no partiality or exceptions.

Character classification follows the Unicode general categories, matching the behaviour of Haskell's Data.Char functions of the same names. Hosts are expected to reproduce these categories rather than a locale- or ASCII-only notion of letter, digit, or whitespace -- with the deliberate exception of isDigit, which is ASCII-only.

Case mapping (toLower/toUpper) is the simple (one-to-one) single-code-point Unicode case mapping, not full case folding.

## hydra.core.lib.chars.isAlpha

Check whether a character is alphabetic.

`∀.int32 → boolean`

*Since: 0.17.6.*

True if the argument is a Unicode letter — any of the general categories L* (Lu, Ll, Lt, Lm, Lo) — and false otherwise.

The argument is interpreted as a Unicode code point: arguments outside the valid code-point range [0, 0x10FFFF] yield an implementation-defined result (typically false).

Alphabetic is broader than cased: letters in scripts without a case distinction are included. isAlphaNum is true whenever isAlpha is true.

Total. Corresponds to Haskell's Data.Char.isAlpha :: Char -> Bool.

## hydra.core.lib.chars.isAlphaNum

Check whether a character is alphanumeric.

`∀.int32 → boolean`

*Since: 0.15.*

True if the argument is a Unicode letter or digit, false otherwise.

The argument is interpreted as a Unicode code point: arguments outside the valid code-point range [0, 0x10FFFF] yield an implementation-defined result (typically false).

The classification is based on Unicode general categories (any of L*, Nd, Nl, No).

Total. Corresponds to Haskell's Data.Char.isAlphaNum :: Char -> Bool.

**[HYDRA-CORE-LIB-CHARS-IS-ALPHA-NUM-CLASSIFIES-ALPHANUMERIC] (requirement) **isAlphaNum returns true for a code point that is a Unicode letter or digit.

**[HYDRA-CORE-LIB-CHARS-IS-ALPHA-NUM-CLASSIFIES-NON-ALPHANUMERIC] (requirement) **isAlphaNum returns false for a code point that is neither a Unicode letter nor a digit, such as whitespace or punctuation.

## hydra.core.lib.chars.isDigit

Check whether a character is an ASCII decimal digit.

`∀.int32 → boolean`

*Since: 0.17.6.*

True if the argument is one of the ten code points '0' through '9' (U+0030 to U+0039), and false otherwise.

Unlike the other predicates in this module, this one is deliberately ASCII-only: decimal digits outside the ASCII range (general category Nd) are rejected here, though isAlphaNum accepts them.

Total. Corresponds to Haskell's Data.Char.isDigit :: Char -> Bool.

## hydra.core.lib.chars.isLower

Check whether a character is lowercase.

`∀.int32 → boolean`

*Since: 0.15.*

True if the argument is a Unicode lowercase letter (general category Ll), false otherwise.

The argument is interpreted as a Unicode code point: arguments outside [0, 0x10FFFF] yield an implementation-defined result (typically false).

Note that not every letter is classified as uppercase or lowercase (e.g. titlecase letters, modifier letters, and letters in scripts without a case distinction are neither).

Total. Corresponds to Haskell's Data.Char.isLower :: Char -> Bool.

**[HYDRA-CORE-LIB-CHARS-IS-LOWER-CLASSIFIES-LOWERCASE] (requirement) **isLower returns true for a code point that is a Unicode lowercase letter.

**[HYDRA-CORE-LIB-CHARS-IS-LOWER-CLASSIFIES-NON-LOWERCASE] (requirement) **isLower returns false for a code point that is not a Unicode lowercase letter, including uppercase letters and digits.

## hydra.core.lib.chars.isSpace

Check whether a character is a whitespace character.

`∀.int32 → boolean`

*Since: 0.15.*

True if the argument is a Unicode whitespace character, false otherwise.

The whitespace set follows Haskell's Data.Char.isSpace, which recognises U+0020 (space), U+0009 (tab), U+000A (line feed), U+000B (vertical tab), U+000C (form feed), U+000D (carriage return), U+00A0 (no-break space), and other Unicode characters with general category Zs, Zl, or Zp.

Total. Corresponds to Haskell's Data.Char.isSpace :: Char -> Bool.

**[HYDRA-CORE-LIB-CHARS-IS-SPACE-CLASSIFIES-WHITESPACE] (requirement) **isSpace returns true for a code point that is a Unicode whitespace character, including the space, tab, and newline characters.

**[HYDRA-CORE-LIB-CHARS-IS-SPACE-CLASSIFIES-NON-WHITESPACE] (requirement) **isSpace returns false for a code point that is not a Unicode whitespace character, such as a letter.

## hydra.core.lib.chars.isUpper

Check whether a character is uppercase.

`∀.int32 → boolean`

*Since: 0.15.*

True if the argument is a Unicode uppercase letter (general category Lu), false otherwise.

The argument is interpreted as a Unicode code point: arguments outside [0, 0x10FFFF] yield an implementation-defined result (typically false).

Note that titlecase letters (Lt) are not classified as uppercase by this predicate.

Total. Corresponds to Haskell's Data.Char.isUpper :: Char -> Bool.

**[HYDRA-CORE-LIB-CHARS-IS-UPPER-CLASSIFIES-UPPERCASE] (requirement) **isUpper returns true for a code point that is a Unicode uppercase letter.

**[HYDRA-CORE-LIB-CHARS-IS-UPPER-CLASSIFIES-NON-UPPERCASE] (requirement) **isUpper returns false for a code point that is not a Unicode uppercase letter, including lowercase letters and digits.

## hydra.core.lib.chars.toLower

Convert a character to lowercase.

`∀.int32 → int32`

*Since: 0.15.*

Return the simple (one-to-one) Unicode lowercase mapping of the argument, or the argument itself if it has no lowercase mapping.

This is a code-point-to-code-point mapping, so it does not handle the string-changing match of full Unicode case folding (e.g. U+00DF "ß" does not lowercase to "ss"; it returns itself).

The argument is interpreted as a Unicode code point.

Total. Corresponds to Haskell's Data.Char.toLower :: Char -> Char.

**[HYDRA-CORE-LIB-CHARS-TO-LOWER-MAPS-TO-LOWERCASE-OR-UNCHANGED] (requirement) **toLower returns the lowercase mapping of an uppercase code point, and returns a code point with no lowercase mapping (e.g. a digit, or an already-lowercase letter) unchanged.

**[HYDRA-CORE-LIB-CHARS-TO-LOWER-USES-SIMPLE-NOT-FULL-MAPPING] (requirement) **toLower uses the simple (one-to-one) Unicode case mapping, not full case folding: a code point whose full lowercase mapping would expand to multiple code points instead maps to its single-code-point simple mapping.

## hydra.core.lib.chars.toUpper

Convert a character to uppercase.

`∀.int32 → int32`

*Since: 0.15.*

Return the simple (one-to-one) Unicode uppercase mapping of the argument, or the argument itself if it has no uppercase mapping.

This is a code-point-to-code-point mapping, so it does not handle the string-changing match of full Unicode case folding (e.g. U+00DF "ß" does not uppercase to "SS"; it returns itself).

The argument is interpreted as a Unicode code point.

Total. Corresponds to Haskell's Data.Char.toUpper :: Char -> Char.

**[HYDRA-CORE-LIB-CHARS-TO-UPPER-MAPS-TO-UPPERCASE-OR-UNCHANGED] (requirement) **toUpper returns the uppercase mapping of a lowercase code point, and returns a code point with no uppercase mapping (e.g. a digit, or an already-uppercase letter) unchanged.

**[HYDRA-CORE-LIB-CHARS-TO-UPPER-USES-SIMPLE-NOT-FULL-MAPPING] (requirement) **toUpper uses the simple (one-to-one) Unicode case mapping, not full case folding: a code point with no single-code-point uppercase mapping (e.g. sharp s, n preceded by apostrophe) is returned unchanged rather than expanded to a multi-code-point full mapping, and a code point whose simple mapping is itself a different single code point maps to that code point.
