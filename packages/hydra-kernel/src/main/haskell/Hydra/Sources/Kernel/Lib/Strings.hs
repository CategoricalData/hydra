-- | Primitive declarations for the hydra.core.lib.strings namespace.

module Hydra.Sources.Kernel.Lib.Strings where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), concat, length)


ns :: ModuleName
ns = ModuleName "hydra.core.lib.strings"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = descriptionMetadataWithComments (Just "Primitives in the hydra.core.lib.strings module.") [
              "Strings as sequences of Unicode code points: a Hydra string is a sequence of Unicode\
              \ code points -- not UTF-16 code units and not grapheme clusters -- and length and index\
              \ positions count code points.",
              "The general split/join pair (splitOn and join) is the primary line-and-token machinery;\
              \ the newline-specific lines and unlines were removed as primitives in its favor, their\
              \ exact semantics preserved as the non-primitive term helpers hydra.formatting.lines and\
              \ hydra.formatting.unlines."]}
  where
    definitions = [charAt, concat, concat2, fromList, isEmpty, join, length,
                   splitOn, toList, toLower, toUpper]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

-- Monomorphic signature helpers
fn :: [(String, String)] -> Type -> Type -> TermSignature
fn params a b = sigWithParams params $ TypeScheme [] (a Types.~> b) mempty

fn2 :: [(String, String)] -> Type -> Type -> Type -> TermSignature
fn2 params a b c = sigWithParams params $ TypeScheme [] (a Types.~> b Types.~> c) mempty

charAt :: PrimitiveDefinition
charAt = availableSince "0.17.6" $ withComments [
  "Renamed from hydra.lib.strings.maybeCharAt."]
  $ withProvisions [
  requirement "returnsCodePointAtIndex"
    "charAt returns the Unicode code point at the given zero-based index, wrapped, indexing by code\
    \ point (not UTF-16 code unit), so an index after an astral (non-BMP) character is unaffected by\
    \ that character's surrogate-pair width in UTF-16-based representations.",
  requirement "noneWhenOutOfRange"
    "charAt returns none when the index is negative, at or beyond the string's code-point length, or\
    \ when the string is empty."]
  $ define "charAt" "Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds."
  (fn2 [("i", "the index of the character to retrieve"), ("s", "the string to index into")] Types.int32 Types.string (Types.optional Types.int32))
  ["charAt(i, s) returns Just(c) where c is the Unicode code point at position i in s, or Nothing if i\
  \ is negative or i >= length(s).",
   "Total."]

concat :: PrimitiveDefinition
concat = availableSince "0.17.6" $ withComments [
  "Renamed from hydra.lib.strings.cat."]
  $ withProvisions [
  requirement "flattensPreservingOrderAndContent"
    "concat concatenates every string in the list in order, preserving all content exactly including\
    \ control characters, null characters, and multi-code-point Unicode sequences (combining\
    \ characters) without alteration.",
  requirement "emptyInputs"
    "concat of a list containing only empty strings, or of an empty list of strings, is the empty\
    \ string; empty strings elsewhere in the list contribute nothing to the result."]
  $ define "concat" "Concatenate a list of strings into a single string."
  (fn [("xs", "the list of strings to concatenate")] (Types.list Types.string) Types.string)
  ["concat(xs) returns the string formed by concatenating every string in xs in order.",
   "Total. Corresponds to Haskell's concat :: [String] -> String."]

concat2 :: PrimitiveDefinition
concat2 = availableSince "0.17.6" $ withComments [
  "Renamed from hydra.lib.strings.cat2."]
  $ withProvisions [
  requirement "appendsPreservingContent"
    "concat2 appends its second string argument to its first, preserving all content exactly\
    \ including Unicode, control, and null characters.",
  requirement "identityOnEmpty"
    "concat2 with either or both arguments empty returns the other argument unchanged (the empty\
    \ string is the identity element)."]
  $ define "concat2" "Concatenate two strings."
  (fn2 [("s", "the first string"), ("t", "the second string")] Types.string Types.string Types.string)
  ["concat2(s, t) returns the concatenation of s and t.",
   "Total. Corresponds to Haskell's (++) :: String -> String -> String."]

fromList :: PrimitiveDefinition
fromList = availableSince "0.15" $ withProvisions [
  requirement "buildsStringFromCodePoints"
    "fromList constructs a string whose characters are exactly the given Unicode code points, in\
    \ order, including multi-code-point sequences (combining characters), control characters, and\
    \ null characters.",
  requirement "emptyInput"
    "fromList of an empty list of code points is the empty string."]
  $ define "fromList" "Convert a list of Unicode code points to a string."
  (fn [("cs", "the list of Unicode code points to convert")] (Types.list Types.int32) Types.string)
  ["fromList(cs) returns the string whose characters are the Unicode code points in cs, in order.",
   "Code points outside the valid Unicode range [0, 0x10FFFF] yield a host-defined result (typically\
  \ substitution with U+FFFD or truncation of the bits).",
   "Total. The inverse of toList."]

isEmpty :: PrimitiveDefinition
isEmpty = availableSince "0.15" $ withProvisions [
  requirement "testsZeroLength"
    "isEmpty returns true exactly when the string has no code points; any non-empty content —\
    \ including whitespace, control characters, or a null character — makes it false."]
  $ define "isEmpty" "Check whether a string is empty."
  (fn [("s", "the string to test for emptiness")] Types.string Types.boolean)
  ["isEmpty(s) returns true iff s is the empty string.",
   "Total. Corresponds to Haskell's null :: String -> Bool."]

join :: PrimitiveDefinition
join = availableSince "0.17.6" $ withComments [
  "Renamed from hydra.lib.strings.intercalate."]
  $ withProvisions [
  requirement "insertsSeparatorBetweenStrings"
    "join concatenates a list of strings with a copy of the separator inserted between each pair of\
    \ adjacent strings (not before the first or after the last), for separators of any content\
    \ including empty, multi-character, and Unicode.",
  requirement "boundaryInputs"
    "join of an empty list of strings is the empty string, regardless of separator; join of a\
    \ single-element list returns that element unchanged, regardless of separator."]
  $ define "join" "Join a list of strings with a separator between each element."
  (fn2 [("sep", "the separator to insert between elements"), ("xs", "the list of strings to join")] Types.string (Types.list Types.string) Types.string)
  ["join(sep, xs) returns the strings in xs concatenated with sep inserted between each pair of\
  \ adjacent strings; for the empty list the result is the empty string, and for a singleton list the\
  \ result is the single string.",
   "Total. Corresponds to Haskell's Data.List.intercalate :: String -> [String] -> String."]

length :: PrimitiveDefinition
length = availableSince "0.15" $ withProvisions [
  requirement "countsCodePoints"
    "length returns the number of Unicode code points in the string, not the byte count or\
    \ grapheme-cluster count — so a multi-byte character counts as one code point and a\
    \ combining-character sequence counts as multiple."]
  $ define "length" "Return the length of a string."
  (fn [("s", "the string to measure")] Types.string Types.int32)
  ["length(s) returns the number of Unicode code points in s as an int32.",
   "Note: this is the code-point count, not the byte count or the grapheme-cluster count, so a\
  \ four-byte UTF-8 character counts as one and an emoji built from multiple code points counts as\
  \ the number of code points it uses.",
   "Total on strings shorter than 2^31-1 code points."]

splitOn :: PrimitiveDefinition
splitOn = availableSince "0.15" $ withProvisions [
  requirement "splitsOnEveryOccurrence"
    "splitOn splits the string into the list of substrings between every occurrence of a non-empty\
    \ delimiter, including when occurrences are consecutive, overlapping, at a boundary, or span the\
    \ whole string — boundary and adjacent delimiters produce empty-string elements in the result.",
  requirement "noSplitWhenAbsent"
    "splitOn returns the single-element list containing the original string when the delimiter does\
    \ not occur in it.",
  requirement "emptyStringOperands"
    "splitOn on an empty string (with a non-empty separator) returns the single-element list\
    \ containing the empty string; splitOn with an empty separator splits between every code point,\
    \ including around an astral (non-BMP) character without breaking its surrogate-pair encoding\
    \ apart."]
  $ define "splitOn" "Split a string on a delimiter string."
  (fn2 [("sep", "the delimiter to split on"), ("s", "the string to split")] Types.string Types.string (Types.list Types.string))
  ["splitOn(sep, s) returns the list of substrings of s obtained by splitting on every occurrence of\
  \ the non-empty delimiter sep. Adjacent or boundary delimiters produce empty-string elements in\
  \ the result.",
   "Behavior when sep is empty is host-defined and should not be relied upon.",
   "Total. Corresponds to Haskell's Data.List.Split.splitOn :: String -> String -> [String]."]

toList :: PrimitiveDefinition
toList = availableSince "0.15" $ withProvisions [
  requirement "decomposesIntoCodePoints"
    "toList returns the list of Unicode code points making up the string, in order, as the exact\
    \ inverse of fromList, including for multi-code-point sequences and control/null characters.",
  requirement "emptyInput"
    "toList of the empty string is the empty list."]
  $ define "toList" "Convert a string to a list of Unicode code points."
  (fn [("s", "the string to convert")] Types.string (Types.list Types.int32))
  ["toList(s) returns the list of Unicode code points making up s, in order. Each code point is\
  \ represented as an int32.",
   "Total. The inverse of fromList."]

toLower :: PrimitiveDefinition
toLower = availableSince "0.15" $ withProvisions [
  requirement "appliesSimpleLowercaseMapping"
    "toLower replaces each character with its Unicode simple (one-to-one) lowercase mapping, leaving\
    \ a character with no lowercase mapping unchanged — including letters, accented Unicode\
    \ characters, non-letter characters (numbers, punctuation, control characters), and the empty\
    \ string.",
  requirement "noFullCaseFoldingExpansion"
    "toLower does not perform full Unicode case folding: a character whose full lowercase mapping\
    \ would expand to multiple code points (e.g. U+0130) is still mapped to its simple,\
    \ single-code-point lowercase form, not the multi-code-point expansion."]
  $ define "toLower" "Convert a string to lowercase."
  (fn [("s", "the string to convert to lowercase")] Types.string Types.string)
  ["toLower(s) returns s with each character replaced by its Unicode simple (one-to-one) lowercase\
  \ mapping, or itself if it has no lowercase mapping.",
   "This is a code-point-by-code-point operation, so it does not handle the string-changing match of\
  \ full Unicode case folding (e.g. U+00DF \"\xDF\" does not lowercase to \"ss\"; it returns itself).",
   "For text intended for human-readable display in locales with non-trivial case mappings, prefer a\
  \ host-specific full case-folding API.",
   "Total."]

toUpper :: PrimitiveDefinition
toUpper = availableSince "0.15" $ withProvisions [
  requirement "appliesSimpleUppercaseMapping"
    "toUpper replaces each character with its Unicode simple (one-to-one) uppercase mapping, leaving\
    \ a character with no uppercase mapping unchanged — including letters, accented Unicode\
    \ characters, non-letter characters, and the empty string.",
  requirement "noFullCaseFoldingExpansion"
    "toUpper does not perform full Unicode case folding: a character whose full uppercase mapping\
    \ would expand to multiple code points, or whose simple mapping targets a single precomposed\
    \ code point rather than a decomposed sequence, is mapped only to its simple, single-code-point\
    \ uppercase form."]
  $ define "toUpper" "Convert a string to uppercase."
  (fn [("s", "the string to convert to uppercase")] Types.string Types.string)
  ["toUpper(s) returns s with each character replaced by its Unicode simple (one-to-one) uppercase\
  \ mapping, or itself if it has no uppercase mapping.",
   "This is a code-point-by-code-point operation, so it does not handle the string-changing match of\
  \ full Unicode case folding (e.g. U+00DF \"\xDF\" does not uppercase to \"SS\"; it returns itself).",
   "For text intended for human-readable display in locales with non-trivial case mappings, prefer a\
  \ host-specific full case-folding API.",
   "Total."]

