-- | Primitive declarations for the hydra.lib.strings namespace.

module Hydra.Sources.Kernel.Lib.Strings where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), length, lines, null, unlines)


ns :: ModuleName
ns = ModuleName "hydra.lib.strings"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.strings module.")}
  where
    definitions = [cat, cat2, fromList, intercalate, length, lines, maybeCharAt, null, splitOn,
                   toList, toLower, toUpper, unlines]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

-- Monomorphic signature helpers
fn :: Type -> Type -> TermSignature
fn a b = sig $ TypeScheme [] (a Types.~> b) Nothing

fn2 :: Type -> Type -> Type -> TermSignature
fn2 a b c = sig $ TypeScheme [] (a Types.~> b Types.~> c) Nothing

cat :: PrimitiveDefinition
cat = define "cat" "Concatenate a list of strings into a single string."
  (fn (Types.list Types.string) Types.string)
  ["cat(xs) returns the string formed by concatenating every string in xs in order.",
   "Total. Corresponds to Haskell's concat :: [String] -> String."]

cat2 :: PrimitiveDefinition
cat2 = define "cat2" "Concatenate two strings."
  (fn2 Types.string Types.string Types.string)
  ["cat2(s, t) returns the concatenation of s and t.",
   "Total. Corresponds to Haskell's (++) :: String -> String -> String."]

fromList :: PrimitiveDefinition
fromList = define "fromList" "Convert a list of Unicode code points to a string."
  (fn (Types.list Types.int32) Types.string)
  ["fromList(cs) returns the string whose characters are the Unicode code points in cs, in order.",
   "Code points outside the valid Unicode range [0, 0x10FFFF] yield a host-defined result (typically\
  \ substitution with U+FFFD or truncation of the bits).",
   "Total. The inverse of toList."]

intercalate :: PrimitiveDefinition
intercalate = define "intercalate" "Join a list of strings with a separator between each element."
  (fn2 Types.string (Types.list Types.string) Types.string)
  ["intercalate(sep, xs) returns the strings in xs concatenated with sep inserted between each pair\
  \ of adjacent strings; for the empty list the result is the empty string, and for a singleton\
  \ list the result is the single string.",
   "Total. Corresponds to Haskell's Data.List.intercalate :: String -> [String] -> String."]

length :: PrimitiveDefinition
length = define "length" "Return the length of a string."
  (fn Types.string Types.int32)
  ["length(s) returns the number of Unicode code points in s as an int32.",
   "Note: this is the code-point count, not the byte count or the grapheme-cluster count, so a\
  \ four-byte UTF-8 character counts as one and an emoji built from multiple code points counts as\
  \ the number of code points it uses.",
   "Total on strings shorter than 2^31-1 code points."]

lines :: PrimitiveDefinition
lines = define "lines" "Split a string into lines."
  (fn Types.string (Types.list Types.string))
  ["lines(s) splits s into a list of lines, splitting on newline characters (U+000A).",
   "The trailing newline is consumed but does not produce an empty trailing element (matching\
  \ Haskell's lines behavior).",
   "Total. Corresponds to Haskell's lines :: String -> [String]."]

maybeCharAt :: PrimitiveDefinition
maybeCharAt = define "maybeCharAt" "Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds."
  (fn2 Types.int32 Types.string (Types.optional Types.int32))
  ["maybeCharAt(i, s) returns Just(c) where c is the Unicode code point at position i in s, or\
  \ Nothing if i is negative or i >= length(s).",
   "Total."]

null :: PrimitiveDefinition
null = define "null" "Check whether a string is empty."
  (fn Types.string Types.boolean)
  ["null(s) returns true iff s is the empty string.",
   "Total. Corresponds to Haskell's null :: String -> Bool."]

splitOn :: PrimitiveDefinition
splitOn = define "splitOn" "Split a string on a delimiter string."
  (fn2 Types.string Types.string (Types.list Types.string))
  ["splitOn(sep, s) returns the list of substrings of s obtained by splitting on every occurrence of\
  \ the non-empty delimiter sep. Adjacent or boundary delimiters produce empty-string elements in\
  \ the result.",
   "Behavior when sep is empty is host-defined and should not be relied upon.",
   "Total. Corresponds to Haskell's Data.List.Split.splitOn :: String -> String -> [String]."]

toList :: PrimitiveDefinition
toList = define "toList" "Convert a string to a list of Unicode code points."
  (fn Types.string (Types.list Types.int32))
  ["toList(s) returns the list of Unicode code points making up s, in order. Each code point is\
  \ represented as an int32.",
   "Total. The inverse of fromList."]

toLower :: PrimitiveDefinition
toLower = define "toLower" "Convert a string to lowercase."
  (fn Types.string Types.string)
  ["toLower(s) returns s with each character replaced by its Unicode simple (one-to-one) lowercase\
  \ mapping, or itself if it has no lowercase mapping.",
   "This is a code-point-by-code-point operation, so it does not handle the string-changing cases of\
  \ full Unicode case folding (e.g. U+00DF \"\xDF\" does not lowercase to \"ss\"; it returns itself).",
   "For text intended for human-readable display in locales with non-trivial case mappings, prefer a\
  \ host-specific full case-folding API.",
   "Total."]

toUpper :: PrimitiveDefinition
toUpper = define "toUpper" "Convert a string to uppercase."
  (fn Types.string Types.string)
  ["toUpper(s) returns s with each character replaced by its Unicode simple (one-to-one) uppercase\
  \ mapping, or itself if it has no uppercase mapping.",
   "This is a code-point-by-code-point operation, so it does not handle the string-changing cases of\
  \ full Unicode case folding (e.g. U+00DF \"\xDF\" does not uppercase to \"SS\"; it returns itself).",
   "For text intended for human-readable display in locales with non-trivial case mappings, prefer a\
  \ host-specific full case-folding API.",
   "Total."]

unlines :: PrimitiveDefinition
unlines = define "unlines" "Join a list of strings with newlines, appending a trailing newline."
  (fn (Types.list Types.string) Types.string)
  ["unlines(xs) returns the concatenation of every string in xs with a newline character (U+000A)\
  \ appended after each, including the last. The inverse of lines for normalized input.",
   "Total. Corresponds to Haskell's unlines :: [String] -> String."]
