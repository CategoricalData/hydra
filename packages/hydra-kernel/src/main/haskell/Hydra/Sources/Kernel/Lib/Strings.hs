-- | Primitive declarations for the hydra.lib.strings namespace.

module Hydra.Sources.Kernel.Lib.Strings where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.strings"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.strings namespace."}
  where
    definitions = [
      primNoDef "cat"
        "Concatenate a list of strings into a single string."
        (sigFn (Types.list Types.string) Types.string) (Just
          "cat(xs) returns the string formed by concatenating every string in xs in order. Total.\
          \ Corresponds to Haskell's concat :: [String] -> String."),
      primNoDef "cat2"
        "Concatenate two strings."
        (sigFn2 Types.string Types.string Types.string) (Just
          "cat2(s, t) returns the concatenation of s and t. Total. Corresponds to Haskell's\
          \ (++) :: String -> String -> String."),
      primNoDef "fromList"
        "Convert a list of Unicode code points to a string."
        (sigFn (Types.list Types.int32) Types.string) (Just
          "fromList(cs) returns the string whose characters are the Unicode code points in cs, in order.\
          \ Code points outside the valid Unicode range [0, 0x10FFFF] yield a host-defined result\
          \ (typically substitution with U+FFFD or truncation of the bits). Total. The inverse of toList."),
      primNoDef "intercalate"
        "Join a list of strings with a separator between each element."
        (sigFn2 Types.string (Types.list Types.string) Types.string) (Just
          "intercalate(sep, xs) returns the strings in xs concatenated with sep inserted between each pair\
          \ of adjacent strings; for the empty list the result is the empty string, and for a singleton\
          \ list the result is the single string. Total. Corresponds to Haskell's\
          \ Data.List.intercalate :: String -> [String] -> String."),
      primNoDef "length"
        "Return the length of a string."
        (sigFn Types.string Types.int32) (Just
          "length(s) returns the number of Unicode code points in s as an int32. Note: this is the\
          \ code-point count, not the byte count or the grapheme-cluster count, so a four-byte UTF-8\
          \ character counts as one and an emoji built from multiple code points counts as the number of\
          \ code points it uses. Total on strings shorter than 2^31-1 code points."),
      primNoDef "lines"
        "Split a string into lines."
        (sigFn Types.string (Types.list Types.string)) (Just
          "lines(s) splits s into a list of lines, splitting on newline characters (U+000A). The trailing\
          \ newline is consumed but does not produce an empty trailing element (matching Haskell's lines\
          \ behavior). Total. Corresponds to Haskell's lines :: String -> [String]."),
      primNoDef "maybeCharAt"
        "Get the Unicode code point of the character at a specific index, returning Nothing if out of bounds."
        (sigFn2 Types.int32 Types.string (Types.optional Types.int32)) (Just
          "maybeCharAt(i, s) returns Just(c) where c is the Unicode code point at position i in s, or\
          \ Nothing if i is negative or i >= length(s). Total."),
      primNoDef "null"
        "Check whether a string is empty."
        (sigFn Types.string Types.boolean) (Just
          "null(s) returns true iff s is the empty string. Total. Corresponds to Haskell's\
          \ null :: String -> Bool."),
      primNoDef "splitOn"
        "Split a string on a delimiter string."
        (sigFn2 Types.string Types.string (Types.list Types.string)) (Just
          "splitOn(sep, s) returns the list of substrings of s obtained by splitting on every occurrence\
          \ of the non-empty delimiter sep. Adjacent or boundary delimiters produce empty-string elements\
          \ in the result. Behavior when sep is empty is host-defined and should not be relied upon.\
          \ Total. Corresponds to Haskell's Data.List.Split.splitOn :: String -> String -> [String]."),
      primNoDef "toList"
        "Convert a string to a list of Unicode code points."
        (sigFn Types.string (Types.list Types.int32)) (Just
          "toList(s) returns the list of Unicode code points making up s, in order. Each code point is\
          \ represented as an int32. Total. The inverse of fromList."),
      primNoDef "toLower"
        "Convert a string to lowercase."
        (sigFn Types.string Types.string) (Just
          "toLower(s) returns s with each character replaced by its Unicode simple (one-to-one) lowercase\
          \ mapping, or itself if it has no lowercase mapping. This is a code-point-by-code-point\
          \ operation, so it does not handle the string-changing cases of full Unicode case folding (e.g.\
          \ U+00DF \"\xDF\" does not lowercase to \"ss\"; it returns itself). For text intended for\
          \ human-readable display in locales with non-trivial case mappings, prefer a host-specific full\
          \ case-folding API. Total."),
      primNoDef "toUpper"
        "Convert a string to uppercase."
        (sigFn Types.string Types.string) (Just
          "toUpper(s) returns s with each character replaced by its Unicode simple (one-to-one) uppercase\
          \ mapping, or itself if it has no uppercase mapping. This is a code-point-by-code-point\
          \ operation, so it does not handle the string-changing cases of full Unicode case folding (e.g.\
          \ U+00DF \"\xDF\" does not uppercase to \"SS\"; it returns itself). For text intended for\
          \ human-readable display in locales with non-trivial case mappings, prefer a host-specific full\
          \ case-folding API. Total."),
      primNoDef "unlines"
        "Join a list of strings with newlines, appending a trailing newline."
        (sigFn (Types.list Types.string) Types.string) (Just
          "unlines(xs) returns the concatenation of every string in xs with a newline character (U+000A)\
          \ appended after each, including the last. The inverse of lines for normalized input. Total.\
          \ Corresponds to Haskell's unlines :: [String] -> String.")]

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Helpers: build TermSignatures from monomorphic function types.

sigFn :: Type -> Type -> TermSignature
sigFn a b = sig $ TypeScheme [] (a Types.~> b) Nothing

sigFn2 :: Type -> Type -> Type -> TermSignature
sigFn2 a b c = sig $ TypeScheme [] (a Types.~> b Types.~> c) Nothing
