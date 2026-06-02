-- | Primitive declarations for the hydra.lib.chars namespace.

module Hydra.Sources.Kernel.Lib.Chars where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.chars"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Just (EntityMetadata
              (Just "Primitives in the hydra.lib.chars module.")
              [ "Characters are represented as Unicode code points carried in an int32. Each primitive interprets its\
                \ argument as a code point; arguments outside the valid range [0, 0x10FFFF] yield an\
                \ implementation-defined result (typically false for the predicates).",
                "All classification predicates in this module are total functions: every int32 argument maps to a\
                \ defined Boolean result, with no partiality or exceptions.",
                "Character classification follows the Unicode general categories, matching the behaviour of Haskell's\
                \ Data.Char functions of the same names. Hosts are expected to reproduce these categories rather than\
                \ a locale- or ASCII-only notion of letter, digit, or whitespace." ]
              []
              Nothing)}
  where
    definitions = [
      primNoDef "isAlphaNum" "Check whether a character is alphanumeric." intToBoolSig [
        "True if the argument is a Unicode letter or digit, false otherwise.",
        "The argument is interpreted as a Unicode code point: arguments outside the valid code-point range\
        \ [0, 0x10FFFF] yield an implementation-defined result (typically false).",
        "The classification is based on Unicode general categories (any of L*, Nd, Nl, No).",
        "Total. Corresponds to Haskell's Data.Char.isAlphaNum :: Char -> Bool."],
      primNoDef "isLower"    "Check whether a character is lowercase." intToBoolSig [
        "True if the argument is a Unicode lowercase letter (general category Ll), false otherwise.",
        "The argument is interpreted as a Unicode code point: arguments outside [0, 0x10FFFF] yield an\
        \ implementation-defined result (typically false).",
        "Note that not every letter is classified as uppercase or lowercase (e.g. titlecase letters, modifier\
        \ letters, and letters in scripts without a case distinction are neither).",
        "Total. Corresponds to Haskell's Data.Char.isLower :: Char -> Bool."],
      primNoDef "isSpace"    "Check whether a character is a whitespace character." intToBoolSig [
        "True if the argument is a Unicode whitespace character, false otherwise.",
        "The whitespace set follows Haskell's Data.Char.isSpace, which recognises U+0020 (space), U+0009\
        \ (tab), U+000A (line feed), U+000B (vertical tab), U+000C (form feed), U+000D (carriage return),\
        \ U+00A0 (no-break space), and other Unicode characters with general category Zs, Zl, or Zp.",
        "Total. Corresponds to Haskell's Data.Char.isSpace :: Char -> Bool."],
      primNoDef "isUpper"    "Check whether a character is uppercase." intToBoolSig [
        "True if the argument is a Unicode uppercase letter (general category Lu), false otherwise.",
        "The argument is interpreted as a Unicode code point: arguments outside [0, 0x10FFFF] yield an\
        \ implementation-defined result (typically false).",
        "Note that titlecase letters (Lt) are not classified as uppercase by this predicate.",
        "Total. Corresponds to Haskell's Data.Char.isUpper :: Char -> Bool."],
      primNoDef "toLower"    "Convert a character to lowercase." intToIntSig [
        "Return the simple (one-to-one) Unicode lowercase mapping of the argument, or the argument itself if\
        \ it has no lowercase mapping.",
        "This is a code-point-to-code-point mapping, so it does not handle the string-changing cases of full\
        \ Unicode case folding (e.g. U+00DF \"\xDF\" does not lowercase to \"ss\"; it returns itself).",
        "The argument is interpreted as a Unicode code point.",
        "Total. Corresponds to Haskell's Data.Char.toLower :: Char -> Char."],
      primNoDef "toUpper"    "Convert a character to uppercase." intToIntSig [
        "Return the simple (one-to-one) Unicode uppercase mapping of the argument, or the argument itself if\
        \ it has no uppercase mapping.",
        "This is a code-point-to-code-point mapping, so it does not handle the string-changing cases of full\
        \ Unicode case folding (e.g. U+00DF \"\xDF\" does not uppercase to \"SS\"; it returns itself).",
        "The argument is interpreted as a Unicode code point.",
        "Total. Corresponds to Haskell's Data.Char.toUpper :: Char -> Char."]]

primNoDef :: String -> String -> TermSignature -> [String] -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Shared signatures.

-- Int32 -> Boolean
intToBoolSig :: TermSignature
intToBoolSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing

-- Int32 -> Int32
intToIntSig :: TermSignature
intToIntSig = sig $ TypeScheme [] (Types.int32 Types.~> Types.int32) Nothing
