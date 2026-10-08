-- | Primitive declarations for the hydra.core.lib.chars namespace.

module Hydra.Sources.Kernel.Lib.Chars where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.core.lib.chars"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Just (EntityMetadata
              (Just "Primitives in the hydra.core.lib.chars module.")
              [ "Characters are represented as Unicode code points carried in an int32. Each primitive interprets its\
                \ argument as a code point; arguments outside the valid range [0, 0x10FFFF] yield an\
                \ implementation-defined result (typically false for the predicates).",
                "All classification predicates in this module are total functions: every int32 argument maps to a\
                \ defined Boolean result, with no partiality or exceptions.",
                "Character classification follows the Unicode general categories, matching the behaviour of Haskell's\
                \ Data.Char functions of the same names. Hosts are expected to reproduce these categories rather than\
                \ a locale- or ASCII-only notion of letter, digit, or whitespace." ]
              []
              Nothing
              [])}
  where
    definitions = [isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper, toLower, toUpper]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

-- Int32 -> Boolean
intToBool :: [(String, String)] -> TermSignature
intToBool params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.boolean) mempty

-- Int32 -> Int32
intToInt :: [(String, String)] -> TermSignature
intToInt params = sigWithParams params $ TypeScheme [] (Types.int32 Types.~> Types.int32) mempty

isAlpha :: PrimitiveDefinition
isAlpha = define "isAlpha" "Check whether a character is alphabetic."
  (intToBool [("char", "the character to test, as a Unicode code point")])
  ["True if the argument is a Unicode letter — any of the general categories L* (Lu, Ll, Lt, Lm, Lo) —\
   \ and false otherwise.",
   "The argument is interpreted as a Unicode code point: arguments outside the valid code-point range\
   \ [0, 0x10FFFF] yield an implementation-defined result (typically false).",
   "Alphabetic is broader than cased: letters in scripts without a case distinction are included.\
   \ isAlphaNum is true whenever isAlpha is true.",
   "Total. Corresponds to Haskell's Data.Char.isAlpha :: Char -> Bool."]

isAlphaNum :: PrimitiveDefinition
isAlphaNum = withProvisions [
  requirement "classifiesAlphanumeric"
    "isAlphaNum returns true for a code point that is a Unicode letter or digit.",
  requirement "classifiesNonAlphanumeric"
    "isAlphaNum returns false for a code point that is neither a Unicode letter nor a digit, such\
    \ as whitespace or punctuation."]
  $ define "isAlphaNum" "Check whether a character is alphanumeric."
  (intToBool [("char", "the character to test, as a Unicode code point")])
  ["True if the argument is a Unicode letter or digit, false otherwise.",
   "The argument is interpreted as a Unicode code point: arguments outside the valid code-point range\
   \ [0, 0x10FFFF] yield an implementation-defined result (typically false).",
   "The classification is based on Unicode general categories (any of L*, Nd, Nl, No).",
   "Total. Corresponds to Haskell's Data.Char.isAlphaNum :: Char -> Bool."]

isDigit :: PrimitiveDefinition
isDigit = define "isDigit" "Check whether a character is an ASCII decimal digit."
  (intToBool [("char", "the character to test, as a Unicode code point")])
  ["True if the argument is one of the ten code points '0' through '9' (U+0030 to U+0039), and false\
   \ otherwise.",
   "Unlike the other predicates in this module, this one is deliberately ASCII-only: decimal digits\
   \ outside the ASCII range (general category Nd) are rejected here, though isAlphaNum accepts them.",
   "Total. Corresponds to Haskell's Data.Char.isDigit :: Char -> Bool."]

isLower :: PrimitiveDefinition
isLower = withProvisions [
  requirement "classifiesLowercase"
    "isLower returns true for a code point that is a Unicode lowercase letter.",
  requirement "classifiesNonLowercase"
    "isLower returns false for a code point that is not a Unicode lowercase letter, including\
    \ uppercase letters and digits."]
  $ define "isLower" "Check whether a character is lowercase."
  (intToBool [("char", "the character to test, as a Unicode code point")])
  ["True if the argument is a Unicode lowercase letter (general category Ll), false otherwise.",
   "The argument is interpreted as a Unicode code point: arguments outside [0, 0x10FFFF] yield an\
   \ implementation-defined result (typically false).",
   "Note that not every letter is classified as uppercase or lowercase (e.g. titlecase letters, modifier\
   \ letters, and letters in scripts without a case distinction are neither).",
   "Total. Corresponds to Haskell's Data.Char.isLower :: Char -> Bool."]

isSpace :: PrimitiveDefinition
isSpace = withProvisions [
  requirement "classifiesWhitespace"
    "isSpace returns true for a code point that is a Unicode whitespace character, including the\
    \ space, tab, and newline characters.",
  requirement "classifiesNonWhitespace"
    "isSpace returns false for a code point that is not a Unicode whitespace character, such as a\
    \ letter."]
  $ define "isSpace" "Check whether a character is a whitespace character."
  (intToBool [("char", "the character to test, as a Unicode code point")])
  ["True if the argument is a Unicode whitespace character, false otherwise.",
   "The whitespace set follows Haskell's Data.Char.isSpace, which recognises U+0020 (space), U+0009\
   \ (tab), U+000A (line feed), U+000B (vertical tab), U+000C (form feed), U+000D (carriage return),\
   \ U+00A0 (no-break space), and other Unicode characters with general category Zs, Zl, or Zp.",
   "Total. Corresponds to Haskell's Data.Char.isSpace :: Char -> Bool."]

isUpper :: PrimitiveDefinition
isUpper = withProvisions [
  requirement "classifiesUppercase"
    "isUpper returns true for a code point that is a Unicode uppercase letter.",
  requirement "classifiesNonUppercase"
    "isUpper returns false for a code point that is not a Unicode uppercase letter, including\
    \ lowercase letters and digits."]
  $ define "isUpper" "Check whether a character is uppercase."
  (intToBool [("char", "the character to test, as a Unicode code point")])
  ["True if the argument is a Unicode uppercase letter (general category Lu), false otherwise.",
   "The argument is interpreted as a Unicode code point: arguments outside [0, 0x10FFFF] yield an\
   \ implementation-defined result (typically false).",
   "Note that titlecase letters (Lt) are not classified as uppercase by this predicate.",
   "Total. Corresponds to Haskell's Data.Char.isUpper :: Char -> Bool."]

toLower :: PrimitiveDefinition
toLower = withProvisions [
  requirement "mapsToLowercaseOrUnchanged"
    "toLower returns the lowercase mapping of an uppercase code point, and returns a code point\
    \ with no lowercase mapping (e.g. a digit, or an already-lowercase letter) unchanged.",
  requirement "usesSimpleNotFullMapping"
    "toLower uses the simple (one-to-one) Unicode case mapping, not full case folding: a code\
    \ point whose full lowercase mapping would expand to multiple code points instead maps to\
    \ its single-code-point simple mapping."]
  $ define "toLower" "Convert a character to lowercase."
  (intToInt [("char", "the character to convert, as a Unicode code point")])
  ["Return the simple (one-to-one) Unicode lowercase mapping of the argument, or the argument itself if\
  \ it has no lowercase mapping.",
   "This is a code-point-to-code-point mapping, so it does not handle the string-changing match of full\
   \ Unicode case folding (e.g. U+00DF \"\xDF\" does not lowercase to \"ss\"; it returns itself).",
   "The argument is interpreted as a Unicode code point.",
   "Total. Corresponds to Haskell's Data.Char.toLower :: Char -> Char."]

toUpper :: PrimitiveDefinition
toUpper = withProvisions [
  requirement "mapsToUppercaseOrUnchanged"
    "toUpper returns the uppercase mapping of a lowercase code point, and returns a code point\
    \ with no uppercase mapping (e.g. a digit, or an already-uppercase letter) unchanged.",
  requirement "usesSimpleNotFullMapping"
    "toUpper uses the simple (one-to-one) Unicode case mapping, not full case folding: a code\
    \ point with no single-code-point uppercase mapping (e.g. sharp s, n preceded by apostrophe)\
    \ is returned unchanged rather than expanded to a multi-code-point full mapping, and a code\
    \ point whose simple mapping is itself a different single code point maps to that code\
    \ point."]
  $ define "toUpper" "Convert a character to uppercase."
  (intToInt [("char", "the character to convert, as a Unicode code point")])
  ["Return the simple (one-to-one) Unicode uppercase mapping of the argument, or the argument itself if\
  \ it has no uppercase mapping.",
   "This is a code-point-to-code-point mapping, so it does not handle the string-changing match of full\
   \ Unicode case folding (e.g. U+00DF \"\xDF\" does not uppercase to \"SS\"; it returns itself).",
   "The argument is interpreted as a Unicode code point.",
   "Total. Corresponds to Haskell's Data.Char.toUpper :: Char -> Char."]
