module Hydra.Sources.Test.Lib.Chars where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Data.Char (ord)
import Hydra.Core.Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Chars as DefChars


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.chars"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model", ModuleName "hydra.core.model", ModuleName "hydra.core.errors", ModuleName "hydra.core.test.testGraph", ModuleName "hydra.core.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.chars primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.core.lib.chars primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.chars primitives" $
    supergroup "hydra.core.lib.chars primitives" [
      charsIsAlpha,
      charsIsAlphaNum,
      charsIsDigit,
      charsIsLower,
      charsIsSpace,
      charsIsUpper,
      charsToLower,
      charsToUpper]

charsIsAlpha :: TypedTerm TestGroup
charsIsAlpha = subgroup "isAlpha" [
  test "lowercase letter" (ord 'a') true,
  test "uppercase letter" (ord 'A') true,
  test "digit" (ord '5') false,
  test "space" (ord ' ') false,
  test "punctuation" (ord '.') false]
  where
    test name x result = primCase name DefChars.isAlpha [int32 x] result

charsIsAlphaNum :: TypedTerm TestGroup
charsIsAlphaNum = supergroup "isAlphaNum" [
  subgroup "classifiesAlphanumeric" [
    test "letter" "classifiesAlphanumeric" (ord 'a') true,
    test "digit" "classifiesAlphanumeric" (ord '5') true],
  subgroup "classifiesNonAlphanumeric" [
    test "space" "classifiesNonAlphanumeric" (ord ' ') false,
    test "punctuation" "classifiesNonAlphanumeric" (ord '.') false]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefChars.isAlphaNum [int32 x] result

charsIsDigit :: TypedTerm TestGroup
charsIsDigit = subgroup "isDigit" [
  test "ascii digit" (ord '5') true,
  test "ascii zero" (ord '0') true,
  test "ascii nine" (ord '9') true,
  test "letter" (ord 'a') false,
  -- U+0660 ARABIC-INDIC DIGIT ZERO is Unicode Nd but not ASCII, so isDigit is deliberately false.
  test "non-ascii unicode digit" 0x0660 false]
  where
    test name x result = primCase name DefChars.isDigit [int32 x] result

charsIsLower :: TypedTerm TestGroup
charsIsLower = supergroup "isLower" [
  subgroup "classifiesLowercase" [
    test "lowercase" "classifiesLowercase" (ord 'a') true],
  subgroup "classifiesNonLowercase" [
    test "uppercase" "classifiesNonLowercase" (ord 'A') false,
    test "digit" "classifiesNonLowercase" (ord '5') false]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefChars.isLower [int32 x] result

charsIsSpace :: TypedTerm TestGroup
charsIsSpace = supergroup "isSpace" [
  subgroup "classifiesWhitespace" [
    test "space" "classifiesWhitespace" (ord ' ') true,
    test "tab" "classifiesWhitespace" (ord '\t') true,
    test "newline" "classifiesWhitespace" (ord '\n') true],
  subgroup "classifiesNonWhitespace" [
    test "letter" "classifiesNonWhitespace" (ord 'a') false]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefChars.isSpace [int32 x] result

charsIsUpper :: TypedTerm TestGroup
charsIsUpper = supergroup "isUpper" [
  subgroup "classifiesUppercase" [
    test "uppercase" "classifiesUppercase" (ord 'A') true],
  subgroup "classifiesNonUppercase" [
    test "lowercase" "classifiesNonUppercase" (ord 'a') false,
    test "digit" "classifiesNonUppercase" (ord '5') false]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefChars.isUpper [int32 x] result

charsToLower :: TypedTerm TestGroup
charsToLower = supergroup "toLower" [
  subgroup "mapsToLowercaseOrUnchanged" [
    test "uppercase" "mapsToLowercaseOrUnchanged" (ord 'A') (ord 'a'),
    test "lowercase" "mapsToLowercaseOrUnchanged" (ord 'a') (ord 'a'),
    test "digit" "mapsToLowercaseOrUnchanged" (ord '5') (ord '5')],
  subgroup "usesSimpleNotFullMapping" [
    -- U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE: the simple lowercase
    -- mapping is plain 'i' (U+0069), not the two-code-point full mapping "i̇".
    test "dotted capital I" "usesSimpleNotFullMapping" 0x130 (ord 'i')]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefChars.toLower [int32 x] (int32 result)

charsToUpper :: TypedTerm TestGroup
charsToUpper = supergroup "toUpper" [
  subgroup "mapsToUppercaseOrUnchanged" [
    test "lowercase" "mapsToUppercaseOrUnchanged" (ord 'a') (ord 'A'),
    test "uppercase" "mapsToUppercaseOrUnchanged" (ord 'A') (ord 'A'),
    test "digit" "mapsToUppercaseOrUnchanged" (ord '5') (ord '5')],
  subgroup "usesSimpleNotFullMapping" [
    -- U+00DF LATIN SMALL LETTER SHARP S ("ß") has no single-code-point
    -- uppercase mapping, so it is returned unchanged rather than expanded
    -- to "SS" (#782).
    test "sharp s" "usesSimpleNotFullMapping" 0xDF 0xDF,
    -- U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPHE ("ŉ") likewise has
    -- no simple uppercase mapping.
    test "n preceded by apostrophe" "usesSimpleNotFullMapping" 0x149 0x149,
    -- U+1F80 GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI: the simple
    -- uppercase mapping is the single code point U+1F88, not the two-code-point
    -- full mapping "ἈΙ" (capital alpha with psili, followed by capital iota).
    test "greek alpha with psili and ypogegrammeni" "usesSimpleNotFullMapping" 0x1F80 0x1F88]]
  where
    test name segment x result = primCaseWithProvisions name [segment] DefChars.toUpper [int32 x] (int32 result)
