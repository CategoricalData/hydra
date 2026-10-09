module Hydra.Sources.Test.Lib.Strings where

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
import Hydra.Core.Testing
import qualified Hydra.Core.Dsl.Lib.Lists as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Strings as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Strings as DefStrings


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.strings"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for hydra.core.lib.strings primitives"))}
  where
    definitions = [
        Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.strings primitives" $
    supergroup "hydra.core.lib.strings primitives" [
      stringsConcat,
      stringsConcat2,
      stringsFromList,
      stringsJoin,
      stringsLength,
      stringsCharAt,
      stringsIsEmpty,
      stringsSplitOn,
      stringsToList,
      stringsToLower,
      stringsToUpper]
    where
      -- Show functions for evalPair
      showInt32 :: TypedTerm (Int -> String)
      showInt32 = Phantoms.lambda "n" $ Literals.printInt32 (Phantoms.var "n")
      showBool :: TypedTerm (Bool -> String)
      showBool = Phantoms.lambda "b" $ Literals.printBoolean (Phantoms.var "b")
      showStringList :: TypedTerm ([String] -> String)
      showStringList = Phantoms.lambda "xs" $ Literals.printString (Strings.join (Phantoms.string ", ") (Phantoms.var "xs"))
      showIntList :: TypedTerm ([Int] -> String)
      showIntList = Phantoms.lambda "xs" $ Strings.join (Phantoms.string ", ") (Lists.map (Phantoms.lambda "n" $ Literals.printInt32 (Phantoms.var "n")) (Phantoms.var "xs"))

      stringsConcat = supergroup "concat" [
        subgroup "flattensPreservingOrderAndContent" [
          test "basic concatenation" "flattensPreservingOrderAndContent" ["one", "two", "three"] "onetwothree",
          test "single string" "flattensPreservingOrderAndContent" ["hello"] "hello",
          test "unicode strings" "flattensPreservingOrderAndContent" ["\241", "\19990", "\127757"] "\241\19990\127757",  -- ñ, 世, 🌍
          test "combining characters" "flattensPreservingOrderAndContent" ["e", "\769"] "e\769",  -- e + combining acute accent
          test "control characters" "flattensPreservingOrderAndContent" ["\n", "\t", "\r"] "\n\t\r",
          test "null character" "flattensPreservingOrderAndContent" ["hello", "\0", "world"] "hello\0world"],
        subgroup "emptyInputs" [
          test "empty list" "emptyInputs" [] "",
          test "with empty strings" "emptyInputs" ["", "one", "", ""] "one",
          test "all empty strings" "emptyInputs" ["", "", "", ""] ""]]
        where
          test name segment ls result = stringEvalPairWithProvisions name [segment] DefStrings.concat
            (Strings.concat (Phantoms.list (Phantoms.string <$> ls)))
            (Phantoms.string result)
  
      stringsConcat2 = supergroup "concat2" [
        subgroup "appendsPreservingContent" [
          test "basic concatenation" "appendsPreservingContent" "hello" "world" "helloworld",
          test "unicode characters" "appendsPreservingContent" "\241" "\19990" "\241\19990",  -- ñ, 世
          test "special characters" "appendsPreservingContent" "\n" "\t" "\n\t",
          test "null characters" "appendsPreservingContent" "hello\0" "world" "hello\0world"],
        subgroup "identityOnEmpty" [
          test "empty first string" "identityOnEmpty" "" "world" "world",
          test "empty second string" "identityOnEmpty" "hello" "" "hello",
          test "both empty strings" "identityOnEmpty" "" "" ""]]
        where
          test name segment s1 s2 result = stringEvalPairWithProvisions name [segment] DefStrings.concat2
            (Strings.concat2 (Phantoms.string s1) (Phantoms.string s2))
            (Phantoms.string result)
  
      stringsFromList = supergroup "fromList" [
        subgroup "buildsStringFromCodePoints" [
          test "basic ascii string" "buildsStringFromCodePoints" [104, 101, 108, 108, 111] "hello",
          test "single character" "buildsStringFromCodePoints" [97] "a",
          test "unicode characters" "buildsStringFromCodePoints" [241, 19990, 127757] "\241\19990\127757",  -- ñ, 世, 🌍
          test "combining character sequence" "buildsStringFromCodePoints" [101, 769] "e\769",  -- e + combining acute
          test "special characters" "buildsStringFromCodePoints" [10, 9, 13] "\n\t\r",
          test "null character" "buildsStringFromCodePoints" [104, 0, 105] "h\0i"],  -- h, null, i
        subgroup "emptyInput" [
          test "empty code point list" "emptyInput" [] ""]]
        where
          test name segment codePoints result = stringEvalPairWithProvisions name [segment] DefStrings.fromList
            (Strings.fromList (Phantoms.list (Phantoms.int32 <$> codePoints)))
            (Phantoms.string result)
  
      stringsJoin = supergroup "join" [
        subgroup "insertsSeparatorBetweenStrings" [
          test "comma separator" "insertsSeparatorBetweenStrings" "," ["one", "two", "three"] "one,two,three",
          test "empty separator" "insertsSeparatorBetweenStrings" "" ["a", "b", "c"] "abc",
          test "multi-character separator" "insertsSeparatorBetweenStrings" " | " ["A", "B", "C"] "A | B | C",
          test "empty strings in list" "insertsSeparatorBetweenStrings" "," ["", "a", ""] ",a,",
          test "unicode separator" "insertsSeparatorBetweenStrings" "\127757" ["link1", "link2"] "link1\127757link2",  -- 🌍
          test "newline separator" "insertsSeparatorBetweenStrings" "\n" ["line1", "line2"] "line1\nline2"],
        subgroup "boundaryInputs" [
          test "empty string list" "boundaryInputs" "," [] "",
          test "single item list" "boundaryInputs" "," ["only"] "only"]]
        where
          test name segment sep strs result = stringEvalPairWithProvisions name [segment] DefStrings.join
            (Strings.join (Phantoms.string sep) (Phantoms.list (Phantoms.string <$> strs)))
            (Phantoms.string result)
  
      stringsLength = subgroup "countsCodePoints" [
        test "empty string" "countsCodePoints" "" 0,
        test "single character" "countsCodePoints" "a" 1,
        test "basic word" "countsCodePoints" "hello" 5,
        test "unicode characters" "countsCodePoints" "\241\19990\127757" 3,  -- ñ, 世, 🌍 (verifies code point count)
        test "combining character sequence" "countsCodePoints" "e\769" 2,  -- e + combining acute (separate code points)
        test "special characters" "countsCodePoints" "\n\t\r" 3]
        where
          test name segment s result = evalPairWithProvisions name [segment] DefStrings.length showInt32
            (Strings.length (Phantoms.string s))
            (Phantoms.int32 result)
  
      stringsCharAt = supergroup "charAt" [
        subgroup "returnsCodePointAtIndex" [
          test "first character" "returnsCodePointAtIndex" 0 "hello" (Just 104),
          test "middle character" "returnsCodePointAtIndex" 2 "hello" (Just 108),
          test "last character" "returnsCodePointAtIndex" 4 "hello" (Just 111),
          test "single character string" "returnsCodePointAtIndex" 0 "a" (Just 97),
          test "unicode character" "returnsCodePointAtIndex" 0 "\241" (Just 241),           -- ñ
          test "multi-byte unicode" "returnsCodePointAtIndex" 0 "\19990" (Just 19990),      -- 世
          test "second of combining pair" "returnsCodePointAtIndex" 1 "e\769" (Just 769),   -- combining acute accent
          -- Astral (non-BMP) characters (#745): an index scheme that counts
          -- UTF-16 code units instead of code points desyncs once an astral
          -- character (outside the Basic Multilingual Plane) appears before
          -- the target index.
          test "before an astral character" "returnsCodePointAtIndex" 0 "\127757a" (Just 127757),  -- 🌍
          test "after an astral character" "returnsCodePointAtIndex" 1 "\127757a" (Just 97)],       -- 🌍, a
        subgroup "noneWhenOutOfRange" [
          test "out of bounds" "noneWhenOutOfRange" 5 "hello" Nothing,
          test "negative index" "noneWhenOutOfRange" (-1) "hello" Nothing,
          test "empty string" "noneWhenOutOfRange" 0 "" Nothing]]
        where
          test name segment idx s result = primCaseWithProvisions name [segment] DefStrings.charAt [int32 idx, string s] (optionalInt32 result)
          optionalInt32 Nothing = Core.termOptional nothing
          optionalInt32 (Just x) = Core.termOptional $ just (int32 x)

      stringsIsEmpty = subgroup "testsZeroLength" [
        test "empty string" "testsZeroLength" "" True,
        test "single character" "testsZeroLength" "a" False,
        test "space" "testsZeroLength" " " False,
        test "unicode space" "testsZeroLength" "\160" False,  -- non-breaking space
        test "newline" "testsZeroLength" "\n" False,
        test "null character" "testsZeroLength" "\0" False,
        test "multi-character" "testsZeroLength" "hello" False]
        where
          test name segment s result = evalPairWithProvisions name [segment] DefStrings.isEmpty showBool
            (Strings.isEmpty (Phantoms.string s))
            (Phantoms.boolean result)
  
      stringsSplitOn = supergroup "splitOn" [
        subgroup "splitsOnEveryOccurrence" [
          test "basic separator" "splitsOnEveryOccurrence" "ss" "Mississippi" ["Mi", "i", "ippi"],
          test "single char separator" "splitsOnEveryOccurrence" " " "one two three" ["one", "two", "three"],
          test "multi-char separator" "splitsOnEveryOccurrence" "  " "a  b  c" ["a", "b", "c"],
          test "separator at start" "splitsOnEveryOccurrence" "h" "hello" ["", "ello"],
          test "separator at end" "splitsOnEveryOccurrence" "o" "hello" ["hell", ""],
          test "leading and trailing separator" "splitsOnEveryOccurrence" " " " one two " ["", "one", "two", ""],
          test "whole string as separator" "splitsOnEveryOccurrence" "Mississippi" "Mississippi" ["", ""],
          test "consecutive separators" "splitsOnEveryOccurrence" " " "a  b" ["a", "", "b"],
          test "multiple occurrences" "splitsOnEveryOccurrence" "l" "hello" ["he", "", "o"],
          test "overlapping pattern" "splitsOnEveryOccurrence" "aa" "aaa" ["", "a"],
          test "single char both" "splitsOnEveryOccurrence" "a" "a" ["", ""],
          test "unicode separator" "splitsOnEveryOccurrence" "\19990" "hello\19990world" ["hello", "world"],  -- 世
          test "unicode content" "splitsOnEveryOccurrence" "," "\241,\19990,\127757" ["\241", "\19990", "\127757"],  -- ñ,世,🌍
          test "newline separator" "splitsOnEveryOccurrence" "\n" "line1\nline2\nline3" ["line1", "line2", "line3"]],
        subgroup "noSplitWhenAbsent" [
          test "separator not found" "noSplitWhenAbsent" "x" "hello" ["hello"]],
        subgroup "emptyStringOperands" [
          test "empty separator" "emptyStringOperands" "" "abc" ["", "a", "b", "c"],
          test "separator on empty string" "emptyStringOperands" "x" "" [""],
          test "both empty" "emptyStringOperands" "" "" [""],
          -- Astral (non-BMP) characters (#745): an empty-separator split that
          -- operates on UTF-16 code units instead of code points breaks a
          -- surrogate pair apart.
          test "empty separator with astral character" "emptyStringOperands" "" "a\127757b" ["", "a", "\127757", "b"]]]
        where
          test name segment s0 s1 result = evalPairWithProvisions name [segment] DefStrings.splitOn showStringList
            (Strings.splitOn (Phantoms.string s0) (Phantoms.string s1))
            (Phantoms.list (Phantoms.string <$> result))
  
      stringsToList = supergroup "toList" [
        subgroup "decomposesIntoCodePoints" [
          test "single character" "decomposesIntoCodePoints" "a" [97],
          test "basic word" "decomposesIntoCodePoints" "hello" [104, 101, 108, 108, 111],
          test "unicode characters" "decomposesIntoCodePoints" "\241\19990\127757" [241, 19990, 127757],  -- ñ, 世, 🌍
          test "combining character sequence" "decomposesIntoCodePoints" "e\769" [101, 769],  -- e + combining acute
          test "control characters" "decomposesIntoCodePoints" "\n\t\r" [10, 9, 13],
          test "null character" "decomposesIntoCodePoints" "h\0i" [104, 0, 105]],
        subgroup "emptyInput" [
          test "empty string" "emptyInput" "" []]]
        where
          test name segment s result = evalPairWithProvisions name [segment] DefStrings.toList showIntList
            (Strings.toList (Phantoms.string s))
            (Phantoms.list (Phantoms.int32 <$> result))
  
      stringsToLower = supergroup "toLower" [
        subgroup "appliesSimpleLowercaseMapping" [
          test "mixed case" "appliesSimpleLowercaseMapping" "Hello World" "hello world",
          test "all uppercase" "appliesSimpleLowercaseMapping" "HELLO" "hello",
          test "all lowercase" "appliesSimpleLowercaseMapping" "hello" "hello",
          test "empty string" "appliesSimpleLowercaseMapping" "" "",
          test "with numbers and punctuation" "appliesSimpleLowercaseMapping" "Abc123, XYZ!" "abc123, xyz!",
          test "control characters" "appliesSimpleLowercaseMapping" "\n\t\r" "\n\t\r",
          test "unicode accented chars" "appliesSimpleLowercaseMapping" "\209\193\201\205\211\218" "\241\225\233\237\243\250"],  -- ÑÁÉÍÓÚ -> ñáéíóú
        subgroup "noFullCaseFoldingExpansion" [
          -- Simple (one-to-one) case mapping only (#782): U+0130 "İ" has a
          -- two-code-point full lowercase mapping ("i̇"), but the simple
          -- mapping used here is plain "i".
          test "dotted capital I has a simple, single-codepoint mapping" "noFullCaseFoldingExpansion" "\304" "i"]]  -- İ -> i
        where
          test name segment s result = stringEvalPairWithProvisions name [segment] DefStrings.toLower
            (Strings.toLower (Phantoms.string s))
            (Phantoms.string result)

      stringsToUpper = supergroup "toUpper" [
        subgroup "appliesSimpleUppercaseMapping" [
          test "mixed case" "appliesSimpleUppercaseMapping" "hello World" "HELLO WORLD",
          test "all lowercase" "appliesSimpleUppercaseMapping" "hello" "HELLO",
          test "all uppercase" "appliesSimpleUppercaseMapping" "HELLO" "HELLO",
          test "empty string" "appliesSimpleUppercaseMapping" "" "",
          test "with numbers and punctuation" "appliesSimpleUppercaseMapping" "abc123, xyz!" "ABC123, XYZ!",
          test "control characters" "appliesSimpleUppercaseMapping" "\n\t\r" "\n\t\r",
          test "unicode accented chars" "appliesSimpleUppercaseMapping" "\241\225\233\237\243\250" "\209\193\201\205\211\218"],  -- ñáéíóú -> ÑÁÉÍÓÚ
        subgroup "noFullCaseFoldingExpansion" [
          -- Code-point-by-code-point, not full Unicode case conversion (#782):
          -- U+00DF "ß" has no single-code-point uppercase mapping, so it is
          -- returned unchanged rather than expanded to "SS".
          test "sharp s is not expanded to SS" "noFullCaseFoldingExpansion" "stra\223e" "STRA\223E",  -- straße -> STRAße
          -- U+0149 "ŉ" likewise has no simple uppercase mapping.
          test "n preceded by apostrophe is unchanged" "noFullCaseFoldingExpansion" "\329" "\329",
          -- U+1F80's simple uppercase mapping is the single code point
          -- U+1F88, not the two-code-point full mapping "ἈΙ".
          test "greek alpha with psili and ypogegrammeni" "noFullCaseFoldingExpansion" "\8064" "\8072"]]
        where
          test name segment s result = stringEvalPairWithProvisions name [segment] DefStrings.toUpper
            (Strings.toUpper (Phantoms.string s))
            (Phantoms.string result)
