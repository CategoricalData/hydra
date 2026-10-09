module Hydra.Sources.Test.Lib.Regex where

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
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Regex as DefRegex


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.regex"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for hydra.core.lib.regex primitives"))}
  where
    definitions = [
        Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Host-independent conformance suite for hydra.core.lib.regex (issue #603). Every case here pins a
-- semantics decision from docs/specification/regex.md (leftmost-longest matching, . = any char
-- including newline, whole-string anchors, the minimal-core grammar and its escaping rules, and the
-- portable ill-formed-pattern-is-no-match failure channel) so that, once every host's primitives
-- route patterns through parse.regex |> print.<dialect>.regex, running this suite on every host
-- proves the printers agree: the SAME pattern text produces the SAME observable behavior everywhere.
allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.regex primitives" $
    supergroup "hydra.core.lib.regex primitives" [
      regexMatches,
      regexFind,
      regexFindAll,
      regexReplace,
      regexReplaceAll,
      regexSplit,
      regexConformance]
    where
      regexMatches = supergroup "matches" [
        subgroup "isWholeStringAnchored" [
          test2 "exact match" "hello" "hello" True,
          test2 "partial content does not match" "[a-z]+" "hello123" False,
          test2 "mixed pattern" "[a-z]+[0-9]+" "hello123" True],
        subgroup "testsMinimalCoreGrammar" [
          test2 "pattern match" "[a-z]+" "hello" True,
          test2 "digit pattern" "[0-9]+" "12345" True,
          test2 "empty pattern matches empty" "" "" True,
          test2 "star matches empty" "a*" "" True,
          test2 "alternation" "cat|dog" "cat" True,
          test2 "alternation second" "cat|dog" "dog" True,
          test2 "quantifier" "ab?c" "ac" True,
          test2 "quantifier with optional" "ab?c" "abc" True],
        subgroup "falseWhenNoMatchOrIllFormed" [
          test2 "no match" "[0-9]+" "hello" False,
          test2 "empty pattern does not match non-empty" "" "hello" False,
          test2 "alternation no match" "cat|dog" "bird" False]]
        where
          test2 name pat input result =
            primCaseWithProvisions name [segmentOf name] DefRegex.matches [string pat, string input] (boolean result)
          segmentOf n
            | n `elem` ["exact match", "partial content does not match", "mixed pattern"] = "isWholeStringAnchored"
            | n `elem` ["no match", "empty pattern does not match non-empty", "alternation no match"] = "falseWhenNoMatchOrIllFormed"
            | otherwise = "testsMinimalCoreGrammar"

      regexFind = supergroup "find" [
        subgroup "returnsLeftmostLongestMatch" [
          test2 "simple find" "[0-9]+" "abc123def" (Just "123"),
          test2 "find first" "[a-z]+" "123abc456def" (Just "abc"),
          test2 "full match" ".*" "hello" (Just "hello")],
        subgroup "noneWhenNoMatch" [
          test2 "no match" "[0-9]+" "abcdef" Nothing,
          test2 "empty input" "[0-9]+" "" Nothing]]
        where
          test2 name pat input result =
            primCaseWithProvisions name [segmentOf result] DefRegex.find [string pat, string input] (Core.termOptional $ optStr result)
          segmentOf (Just _) = "returnsLeftmostLongestMatch"
          segmentOf Nothing = "noneWhenNoMatch"
          optStr Nothing = nothing
          optStr (Just s) = just (string s)

      regexFindAll = supergroup "findAll" [
        subgroup "returnsAllNonOverlappingMatchesInOrder" [
          test2 "multiple matches" "[0-9]+" "a1b2c3" ["1", "2", "3"],
          test2 "overlapping words" "[a-z]+" "abc def ghi" ["abc", "def", "ghi"],
          test2 "single match" "hello" "say hello world" ["hello"]],
        subgroup "emptyListWhenNoMatch" [
          test2 "no matches" "[0-9]+" "abc" []]]
        where
          test2 name pat input result =
            primCaseWithProvisions name [if null result then "emptyListWhenNoMatch" else "returnsAllNonOverlappingMatchesInOrder"] DefRegex.findAll [string pat, string input] (list (string <$> result))

      regexReplace = supergroup "replace" [
        subgroup "replacesLeftmostLongestMatch" [
          test3 "basic replace" "[0-9]+" "X" "abc123def456" "abcXdef456",
          test3 "replace at start" "^[a-z]+" "X" "abc123" "X123",
          test3 "empty replacement" "[0-9]+" "" "abc123def" "abcdef"],
        subgroup "unchangedWhenNoMatch" [
          test3 "no match" "[0-9]+" "X" "abcdef" "abcdef"]]
        where
          test3 name pat repl input result =
            primCaseWithProvisions name [segmentOf name] DefRegex.replace [string pat, string repl, string input] (string result)
          segmentOf n = if n == "no match" then "unchangedWhenNoMatch" else "replacesLeftmostLongestMatch"

      regexReplaceAll = supergroup "replaceAll" [
        subgroup "replacesEveryNonOverlappingMatch" [
          test3 "replace all digits" "[0-9]+" "X" "a1b2c3" "aXbXcX",
          test3 "replace all words" "[a-z]+" "X" "abc 123 def" "X 123 X",
          test3 "empty replacement" "[0-9]+" "" "a1b2c3" "abc"],
        subgroup "unchangedWhenNoMatch" [
          test3 "no match" "[0-9]+" "X" "abc" "abc"]]
        where
          test3 name pat repl input result =
            primCaseWithProvisions name [segmentOf name] DefRegex.replaceAll [string pat, string repl, string input] (string result)
          segmentOf n = if n == "no match" then "unchangedWhenNoMatch" else "replacesEveryNonOverlappingMatch"

      regexSplit = supergroup "split" [
        subgroup "splitsOnEveryNonOverlappingMatch" [
          test2 "split on comma" "," "a,b,c" ["a", "b", "c"],
          test2 "split on spaces" " +" "a b  c" ["a", "b", "c"],
          test2 "split on digits" "[0-9]+" "a1b2c" ["a", "b", "c"],
          test2 "trailing delimiter" "," "a,b," ["a", "b", ""]],
        subgroup "singleElementListWhenNoMatchOrIllFormed" [
          test2 "no match" "," "abc" ["abc"]]]
        where
          test2 name pat input result =
            primCaseWithProvisions name [segmentOf name] DefRegex.split [string pat, string input] (list (string <$> result))
          segmentOf n = if n == "no match" then "singleElementListWhenNoMatchOrIllFormed" else "splitsOnEveryNonOverlappingMatch"

      regexConformance = supergroup "conformance (docs/specification/regex.md)" [
          dotIncludesNewline,
          leftmostLongest,
          wholeStringAnchors,
          minimalCoreGrammar,
          escapingRules,
          illFormedPatterns]
        where
          -- "." matches ANY character, including newline -- a deliberate departure from every host's
          -- native "." (which excludes \n). No printer may ever emit a bare ".".
          dotIncludesNewline = subgroup "dot includes newline" [
            matchesCase "dot matches newline" "a.b" "a\nb" True,
            matchesCase "dot-star spans newline" "a.*b" "a\nx\ny\nb" True,
            findCase "find dot matches newline char" "." "\n" (Just "\n"),
            matchesCase "explicit non-newline class excludes it" "a[^\n]b" "a\nb" False,
            matchesCase "explicit non-newline class matches other chars" "a[^\n]b" "aXb" True]

          -- Leftmost-longest (POSIX) is the canonical Hydra match preference, even though most hosts
          -- are natively leftmost-first (Perl/ECMA). The classic divergent case is `a|ab` on "ab":
          -- leftmost-first stops at the first alternative that matches ("a"); leftmost-longest picks
          -- the longest match starting at the leftmost position ("ab").
          leftmostLongest = subgroup "leftmost-longest match preference" [
            findCase "alternation prefers the longer branch" "a|ab" "ab" (Just "ab"),
            findCase "alternation longer-branch-first is equivalent" "ab|a" "ab" (Just "ab"),
            matchesCase "whole-string alternation picks the matching length" "a|ab" "ab" True,
            findCase "quantifier greediness picks the longest run" "a+" "aaa" (Just "aaa")]

          -- Anchors ^ and $ are STRING-boundary, not line-boundary: they match the empty string at
          -- the start/end of the whole input, never at an internal line break. `matches` is always
          -- whole-string-anchored regardless of explicit ^/$ in the pattern.
          wholeStringAnchors = subgroup "whole-string anchors" [
            matchesCase "matches is anchored without explicit anchors" "abc" "abc" True,
            matchesCase "matches rejects a proper substring" "abc" "xabcx" False,
            matchesCase "^ and $ do not match at internal line breaks" "^b$" "a\nb\nc" False,
            findCase "^ anchors to the start of the whole input, not a line" "^b" "a\nb" Nothing,
            findCase "$ anchors to the end of the whole input, not a line" "b$" "b\nc" Nothing,
            matchesCase "^...$ spanning the whole multi-line input matches" "^a\n.*c$" "a\nb\nc" True]

          -- The minimal-core grammar: literals, character classes (incl. negation and ranges), ".",
          -- quantifiers (? * + {n} {n,} {n,m}), alternation, anchors, and (non-capturing) grouping.
          minimalCoreGrammar = subgroup "minimal-core grammar" [
            matchesCase "character class range" "[a-z]+" "hello" True,
            matchesCase "negated character class" "[^0-9]+" "hello" True,
            matchesCase "negated character class rejects a member" "[^0-9]+" "hello1" False,
            matchesCase "zero-or-one quantifier" "colou?r" "color" True,
            matchesCase "zero-or-one quantifier with the optional char" "colou?r" "colour" True,
            matchesCase "exact-count quantifier {n}" "a{3}" "aaa" True,
            matchesCase "exact-count quantifier {n} rejects short input" "a{3}" "aa" False,
            matchesCase "at-least quantifier {n,}" "a{2,}" "aaaa" True,
            matchesCase "at-least quantifier {n,} rejects short input" "a{2,}" "a" False,
            matchesCase "range quantifier {n,m}" "a{2,4}" "aaa" True,
            matchesCase "range quantifier {n,m} rejects out-of-range" "a{2,4}" "aaaaa" False,
            matchesCase "non-capturing group with quantifier" "(ab)+" "ababab" True,
            matchesCase "empty group matches the empty string" "a()b" "ab" True,
            matchesCase "empty whole pattern matches only the empty string" "" "" True,
            matchesCase "nested groups" "((a)(b))+" "abab" True,
            matchesCase "alternation nested inside a group" "(cat|dog)s" "cats" True,
            matchesCase "alternation nested inside a group, other branch" "(cat|dog)s" "dogs" True,
            matchesCase "quantified group of an alternation" "(a|b){2,3}" "aba" True,
            matchesCase "quantified group of an alternation rejects out-of-range" "(a|b){2,3}" "a" False]

          -- Escaping: a literal metacharacter is always written with a backslash (never positional
          -- conventions), and the class-internal metaset { \ ] ^ - } is always escaped uniformly.
          -- A dash is NOT special at top level (outside a class): "a-b" there is three literals, not
          -- a range -- ranges only exist inside character classes.
          escapingRules = subgroup "escaping rules" [
            matchesCase "escaped metacharacter matches literally" "a\\.b" "a.b" True,
            matchesCase "escaped metacharacter does not match the wildcard behavior" "a\\.b" "aXb" False,
            matchesCase "escaped star matches a literal star" "a\\*b" "a*b" True,
            matchesCase "escaped open-paren matches a literal paren" "a\\(b" "a(b" True,
            matchesCase "escaped pipe matches a literal pipe, not alternation" "a\\|b" "a|b" True,
            matchesCase "unescaped dash at top level is a literal, not a range" "a-b" "a-b" True,
            matchesCase "top-level dash does not form a range" "a-b" "aXb" False,
            matchesCase "escaped dash inside a class is a literal dash" "[a\\-z]" "-" True,
            matchesCase "unescaped dash forms a range inside a class" "[a-z]" "-" False,
            matchesCase "escaped bracket inside a class is a literal bracket" "[\\]]" "]" True,
            matchesCase "escaped caret inside a class is a literal caret (not negation)" "[a\\^]" "^" True,
            -- Pattern text (Hydra regex syntax) is: [a\\] -- a class containing 'a' and an escaped
            -- backslash. backslash1 is Haskell's own string escape, backslash2 is Hydra's.
            matchesCase "escaped backslash inside a class is a literal backslash" "[a\\\\]" "\\" True]

          -- Ill-formed patterns (ones that do not parse under hydra.core.parse.regex) fail uniformly as
          -- "no match" on every host -- never a host-specific engine exception -- per the portable
          -- failure channel (scope item 2).
          illFormedPatterns = subgroup "ill-formed patterns fail as portable no-match" [
            matchesCaseIllFormed "unbalanced open paren is ill-formed, so matches is false" "(a" "a" False,
            findCaseIllFormed "unbalanced open paren is ill-formed, so find is nothing" "(a" "a" Nothing,
            findAllCaseIllFormed "unbalanced open paren is ill-formed, so findAll is empty" "(a" "aaa" [],
            replaceCaseIllFormed "unbalanced open paren is ill-formed, so replace is a no-op" "(a" "X" "aaa" "aaa",
            replaceAllCaseIllFormed "unbalanced open paren is ill-formed, so replaceAll is a no-op" "(a" "X" "aaa" "aaa",
            splitCaseIllFormed "unbalanced open paren is ill-formed, so split returns the input unsplit" "(a" "aaa" ["aaa"],
            matchesCaseIllFormed "empty alternation branch is ill-formed" "a|" "a" False,
            matchesCaseIllFormed "empty character class is ill-formed" "a[]b" "ab" False,
            matchesCaseIllFormed "up-to-m quantifier form is not in the minimal core, so it is ill-formed" "a{,2}" "a" False]

          matchesCase name pat input result =
            primCaseWithProvisions name ["testsMinimalCoreGrammar"] DefRegex.matches [string pat, string input] (boolean result)
          matchesCaseIllFormed name pat input result =
            primCaseWithProvisions name ["falseWhenNoMatchOrIllFormed"] DefRegex.matches [string pat, string input] (boolean result)
          findCase name pat input result =
            primCaseWithProvisions name [if result == Nothing then "noneWhenNoMatch" else "returnsLeftmostLongestMatch"] DefRegex.find [string pat, string input] (Core.termOptional $ optStr result)
            where
              optStr Nothing = nothing
              optStr (Just s) = just (string s)
          findCaseIllFormed name pat input result =
            primCaseWithProvisions name ["noneWhenNoMatch"] DefRegex.find [string pat, string input] (Core.termOptional $ optStr result)
            where
              optStr Nothing = nothing
              optStr (Just s) = just (string s)
          findAllCaseIllFormed name pat input result =
            primCaseWithProvisions name ["emptyListWhenNoMatch"] DefRegex.findAll [string pat, string input] (list (string <$> result))
          replaceCaseIllFormed name pat repl input result =
            primCaseWithProvisions name ["unchangedWhenNoMatch"] DefRegex.replace [string pat, string repl, string input] (string result)
          replaceAllCaseIllFormed name pat repl input result =
            primCaseWithProvisions name ["unchangedWhenNoMatch"] DefRegex.replaceAll [string pat, string repl, string input] (string result)
          splitCaseIllFormed name pat input result =
            primCaseWithProvisions name ["singleElementListWhenNoMatchOrIllFormed"] DefRegex.split [string pat, string input] (list (string <$> result))
