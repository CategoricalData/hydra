module Hydra.Sources.Test.Lib.Regex where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.lib.regex"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for hydra.lib.regex primitives")
  where
    elements = [
        Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for hydra.lib.regex primitives" $
    supergroup "hydra.lib.regex primitives" [
      regexMatches,
      regexFind,
      regexFindAll,
      regexReplace,
      regexReplaceAll,
      regexSplit]
    where
      regexMatches = subgroup "matches" [
        -- Basic matching
        test2 "exact match" "hello" "hello" True,
        test2 "pattern match" "[a-z]+" "hello" True,
        test2 "no match" "[0-9]+" "hello" False,

        -- Anchoring behavior (matches checks full string)
        test2 "partial content does not match" "[a-z]+" "hello123" False,
        test2 "digit pattern" "[0-9]+" "12345" True,
        test2 "mixed pattern" "[a-z]+[0-9]+" "hello123" True,

        -- Empty string
        test2 "empty pattern matches empty" "" "" True,
        test2 "empty pattern does not match non-empty" "" "hello" False,
        test2 "star matches empty" "a*" "" True,

        -- Special regex features
        test2 "alternation" "cat|dog" "cat" True,
        test2 "alternation second" "cat|dog" "dog" True,
        test2 "alternation no match" "cat|dog" "bird" False,
        test2 "quantifier" "ab?c" "ac" True,
        test2 "quantifier with optional" "ab?c" "abc" True]
        where
          test2 name pat input result =
            primCase name _regex_matches [string pat, string input] (boolean result)

      regexFind = subgroup "find" [
        test2 "simple find" "[0-9]+" "abc123def" (Just "123"),
        test2 "no match" "[0-9]+" "abcdef" Nothing,
        test2 "find first" "[a-z]+" "123abc456def" (Just "abc"),
        test2 "empty input" "[0-9]+" "" Nothing,
        test2 "full match" ".*" "hello" (Just "hello")]
        where
          test2 name pat input result =
            primCase name _regex_find [string pat, string input] (Core.termMaybe $ optStr result)
          optStr Nothing = nothing
          optStr (Just s) = just (string s)

      regexFindAll = subgroup "findAll" [
        test2 "multiple matches" "[0-9]+" "a1b2c3" ["1", "2", "3"],
        test2 "no matches" "[0-9]+" "abc" [],
        test2 "overlapping words" "[a-z]+" "abc def ghi" ["abc", "def", "ghi"],
        test2 "single match" "hello" "say hello world" ["hello"]]
        where
          test2 name pat input result =
            primCase name _regex_findAll [string pat, string input] (list (string <$> result))

      regexReplace = subgroup "replace" [
        test3 "basic replace" "[0-9]+" "X" "abc123def456" "abcXdef456",
        test3 "no match" "[0-9]+" "X" "abcdef" "abcdef",
        test3 "replace at start" "^[a-z]+" "X" "abc123" "X123",
        test3 "empty replacement" "[0-9]+" "" "abc123def" "abcdef"]
        where
          test3 name pat repl input result =
            primCase name _regex_replace [string pat, string repl, string input] (string result)

      regexReplaceAll = subgroup "replaceAll" [
        test3 "replace all digits" "[0-9]+" "X" "a1b2c3" "aXbXcX",
        test3 "no match" "[0-9]+" "X" "abc" "abc",
        test3 "replace all words" "[a-z]+" "X" "abc 123 def" "X 123 X",
        test3 "empty replacement" "[0-9]+" "" "a1b2c3" "abc"]
        where
          test3 name pat repl input result =
            primCase name _regex_replaceAll [string pat, string repl, string input] (string result)

      regexSplit = subgroup "split" [
        test2 "split on comma" "," "a,b,c" ["a", "b", "c"],
        test2 "split on spaces" " +" "a b  c" ["a", "b", "c"],
        test2 "no match" "," "abc" ["abc"],
        test2 "split on digits" "[0-9]+" "a1b2c" ["a", "b", "c"],
        test2 "trailing delimiter" "," "a,b," ["a", "b", ""]]
        where
          test2 name pat input result =
            primCase name _regex_split [string pat, string input] (list (string <$> result))
