module Hydra.Sources.Test.Primitives (testPrimitivesModule) where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.Sources.Libraries
import Hydra.Sources.Testing
import Hydra.Testing
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Annotations as Ann

import qualified Data.List as L


primitivesNs = Namespace "hydra/test/primitives"

testPrimitivesModule :: Module Kv
testPrimitivesModule = Module primitivesNs elements [hydraCoreModule, hydraTestingModule] $
    Just "Test cases for primitive functions"
  where
    elements = [
      groupElement "primitiveFunctionTestCases" allPrimitives]

def :: String -> Type Kv -> Term Kv -> Element Kv
def lname typ term = Element (fromQname primitivesNs lname) (epsilonEncodeType typ) term

groupElement :: String -> TestGroup Kv -> Element Kv
groupElement lname group = def lname (Types.variable "ignored") $ encodeGroup group
  where
    encodeGroup (TestGroup name desc groups cases) = Terms.record _TestGroup [
      Field _TestGroup_name $ Terms.string name,
      Field _TestGroup_description $ Terms.optional (Terms.string <$> desc),
      Field _TestGroup_subgroups $ Terms.list (encodeGroup <$> groups),
      Field _TestGroup_cases $ Terms.list (encodeCase <$> cases)]
    encodeCase (TestCase desc input output) = Terms.record _TestCase [
      Field _TestCase_description $ Terms.optional (Terms.string <$> desc),
      Field _TestCase_input $ sigmaEncodeTerm input,
      Field _TestCase_output $ sigmaEncodeTerm output]

testCase = TestCase Nothing

primCase :: Name -> [Term Kv] -> Term Kv -> TestCase Kv
primCase name args output = TestCase Nothing input output
  where
    input = L.foldl (\a arg -> a @@ arg) (primitive name) args

allPrimitives :: TestGroup Kv
allPrimitives = TestGroup "All primitives" Nothing [stringPrimitives] []

stringPrimitives :: TestGroup Kv
stringPrimitives = TestGroup "hydra/lib/strings primitives" Nothing groups []
  where
    groups = [stringsCat, stringsLength, stringsSplitOn, stringsToLower, stringsToUpper]

stringsCat :: TestGroup Kv
stringsCat = TestGroup "cat" Nothing [] [
    test ["one", "two", "three"] "onetwothree",
    test ["", "one", "", ""] "one",
    test [] ""]
  where
    test ls result = primCase _strings_length [list (string <$> ls)] (string result)

stringsLength :: TestGroup Kv
stringsLength = TestGroup "length" Nothing [] [
    test "" 0,
    test "a" 1,
    test "one" 3]
  where
    test s result = primCase _strings_length [string s] (int32 result)

stringsSplitOn :: TestGroup Kv
stringsSplitOn = TestGroup "splitOn" Nothing [] [
    test "ss" "Mississippi" ["Mi", "i", "ippi"],
    test "Mississippi" "Mississippi" ["", ""],

    test " " "one two three" ["one", "two", "three"],
    test " " " one two three " ["", "one", "two", "three", ""],
    test " " "  one two three" ["", "", "one", "two", "three"],
    test "  " "  one two three" ["", "one two three"],

    test "aa" "aaa" ["", "a"],

    test "a" "" [""],

    test "" "abc" ["a", "b", "c"],
    test "" "" [""]]
  where
    test s0 s1 result = primCase _strings_splitOn [string s0, string s1] (list (string <$> result))

stringsToLower :: TestGroup Kv
stringsToLower = TestGroup "toLower" Nothing [] [
    test "One TWO threE" "one two three",
    test "Abc123" "abc123"]
  where
    test s result = primCase _strings_toLower [string s] (string result)

stringsToUpper :: TestGroup Kv
stringsToUpper = TestGroup "toUpper" Nothing [] [
    test "One TWO threE" "ONE TWO THREE",
    test "Abc123" "ABC123"]
  where
    test s result = primCase _strings_toUpper [string s] (string result)
