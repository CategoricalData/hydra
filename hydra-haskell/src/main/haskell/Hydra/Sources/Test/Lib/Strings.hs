module Hydra.Sources.Test.Lib.Strings (stringPrimitiveTests) where

import Hydra.Dsl.Tests


stringPrimitiveTests :: TestGroup
stringPrimitiveTests = TestGroup "hydra.lib.strings primitives" Nothing groups []
  where
    groups = [stringsCat, stringsLength, stringsSplitOn, stringsToLower, stringsToUpper]

stringsCat :: TestGroup
stringsCat = TestGroup "cat" Nothing [] [
    test "1" ["one", "two", "three"] "onetwothree",
    test "2" ["", "one", "", ""] "one",
    test "3" [] ""]
  where
    test name ls result = primCase name _strings_cat [list (string <$> ls)] (string result)

stringsLength :: TestGroup
stringsLength = TestGroup "length" Nothing [] [
    test "1" "" 0,
    test "2" "a" 1,
    test "3" "one" 3]
  where
    test name s result = primCase name _strings_length [string s] (int32 result)

stringsSplitOn :: TestGroup
stringsSplitOn = TestGroup "splitOn" Nothing [] [
    test "1" "ss" "Mississippi" ["Mi", "i", "ippi"],
    test "2" "Mississippi" "Mississippi" ["", ""],

    test "3" " " "one two three" ["one", "two", "three"],
    test "4" " " " one two three " ["", "one", "two", "three", ""],
    test "5" " " "  one two three" ["", "", "one", "two", "three"],
    test "6" "  " "  one two three" ["", "one two three"],

    test "6" "aa" "aaa" ["", "a"],

    test "7" "a" "" [""],

    test "8" "" "abc" ["", "a", "b", "c"],
    test "9" "" "" [""]]
  where
    test name s0 s1 result = primCase name _strings_splitOn [string s0, string s1] (list (string <$> result))

stringsToLower :: TestGroup
stringsToLower = TestGroup "toLower" Nothing [] [
    test "1" "One TWO threE" "one two three",
    test "2" "Abc123" "abc123"]
  where
    test name s result = primCase name _strings_toLower [string s] (string result)

stringsToUpper :: TestGroup
stringsToUpper = TestGroup "toUpper" Nothing [] [
    test "1" "One TWO threE" "ONE TWO THREE",
    test "2" "Abc123" "ABC123"]
  where
    test name s result = primCase name _strings_toUpper [string s] (string result)
