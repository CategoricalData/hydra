module Hydra.Sources.Test.Lib.Strings (stringPrimitiveTests) where

import Hydra.Dsl.Tests


stringPrimitiveTests :: TestGroup
stringPrimitiveTests = TestGroup "hydra.lib.strings primitives" Nothing groups []
  where
    groups = [stringsCat, stringsCat2, stringsCharAt, stringsFromList,
              stringsIntercalate, stringsLength, stringsLines, stringsNull,
              stringsSplitOn, stringsToList, stringsToLower, stringsToUpper,
              stringsUnlines]

stringsCat :: TestGroup
stringsCat = TestGroup "cat" Nothing [] [
    test "basic concatenation" ["one", "two", "three"] "onetwothree",
    test "with empty strings" ["", "one", "", ""] "one",
    test "empty list" [] "",
    test "single string" ["hello"] "hello",
    test "many empty strings" ["", "", "", ""] "",
    test "unicode strings" ["ñ", "世", "🌍"] "ñ世🌍",
    test "special characters" ["\n", "\t", "\r"] "\n\t\r",
    test "numbers as strings" ["1", "2", "3"] "123",
    test "spaces" [" ", " ", " "] "   ",
    test "mixed content" ["Hello", " ", "World", "!"] "Hello World!"]
  where
    test name ls result = primCase name _strings_cat [list (string <$> ls)] (string result)

stringsCat2 :: TestGroup
stringsCat2 = TestGroup "cat2" Nothing [] [
    test "basic concatenation" "hello" "world" "helloworld",
    test "empty first string" "" "world" "world",
    test "empty second string" "hello" "" "hello",
    test "both empty strings" "" "" "",
    test "with spaces" "hello " "world" "hello world",
    test "unicode characters" "ñ" "世" "ñ世",
    test "numeric strings" "123" "456" "123456",
    test "special characters" "\n" "\t" "\n\t"]
  where
    test name s1 s2 result = primCase name _strings_cat2 [string s1, string s2] (string result)

stringsCharAt :: TestGroup
stringsCharAt = TestGroup "charAt" Nothing [] [
    test "first character" 0 "hello" 104,  -- 'h'
    test "middle character" 2 "hello" 108, -- 'l'
    test "last character" 4 "hello" 111,   -- 'o'
    test "single character string" 0 "a" 97,      -- 'a'
    test "unicode character" 0 "ñ" 241,    -- 'ñ'
    test "space character" 0 " " 32,       -- space
    test "newline character" 0 "\n" 10,    -- newline
    test "tab character" 0 "\t" 9]         -- tab
  where
    test name idx s result = primCase name _strings_charAt [int32 idx, string s] (int32 result)

stringsFromList :: TestGroup
stringsFromList = TestGroup "fromList" Nothing [] [
    test "basic ascii string" [104, 101, 108, 108, 111] "hello",
    test "empty code point list" [] "",
    test "single character" [97] "a",
    test "with spaces" [104, 32, 105] "h i",
    test "unicode characters" [241, 19990, 127757] "ñ世🌍",
    test "numeric characters" [49, 50, 51] "123",
    test "special characters" [10, 9, 13] "\n\t\r"]
  where
    test name codePoints result = primCase name _strings_fromList [list (int32 <$> codePoints)] (string result)

stringsIntercalate :: TestGroup
stringsIntercalate = TestGroup "intercalate" Nothing [] [
    test "comma separator" "," ["one", "two", "three"] "one,two,three",
    test "space separator" " " ["hello", "world"] "hello world",
    test "empty separator" "" ["a", "b", "c"] "abc",
    test "multi-character separator" " | " ["A", "B", "C"] "A | B | C",
    test "empty string list" "," [] "",
    test "single item list" "," ["only"] "only",
    test "empty strings in list" "," ["", "a", ""] ",a,",
    test "unicode separator" "🔗" ["link1", "link2"] "link1🔗link2"]
  where
    test name sep strs result = primCase name _strings_intercalate [string sep, list (string <$> strs)] (string result)

stringsLength :: TestGroup
stringsLength = TestGroup "length" Nothing [] [
    test "empty string" "" 0,
    test "single character" "a" 1,
    test "basic word" "one" 3,
    test "string with spaces" "hello world" 11,
    test "unicode characters" "ñ世🌍" 3,
    test "special characters" "\n\t\r" 3,
    test "numeric string" "12345" 5,
    test "long string" "this is a longer string for testing" 35]
  where
    test name s result = primCase name _strings_length [string s] (int32 result)

stringsLines :: TestGroup
stringsLines = TestGroup "lines" Nothing [] [
    test "single line" "hello world" ["hello world"],
    test "two lines" "hello\nworld" ["hello", "world"],
    test "three lines" "one\ntwo\nthree" ["one", "two", "three"],
    test "empty string" "" [],
    test "just newline" "\n" [""],
    test "trailing newline" "hello\n" ["hello"],
    test "leading newline" "\nhello" ["", "hello"],
    test "multiple consecutive newlines" "a\n\nb" ["a", "", "b"]]
  where
    test name s result = primCase name _strings_lines [string s] (list (string <$> result))

stringsNull :: TestGroup
stringsNull = TestGroup "null" Nothing [] [
    test "empty string" "" True,
    test "single character" "a" False,
    test "space only" " " False,
    test "basic word" "hello" False,
    test "newline only" "\n" False,
    test "tab only" "\t" False]
  where
    test name s result = primCase name _strings_null [string s] (boolean result)

stringsSplitOn :: TestGroup
stringsSplitOn = TestGroup "splitOn" Nothing [] [
    test "double s in Mississippi" "ss" "Mississippi" ["Mi", "i", "ippi"],
    test "whole string as separator" "Mississippi" "Mississippi" ["", ""],
    test "space separated words" " " "one two three" ["one", "two", "three"],
    test "leading and trailing spaces" " " " one two three " ["", "one", "two", "three", ""],
    test "multiple consecutive spaces" " " "  one two three" ["", "", "one", "two", "three"],
    test "double space separator" "  " "  one two three" ["", "one two three"],
    test "overlapping pattern aa in aaa" "aa" "aaa" ["", "a"],
    test "separator on empty string" "a" "" [""],
    test "empty separator on abc" "" "abc" ["", "a", "b", "c"],
    test "both separator and string empty" "" "" [""],
    test "separator not found" "x" "hello" ["hello"],
    test "separator at start" "h" "hello" ["", "ello"],
    test "separator at end" "o" "hello" ["hell", ""],
    test "multiple same separators" "l" "hello" ["he", "", "o"],
    test "single character string and separator" "a" "a" ["", ""],
    test "unicode separator" "世" "hello世world世!" ["hello", "world", "!"],
    test "multi-character separator" "ab" "cabbage" ["c", "bage"],
    test "newline separator" "\n" "line1\nline2\nline3" ["line1", "line2", "line3"]]
  where
    test name s0 s1 result = primCase name _strings_splitOn [string s0, string s1] (list (string <$> result))

stringsToList :: TestGroup
stringsToList = TestGroup "toList" Nothing [] [
    test "empty string" "" [],
    test "single character" "a" [97],
    test "basic word" "hello" [104, 101, 108, 108, 111],
    test "with space" "h i" [104, 32, 105],
    test "unicode characters" "ñ世🌍" [241, 19990, 127757],
    test "numeric string" "123" [49, 50, 51],
    test "special characters" "\n\t\r" [10, 9, 13]]
  where
    test name s result = primCase name _strings_toList [string s] (list (int32 <$> result))

stringsToLower :: TestGroup
stringsToLower = TestGroup "toLower" Nothing [] [
    test "mixed case sentence" "One TWO threE" "one two three",
    test "alphanumeric mixed case" "Abc123" "abc123",
    test "all uppercase" "HELLO" "hello",
    test "all lowercase" "hello" "hello",
    test "empty string" "" "",
    test "single uppercase char" "A" "a",
    test "single lowercase char" "a" "a",
    test "with spaces" "Hello World" "hello world",
    test "with punctuation" "Hello, World!" "hello, world!",
    test "unicode accented chars" "ÑÁÉÍÓÚ" "ñáéíóú",
    test "numbers only" "12345" "12345",
    test "special characters only" "\n\t\r" "\n\t\r"]
  where
    test name s result = primCase name _strings_toLower [string s] (string result)

stringsToUpper :: TestGroup
stringsToUpper = TestGroup "toUpper" Nothing [] [
    test "mixed case sentence" "One TWO threE" "ONE TWO THREE",
    test "alphanumeric mixed case" "Abc123" "ABC123",
    test "all lowercase" "hello" "HELLO",
    test "all uppercase" "HELLO" "HELLO",
    test "empty string" "" "",
    test "single lowercase char" "a" "A",
    test "single uppercase char" "A" "A",
    test "with spaces" "hello world" "HELLO WORLD",
    test "with punctuation" "hello, world!" "HELLO, WORLD!",
    test "unicode accented chars" "ñáéíóú" "ÑÁÉÍÓÚ",
    test "numbers only" "12345" "12345",
    test "special characters only" "\n\t\r" "\n\t\r"]
  where
    test name s result = primCase name _strings_toUpper [string s] (string result)

stringsUnlines :: TestGroup
stringsUnlines = TestGroup "unlines" Nothing [] [
    test "basic two lines" ["hello", "world"] "hello\nworld\n",
    test "single line" ["hello"] "hello\n",
    test "empty list" [] "",
    test "three lines" ["one", "two", "three"] "one\ntwo\nthree\n",
    test "with empty lines" ["hello", "", "world"] "hello\n\nworld\n",
    test "all empty lines" ["", "", ""] "\n\n\n",
    test "unicode content" ["ñoño", "世界"] "ñoño\n世界\n"]
  where
    test name strs result = primCase name _strings_unlines [list (string <$> strs)] (string result)