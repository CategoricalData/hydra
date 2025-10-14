module Hydra.Sources.Test.Lib.Strings (stringPrimitiveTests) where

import Hydra.Dsl.Tests


stringPrimitiveTests :: TestGroup
stringPrimitiveTests = TestGroup "hydra.lib.strings primitives" Nothing groups []
  where
    groups = [
      stringsCat,
      stringsCat2,
      stringsCharAt,
      stringsFromList,
      stringsIntercalate,
      stringsLength,
      stringsLines,
      stringsNull,
      stringsSplitOn,
      stringsToList,
      stringsToLower,
      stringsToUpper,
      stringsUnlines]

stringsCat :: TestGroup
stringsCat = TestGroup "cat" Nothing [] [
    -- Basic functionality
    test "basic concatenation" ["one", "two", "three"] "onetwothree",
    test "single string" ["hello"] "hello",
    test "empty list" [] "",

    -- Empty string handling
    test "with empty strings" ["", "one", "", ""] "one",
    test "all empty strings" ["", "", "", ""] "",

    -- Unicode correctness
    test "unicode strings" ["\241", "\19990", "\127757"] "\241\19990\127757",  -- ñ, 世, 🌍
    test "combining characters" ["e", "\769"] "e\769",  -- e + combining acute accent

    -- Special characters (control characters may be handled specially)
    test "control characters" ["\n", "\t", "\r"] "\n\t\r",
    test "null character" ["hello", "\0", "world"] "hello\0world"]
  where
    test name ls result = primCase name _strings_cat [list (string <$> ls)] (string result)

stringsCat2 :: TestGroup
stringsCat2 = TestGroup "cat2" Nothing [] [
    test "basic concatenation" "hello" "world" "helloworld",
    test "empty first string" "" "world" "world",
    test "empty second string" "hello" "" "hello",
    test "both empty strings" "" "" "",
    test "unicode characters" "\241" "\19990" "\241\19990",  -- ñ, 世
    test "special characters" "\n" "\t" "\n\t",
    test "null characters" "hello\0" "world" "hello\0world"]
  where
    test name s1 s2 result = primCase name _strings_cat2 [string s1, string s2] (string result)

stringsCharAt :: TestGroup
stringsCharAt = TestGroup "charAt" Nothing [] [
    test "first character" 0 "hello" 104,  -- 'h'
    test "middle character" 2 "hello" 108, -- 'l'
    test "last character" 4 "hello" 111,   -- 'o'
    test "single character string" 0 "a" 97,      -- 'a'
    test "unicode character" 0 "\241" 241,    -- ñ
    test "multi-byte unicode" 0 "\19990" 19990,  -- 世
    test "second of combining pair" 1 "e\769" 769]  -- combining acute accent
    -- TODO: failure cases (need syntax support)
    -- test "negative index" (-1) "hello" <error>
    -- test "index out of bounds" 10 "hello" <error>
    -- test "index on empty string" 0 "" <error>
  where
    test name idx s result = primCase name _strings_charAt [int32 idx, string s] (int32 result)

stringsFromList :: TestGroup
stringsFromList = TestGroup "fromList" Nothing [] [
    test "basic ascii string" [104, 101, 108, 108, 111] "hello",
    test "empty code point list" [] "",
    test "single character" [97] "a",
    test "unicode characters" [241, 19990, 127757] "\241\19990\127757",  -- ñ, 世, 🌍
    test "combining character sequence" [101, 769] "e\769",  -- e + combining acute
    test "special characters" [10, 9, 13] "\n\t\r",
    test "null character" [104, 0, 105] "h\0i"]  -- h, null, i
    -- TODO: failure cases (need syntax support)
    -- test "negative code point" [-1] <error>
    -- test "invalid code point" [1114112] <error>  -- beyond valid Unicode range
  where
    test name codePoints result = primCase name _strings_fromList [list (int32 <$> codePoints)] (string result)

stringsIntercalate :: TestGroup
stringsIntercalate = TestGroup "intercalate" Nothing [] [
    -- Basic functionality
    test "comma separator" "," ["one", "two", "three"] "one,two,three",
    test "empty separator" "" ["a", "b", "c"] "abc",
    test "multi-character separator" " | " ["A", "B", "C"] "A | B | C",

    -- Edge cases
    test "empty string list" "," [] "",
    test "single item list" "," ["only"] "only",
    test "empty strings in list" "," ["", "a", ""] ",a,",

    -- Unicode and special characters
    test "unicode separator" "\127757" ["link1", "link2"] "link1\127757link2",  -- 🌍
    test "newline separator" "\n" ["line1", "line2"] "line1\nline2"]
  where
    test name sep strs result = primCase name _strings_intercalate [string sep, list (string <$> strs)] (string result)

stringsLength :: TestGroup
stringsLength = TestGroup "length" Nothing [] [
    test "empty string" "" 0,
    test "single character" "a" 1,
    test "basic word" "hello" 5,
    test "unicode characters" "\241\19990\127757" 3,  -- ñ, 世, 🌍 (verifies code point count)
    test "combining character sequence" "e\769" 2,  -- e + combining acute (separate code points)
    test "special characters" "\n\t\r" 3]
  where
    test name s result = primCase name _strings_length [string s] (int32 result)

stringsLines :: TestGroup
stringsLines = TestGroup "lines" Nothing [] [
    -- Basic functionality
    test "single line" "hello world" ["hello world"],
    test "two lines" "hello\nworld" ["hello", "world"],
    test "three lines" "one\ntwo\nthree" ["one", "two", "three"],

    -- Edge cases with newlines
    test "empty string" "" [],
    test "just newline" "\n" [""],
    test "trailing newline" "hello\n" ["hello"],
    test "leading newline" "\nhello" ["", "hello"],

    -- Consecutive newlines
    test "multiple consecutive newlines" "a\n\nb" ["a", "", "b"],

    -- Unicode and other whitespace
    test "unicode content" "\241\n\19990" ["\241", "\19990"],  -- ñ, 世
    test "tabs not split" "a\tb\nc" ["a\tb", "c"]]  -- only \n splits, not \t
  where
    test name s result = primCase name _strings_lines [string s] (list (string <$> result))

stringsNull :: TestGroup
stringsNull = TestGroup "null" Nothing [] [
    test "empty string" "" True,
    test "single character" "a" False,
    test "space" " " False,
    test "unicode space" "\160" False,  -- non-breaking space
    test "newline" "\n" False,
    test "null character" "\0" False,
    test "multi-character" "hello" False]
  where
    test name s result = primCase name _strings_null [string s] (boolean result)

stringsSplitOn :: TestGroup
stringsSplitOn = TestGroup "splitOn" Nothing [] [
    -- Basic functionality
    test "basic separator" "ss" "Mississippi" ["Mi", "i", "ippi"],
    test "single char separator" " " "one two three" ["one", "two", "three"],
    test "multi-char separator" "  " "a  b  c" ["a", "b", "c"],
    test "separator not found" "x" "hello" ["hello"],

    -- Edge cases with separator positions
    test "separator at start" "h" "hello" ["", "ello"],
    test "separator at end" "o" "hello" ["hell", ""],
    test "leading and trailing separator" " " " one two " ["", "one", "two", ""],
    test "whole string as separator" "Mississippi" "Mississippi" ["", ""],

    -- Consecutive and overlapping separators
    test "consecutive separators" " " "a  b" ["a", "", "b"],
    test "multiple occurrences" "l" "hello" ["he", "", "o"],
    test "overlapping pattern" "aa" "aaa" ["", "a"],

    -- Empty string cases
    test "empty separator" "" "abc" ["", "a", "b", "c"],
    test "separator on empty string" "x" "" [""],
    test "both empty" "" "" [""],

    -- Single character cases
    test "single char both" "a" "a" ["", ""],

    -- Unicode
    test "unicode separator" "\19990" "hello\19990world" ["hello", "world"],  -- 世
    test "unicode content" "," "\241,\19990,\127757" ["\241", "\19990", "\127757"],  -- ñ,世,🌍

    -- Special characters
    test "newline separator" "\n" "line1\nline2\nline3" ["line1", "line2", "line3"]]
  where
    test name s0 s1 result = primCase name _strings_splitOn [string s0, string s1] (list (string <$> result))

stringsToList :: TestGroup
stringsToList = TestGroup "toList" Nothing [] [
    -- Basic functionality
    test "empty string" "" [],
    test "single character" "a" [97],
    test "basic word" "hello" [104, 101, 108, 108, 111],

    -- Unicode
    test "unicode characters" "\241\19990\127757" [241, 19990, 127757],  -- ñ, 世, 🌍
    test "combining character sequence" "e\769" [101, 769],  -- e + combining acute

    -- Special characters
    test "control characters" "\n\t\r" [10, 9, 13],
    test "null character" "h\0i" [104, 0, 105]]
  where
    test name s result = primCase name _strings_toList [string s] (list (int32 <$> result))

stringsToLower :: TestGroup
stringsToLower = TestGroup "toLower" Nothing [] [
    -- Basic functionality
    test "mixed case" "Hello World" "hello world",
    test "all uppercase" "HELLO" "hello",
    test "all lowercase" "hello" "hello",
    test "empty string" "" "",

    -- Non-letter characters unchanged
    test "with numbers and punctuation" "Abc123, XYZ!" "abc123, xyz!",
    test "control characters" "\n\t\r" "\n\t\r",

    -- Unicode
    test "unicode accented chars" "\209\193\201\205\211\218" "\241\225\233\237\243\250"]  -- ÑÁÉÍÓÚ -> ñáéíóú
  where
    test name s result = primCase name _strings_toLower [string s] (string result)

stringsToUpper :: TestGroup
stringsToUpper = TestGroup "toUpper" Nothing [] [
    -- Basic functionality
    test "mixed case" "hello World" "HELLO WORLD",
    test "all lowercase" "hello" "HELLO",
    test "all uppercase" "HELLO" "HELLO",
    test "empty string" "" "",

    -- Non-letter characters unchanged
    test "with numbers and punctuation" "abc123, xyz!" "ABC123, XYZ!",
    test "control characters" "\n\t\r" "\n\t\r",

    -- Unicode
    test "unicode accented chars" "\241\225\233\237\243\250" "\209\193\201\205\211\218"]  -- ñáéíóú -> ÑÁÉÍÓÚ
  where
    test name s result = primCase name _strings_toUpper [string s] (string result)

stringsUnlines :: TestGroup
stringsUnlines = TestGroup "unlines" Nothing [] [
    -- Basic functionality
    test "multiple lines" ["one", "two", "three"] "one\ntwo\nthree\n",
    test "single line" ["hello"] "hello\n",
    test "empty list" [] "",

    -- Empty strings in list
    test "with empty lines" ["hello", "", "world"] "hello\n\nworld\n",
    test "all empty lines" ["", "", ""] "\n\n\n",

    -- Unicode
    test "unicode content" ["\241o\241o", "\19990\30028"] "\241o\241o\n\19990\30028\n"]  -- ñoño, 世界
  where
    test name strs result = primCase name _strings_unlines [list (string <$> strs)] (string result)
