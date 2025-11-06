module Hydra.Sources.Test.Lib.Lists (listPrimitiveTests) where

import Hydra.Dsl.Tests
import Hydra.Dsl.Terms


listPrimitiveTests :: TestGroup
listPrimitiveTests = TestGroup "hydra.lib.lists primitives" Nothing groups []
  where
    groups = [
      listsApply,
      listsAt,
      listsBind,
      listsConcat,
      listsConcat2,
      listsCons,
      listsDrop,
--      listsDropWhile, -- TODO
      listsElem,
--      listsFilter, -- TODO
--      listsFoldl, -- TODO
      listsGroup,
      listsHead,
      listsInit,
      listsIntercalate,
      listsIntersperse,
      listsLast,
      listsLength,
      listsMap,
      listsNub,
      listsNull,
      listsPure,
      listsReplicate,
      listsReverse,
      listsSafeHead,
      listsSingleton,
      listsSort,
--      listsSortOn, -- TODO
--      listsSpan, -- TODO
      listsTail,
      listsTake,
      listsTranspose,
      listsZip]
--      listsZipWith] -- TODO

listsApply :: TestGroup
listsApply = TestGroup "apply" Nothing [] [
    testStr "string transformations" [primitive _strings_toUpper, primitive _strings_toLower] ["One", "Two", "Three"] ["ONE", "TWO", "THREE", "one", "two", "three"],
    testStr "empty function list" [] ["a", "b"] [],
    testStr "empty input list" [primitive _strings_toUpper] [] [],
    testStr "single function" [primitive _strings_toUpper] ["hello"] ["HELLO"],
    testStr "single input" [primitive _strings_toUpper, primitive _strings_toLower] ["Test"] ["TEST", "test"]]
  where
    testStr name funs lst result = primCase name _lists_apply [list funs, stringList lst] (stringList result)

listsAt :: TestGroup
listsAt = TestGroup "at" Nothing [] [
    testInt "first element" 0 [1, 2, 3] 1,
    testInt "middle element" 1 [1, 2, 3] 2,
    testInt "last element" 2 [1, 2, 3] 3,
    testInt "single element list" 0 [42] 42,
    testStr "string list access" 1 ["hello", "world"] "world"]
  where
    testInt name idx lst result = primCase name _lists_at [int32 idx, intList lst] (int32 result)
    testStr name idx lst result = primCase name _lists_at [int32 idx, stringList lst] (string result)

listsBind :: TestGroup
listsBind = TestGroup "bind" Nothing [] [
    test "negation function" [1, 2, 3, 4] (primitive _lists_pure <.> primitive _math_negate) (negate <$> [1, 2, 3, 4]),
    test "empty list" [] (primitive _lists_pure <.> primitive _math_negate) [],
    test "single element" [5] (primitive _lists_pure <.> primitive _math_negate) [-5],
    test "duplicate elements" [1, 1, 2] (primitive _lists_pure <.> primitive _math_negate) [-1, -1, -2]]
  where
    test name lst fun result = primCase name _lists_bind [intList lst, fun] (intList result)

listsConcat :: TestGroup
listsConcat = TestGroup "concat" Nothing [] [
    test "multiple non-empty lists" [[1, 2, 3], [4, 5], [6, 7, 8]] [1, 2, 3, 4, 5, 6, 7, 8],
    test "empty lists included" [[], [1, 2], [], [3]] [1, 2, 3],
    test "single list" [[1, 2, 3]] [1, 2, 3],
    test "all empty lists" [[], [], []] [],
    test "empty list of lists" [] []]
  where
    test name lists result = primCase name _lists_concat [intListList lists] (intList result)

listsConcat2 :: TestGroup
listsConcat2 = TestGroup "concat2" Nothing [] [
    testInt "two non-empty lists" [1, 2] [3, 4] [1, 2, 3, 4],
    testInt "first list empty" [] [1, 2] [1, 2],
    testInt "second list empty" [1, 2] [] [1, 2],
    testInt "both lists empty" [] [] [],
    testInt "single elements" [1] [2] [1, 2],
    testStr "string lists" ["a", "b"] ["c", "d"] ["a", "b", "c", "d"]]
  where
    testInt name lst1 lst2 result = primCase name _lists_concat2 [intList lst1, intList lst2] (intList result)
    testStr name lst1 lst2 result = primCase name _lists_concat2 [stringList lst1, stringList lst2] (stringList result)

listsCons :: TestGroup
listsCons = TestGroup "cons" Nothing [] [
    testInt "cons to non-empty list" 1 [2, 3] [1, 2, 3],
    testInt "cons to empty list" 1 [] [1],
    testInt "cons negative number" (-1) [2, 3] [-1, 2, 3],
    testStr "cons string" "hello" ["world"] ["hello", "world"]]
  where
    testInt name x lst result = primCase name _lists_cons [int32 x, intList lst] (intList result)
    testStr name x lst result = primCase name _lists_cons [string x, stringList lst] (stringList result)

listsDrop :: TestGroup
listsDrop = TestGroup "drop" Nothing [] [
    test "drop from beginning" 2 [1, 2, 3, 4, 5] [3, 4, 5],
    test "drop zero elements" 0 [1, 2, 3] [1, 2, 3],
    test "drop all elements" 3 [1, 2, 3] [],
    test "drop more than length" 5 [1, 2] [],
    test "drop from empty list" 3 [] [],
    test "drop negative amount" (-1) [1, 2, 3] [1, 2, 3]]
  where
    test name n lst result = primCase name _lists_drop [int32 n, intList lst] (intList result)

listsDropWhile :: TestGroup
listsDropWhile = TestGroup "dropWhile" Nothing [] [
    test "drop while less than 3" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 3)) [1, 2, 3, 2, 1] [3, 2, 1],
    test "drop all elements" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 10)) [1, 2, 3] [],
    test "drop no elements" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 0)) [1, 2, 3] [1, 2, 3],
    test "empty list" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 5)) [] []]
  where
    test name pred lst result = primCase name _lists_dropWhile [pred, intList lst] (intList result)

listsElem :: TestGroup
listsElem = TestGroup "elem" Nothing [] [
    testInt "element present" 2 [1, 2, 3] True,
    testInt "element not present" 4 [1, 2, 3] False,
    testInt "empty list" 1 [] False,
    testInt "single element present" 1 [1] True,
    testInt "single element not present" 2 [1] False,
    testInt "duplicate elements" 2 [1, 2, 2, 3] True,
    testStr "string element present" "hello" ["world", "hello", "test"] True,
    testStr "string element not present" "missing" ["world", "hello"] False]
  where
    testInt name x lst result = primCase name _lists_elem [int32 x, intList lst] (boolean result)
    testStr name x lst result = primCase name _lists_elem [string x, stringList lst] (boolean result)

listsFilter :: TestGroup
listsFilter = TestGroup "filter" Nothing [] [
    test "filter positive numbers" (lambda "x" (primitive _equality_gt <.> var "x" <.> int32 0)) [-1, 2, -3, 4, 5] [2, 4, 5],
    test "filter all elements" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 10)) [1, 2, 3] [1, 2, 3],
    test "filter no elements" (lambda "x" (primitive _equality_gt <.> var "x" <.> int32 10)) [1, 2, 3] [],
    test "empty list" (lambda "x" (primitive _equality_gt <.> var "x" <.> int32 0)) [] []]
  where
    test name pred lst result = primCase name _lists_filter [pred, intList lst] (intList result)

listsFoldl :: TestGroup
listsFoldl = TestGroup "foldl" Nothing [] [
    test "sum with addition" (primitive _math_add) 0 [1, 2, 3, 4] 10,
    test "product with multiplication" (primitive _math_mul) 1 [2, 3, 4] 24,
    test "empty list" (primitive _math_add) 5 [] 5,
    test "single element" (primitive _math_add) 10 [5] 15,
    test "subtraction fold" (primitive _math_sub) 10 [1, 2, 3] 4]
  where
    test name op acc lst result = primCase name _lists_foldl [op, int32 acc, intList lst] (int32 result)

listsGroup :: TestGroup
listsGroup = TestGroup "group" Nothing [] [
    test "consecutive duplicates" [1, 1, 2, 2, 2, 3, 1] [[1, 1], [2, 2, 2], [3], [1]],
    test "no duplicates" [1, 2, 3] [[1], [2], [3]],
    test "all same" [1, 1, 1] [[1, 1, 1]],
    test "empty list" [] [],
    test "single element" [1] [[1]]]
  where
    test name lst result = primCase name _lists_group [intList lst] (intListList result)

listsHead :: TestGroup
listsHead = TestGroup "head" Nothing [] [
    testInt "three element list" [1, 2, 3] 1,
    testInt "single element list" [42] 42,
    testInt "negative numbers" [-1, -2, -3] (-1),
    testStr "string list" ["hello", "world"] "hello"]
  where
    testInt name lst result = primCase name _lists_head [intList lst] (int32 result)
    testStr name lst result = primCase name _lists_head [stringList lst] (string result)

listsInit :: TestGroup
listsInit = TestGroup "init" Nothing [] [
    testInt "multiple elements" [1, 2, 3, 4] [1, 2, 3],
    testInt "two elements" [1, 2] [1],
    testInt "single element" [1] [],
    testStr "string list" ["a", "b", "c"] ["a", "b"]]
  where
    testInt name lst result = primCase name _lists_init [intList lst] (intList result)
    testStr name lst result = primCase name _lists_init [stringList lst] (stringList result)

listsIntercalate :: TestGroup
listsIntercalate = TestGroup "intercalate" Nothing [] [
    test "double zero separator" [0, 0] [[1, 2, 3], [4, 5], [6, 7, 8]] [1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8],
    test "empty separator" [] [[1, 2], [3, 4]] [1, 2, 3, 4],
    test "single element separator" [99] [[1], [2], [3]] [1, 99, 2, 99, 3],
    test "empty list of lists" [0] [] [],
    test "single list" [0] [[1, 2, 3]] [1, 2, 3],
    test "lists with empty lists" [0] [[], [1], []] [0, 1, 0]]
  where
    test name ifx lists result = primCase name _lists_intercalate [intList ifx, intListList lists] (intList result)

listsIntersperse :: TestGroup
listsIntersperse = TestGroup "intersperse" Nothing [] [
    testStr "string interspersion" "and" ["one", "two", "three"] ["one", "and", "two", "and", "three"],
    testStr "single element" "x" ["only"] ["only"],
    testStr "empty list" "x" [] [],
    testStr "two elements" "+" ["a", "b"] ["a", "+", "b"],
    testInt "number interspersion" 0 [1, 2, 3] [1, 0, 2, 0, 3]]
  where
    testStr name ifx lst result = primCase name _lists_intersperse [string ifx, stringList lst] (stringList result)
    testInt name ifx lst result = primCase name _lists_intersperse [int32 ifx, intList lst] (intList result)

listsLast :: TestGroup
listsLast = TestGroup "last" Nothing [] [
    testInt "three element list" [1, 2, 3] 3,
    testInt "single element list" [42] 42,
    testInt "negative numbers" [-1, -2, -3] (-3),
    testStr "string list" ["hello", "world"] "world"]
  where
    testInt name lst result = primCase name _lists_last [intList lst] (int32 result)
    testStr name lst result = primCase name _lists_last [stringList lst] (string result)

listsLength :: TestGroup
listsLength = TestGroup "length" Nothing [] [
    testInt "three elements" [1, 2, 3] 3,
    testInt "empty list" [] 0,
    testInt "single element" [42] 1,
    testInt "many elements" [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 10,
    testStr "string list" ["a", "b", "c"] 3]
  where
    testInt name lst result = primCase name _lists_length [intList lst] (int32 result)
    testStr name lst result = primCase name _lists_length [stringList lst] (int32 result)

listsMap :: TestGroup
listsMap = TestGroup "map" Nothing [] [
    testStr "string to uppercase" (primitive _strings_toUpper) ["one", "two"] ["ONE", "TWO"],
    testStr "empty list" (primitive _strings_toUpper) [] [],
    testStr "single element" (primitive _strings_toUpper) ["hello"] ["HELLO"],
    testInt "number negation" (primitive _math_negate) [1, 2, 3] [-1, -2, -3],
    testInt "identity function" (primitive _equality_identity) [1, 2, 3] [1, 2, 3]]
  where
    testStr name fun lst result = primCase name _lists_map [fun, stringList lst] (stringList result)
    testInt name fun lst result = primCase name _lists_map [fun, intList lst] (intList result)

listsNub :: TestGroup
listsNub = TestGroup "nub" Nothing [] [
    testInt "remove duplicates" [1, 2, 1, 3, 2, 4] [1, 2, 3, 4],
    testInt "no duplicates" [1, 2, 3] [1, 2, 3],
    testInt "all duplicates" [1, 1, 1] [1],
    testInt "empty list" [] [],
    testInt "single element" [1] [1],
    testStr "string duplicates" ["a", "b", "a", "c"] ["a", "b", "c"]]
  where
    testInt name lst result = primCase name _lists_nub [intList lst] (intList result)
    testStr name lst result = primCase name _lists_nub [stringList lst] (stringList result)

listsNull :: TestGroup
listsNull = TestGroup "null" Nothing [] [
    testInt "empty int list" [] True,
    testInt "single element" [1] False,
    testInt "multiple elements" [1, 2, 3] False,
    testStr "empty string list" [] True,
    testStr "non-empty string list" ["a"] False]
  where
    testInt name lst result = primCase name _lists_null [intList lst] (boolean result)
    testStr name lst result = primCase name _lists_null [stringList lst] (boolean result)

listsPure :: TestGroup
listsPure = TestGroup "pure" Nothing [] [
    testStr "string element" "one" ["one"],
    testStr "empty string" "" [""],
    testInt "number element" 42 [42],
    testInt "negative number" (-5) [-5]]
  where
    testStr name arg result = primCase name _lists_pure [string arg] (stringList result)
    testInt name arg result = primCase name _lists_pure [int32 arg] (intList result)

listsReplicate :: TestGroup
listsReplicate = TestGroup "replicate" Nothing [] [
    testInt "replicate three times" 3 42 [42, 42, 42],
    testInt "replicate zero times" 0 1 [],
    testInt "replicate once" 1 99 [99],
    testStr "replicate string" 2 "hello" ["hello", "hello"]]
  where
    testInt name n x result = primCase name _lists_replicate [int32 n, int32 x] (intList result)
    testStr name n x result = primCase name _lists_replicate [int32 n, string x] (stringList result)

listsReverse :: TestGroup
listsReverse = TestGroup "reverse" Nothing [] [
    testInt "multiple elements" [1, 2, 3, 4] [4, 3, 2, 1],
    testInt "single element" [1] [1],
    testInt "empty list" [] [],
    testInt "two elements" [1, 2] [2, 1],
    testStr "string list" ["a", "b", "c"] ["c", "b", "a"]]
  where
    testInt name lst result = primCase name _lists_reverse [intList lst] (intList result)
    testStr name lst result = primCase name _lists_reverse [stringList lst] (stringList result)

listsSafeHead :: TestGroup
listsSafeHead = TestGroup "safeHead" Nothing [] [
    testInt "non-empty int list" [1, 2, 3] (Just 1),
    testInt "empty int list" [] Nothing,
    testInt "single element" [42] (Just 42),
    testStr "non-empty string list" ["hello", "world"] (Just "hello"),
    testStr "empty string list" [] Nothing]
  where
    testInt name lst result = primCase name _lists_safeHead [intList lst] (optionalInt32 result)
    testStr name lst result = primCase name _lists_safeHead [stringList lst] (optionalString result)
    optionalInt32 Nothing = optional Nothing
    optionalInt32 (Just x) = optional (Just (int32 x))
    optionalString Nothing = optional Nothing
    optionalString (Just x) = optional (Just (string x))

listsSingleton :: TestGroup
listsSingleton = TestGroup "singleton" Nothing [] [
    testInt "number element" 42 [42],
    testInt "negative number" (-1) [-1],
    testInt "zero" 0 [0],
    testStr "string element" "hello" ["hello"]]
  where
    testInt name x result = primCase name _lists_singleton [int32 x] (intList result)
    testStr name x result = primCase name _lists_singleton [string x] (stringList result)

listsSort :: TestGroup
listsSort = TestGroup "sort" Nothing [] [
    testInt "unsorted numbers" [3, 1, 4, 1, 5] [1, 1, 3, 4, 5],
    testInt "already sorted" [1, 2, 3] [1, 2, 3],
    testInt "reverse sorted" [3, 2, 1] [1, 2, 3],
    testInt "single element" [1] [1],
    testInt "empty list" [] [],
    testInt "duplicates" [2, 1, 2, 3, 1] [1, 1, 2, 2, 3],
    testStr "string sort" ["zebra", "apple", "banana"] ["apple", "banana", "zebra"]]
  where
    testInt name lst result = primCase name _lists_sort [intList lst] (intList result)
    testStr name lst result = primCase name _lists_sort [stringList lst] (stringList result)

listsSortOn :: TestGroup
listsSortOn = TestGroup "sortOn" Nothing [] [
   testStr "sort by string length" (primitive _strings_length) ["hello", "hi", "world"] ["hi", "hello", "world"],
   testStr "empty string list" (primitive _strings_length) [] [],
   testStr "single string element" (primitive _strings_length) ["test"] ["test"],
   testInt "sort by negation" (primitive _math_negate) [1, 3, 2] [3, 2, 1],
   testInt "sort by absolute value" (primitive _math_negate) [-1, -3, 2] [-1, 2, -3]]
 where
   testStr name keyFn lst result = primCase name _lists_sortOn [keyFn, stringList lst] (stringList result)
   testInt name keyFn lst result = primCase name _lists_sortOn [keyFn, intList lst] (intList result)

listsSpan :: TestGroup
listsSpan = TestGroup "span" Nothing [] [
    test "span less than 3" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 3)) [1, 2, 3, 1, 2] ([1, 2], [3, 1, 2]),
    test "span all elements" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 10)) [1, 2, 3] ([1, 2, 3], []),
    test "span no elements" (lambda "x" (primitive _equality_gt <.> var "x" <.> int32 10)) [1, 2, 3] ([], [1, 2, 3]),
    test "empty list" (lambda "x" (primitive _equality_lt <.> var "x" <.> int32 5)) [] ([], [])]
  where
    test name pred lst (prefix, suffix) = primCase name _lists_span [pred, intList lst] (tuple2 (intList prefix) (intList suffix))

listsTail :: TestGroup
listsTail = TestGroup "tail" Nothing [] [
    testInt "multiple elements" [1, 2, 3, 4] [2, 3, 4],
    testInt "two elements" [1, 2] [2],
    testInt "single element" [1] [],
    testStr "string list" ["a", "b", "c"] ["b", "c"]]
  where
    testInt name lst result = primCase name _lists_tail [intList lst] (intList result)
    testStr name lst result = primCase name _lists_tail [stringList lst] (stringList result)

listsTake :: TestGroup
listsTake = TestGroup "take" Nothing [] [
    test "take from beginning" 2 [1, 2, 3, 4, 5] [1, 2],
    test "take zero elements" 0 [1, 2, 3] [],
    test "take all elements" 3 [1, 2, 3] [1, 2, 3],
    test "take more than length" 5 [1, 2] [1, 2],
    test "take from empty list" 3 [] [],
    test "take negative amount" (-1) [1, 2, 3] []]
  where
    test name n lst result = primCase name _lists_take [int32 n, intList lst] (intList result)

listsTranspose :: TestGroup
listsTranspose = TestGroup "transpose" Nothing [] [
    test "square matrix" [[1, 2, 3], [4, 5, 6]] [[1, 4], [2, 5], [3, 6]],
    test "empty lists" [] [],
    test "single row" [[1, 2, 3]] [[1], [2], [3]],
    test "single column" [[1], [2], [3]] [[1, 2, 3]],
    test "ragged matrix" [[1, 2], [3], [4, 5, 6]] [[1, 3, 4], [2, 5], [6]]]
  where
    test name matrix result = primCase name _lists_transpose [intListList matrix] (intListList result)

listsZip :: TestGroup
listsZip = TestGroup "zip" Nothing [] [
    test "equal length lists" [1, 2, 3] ["a", "b", "c"] [(1, "a"), (2, "b"), (3, "c")],
    test "first list shorter" [1, 2] ["a", "b", "c"] [(1, "a"), (2, "b")],
    test "second list shorter" [1, 2, 3] ["a", "b"] [(1, "a"), (2, "b")],
    test "empty first list" [] ["a", "b"] [],
    test "empty second list" [1, 2] [] [],
    test "both empty lists" [] [] []]
  where
    test name lst1 lst2 result = primCase name _lists_zip [intList lst1, stringList lst2] (list ((\(x, y) -> tuple2 (int32 x) (string y)) <$> result))

listsZipWith :: TestGroup
listsZipWith = TestGroup "zipWith" Nothing [] [
    testInt "addition" (primitive _math_add) [1, 2, 3] [4, 5, 6] [5, 7, 9],
    testInt "first list shorter" (primitive _math_add) [1, 2] [4, 5, 6] [5, 7],
    testInt "second list shorter" (primitive _math_add) [1, 2, 3] [4, 5] [5, 7],
    testInt "empty first list" (primitive _math_add) [] [1, 2, 3] [],
    testInt "empty second list" (primitive _math_add) [1, 2, 3] [] [],
    testStr "string concatenation" (primitive _strings_cat2) ["a", "b"] ["1", "2"] ["a1", "b1"]]
  where
    testInt name op lst1 lst2 result = primCase name _lists_zipWith [op, intList lst1, intList lst2] (intList result)
    testStr name op lst1 lst2 result = primCase name _lists_zipWith [op, stringList lst1, stringList lst2] (stringList result)
