module Hydra.Sources.Test.Lib.Lists (listPrimitiveTests) where

import Hydra.Dsl.Tests
import Hydra.Dsl.Terms


listPrimitiveTests :: TestGroup
listPrimitiveTests = TestGroup "hydra.lib.lists primitives" Nothing groups []
  where
    groups = [
      listsApply,
      listsBind,
      listsConcat,
      listsHead,
      listsIntercalate,
      listsIntersperse,
      listsLast,
      listsLength,
      listsMap,
      listsPure]

listsApply :: TestGroup
listsApply = TestGroup "apply" Nothing [] [
    test "1" [primitive _strings_toUpper, primitive _strings_toLower] ["One", "Two", "Three"] ["ONE", "TWO", "THREE", "one", "two", "three"]]
  where
    test name funs lst result = primCase name _lists_apply [list funs, stringList lst] (stringList result)

listsBind :: TestGroup
listsBind = TestGroup "bind" Nothing [] [
    test "1" [1, 2, 3, 4] (primitive _lists_pure <.> primitive _math_neg) (negate <$> [1, 2, 3, 4])]
  where
    test name lst fun result = primCase name _lists_bind [intList lst, fun] (intList result)

listsConcat :: TestGroup
listsConcat = TestGroup "concat" Nothing [] [
    test "1" [[1, 2, 3], [4, 5], [6, 7, 8]] [1, 2, 3, 4, 5, 6, 7, 8]]
  where
    test name lists result = primCase name _lists_concat [intListList lists] (intList result)

listsHead :: TestGroup
listsHead = TestGroup "head" Nothing [] [
    test "1" [1, 2, 3] 1]
  where
    test name lst result = primCase name _lists_head [intList lst] (int32 result)

listsIntercalate :: TestGroup
listsIntercalate = TestGroup "intercalate" Nothing [] [
    test "1" [0, 0] [[1, 2, 3], [4, 5], [6, 7, 8]] [1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8]]
  where
    test name ifx lists result = primCase name _lists_intercalate [intList ifx, intListList lists] (intList result)

listsIntersperse :: TestGroup
listsIntersperse = TestGroup "intersperse" Nothing [] [
    test "1" "and" ["one", "two", "three"] ["one", "and", "two", "and", "three"]]
  where
    test name ifx lst result = primCase name _lists_intersperse [string ifx, stringList lst] (stringList result)

listsLast :: TestGroup
listsLast = TestGroup "last" Nothing [] [
    test "1" [1, 2, 3] 3]
  where
    test name lst result = primCase name _lists_last [intList lst] (int32 result)

listsLength :: TestGroup
listsLength = TestGroup "length" Nothing [] [
    test "1" [1, 2, 3] 3]
  where
    test name lst result = primCase name _lists_length [intList lst] (int32 result)

listsMap :: TestGroup
listsMap = TestGroup "map" Nothing [] [
    test "1" (primitive _strings_toUpper) ["one", "two"] ["ONE", "TWO"]]
  where
    test name fun lst result = primCase name _lists_map [fun, stringList lst] (stringList result)

listsPure :: TestGroup
listsPure = TestGroup "pure" Nothing [] [
    test "1" "one"]
  where
    test name arg = primCase name _lists_pure [string arg] (stringList [arg])
