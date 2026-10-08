module Hydra.Sources.Test.Lib.Lists where

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
import qualified Hydra.Core.Lib.Equality as DefEquality
import qualified Hydra.Core.Lib.Functions as DefFunctions
import qualified Hydra.Core.Lib.Ordering as DefOrdering
import qualified Hydra.Core.Lib.Lists as DefLists
import qualified Hydra.Core.Lib.Math as DefMath
import qualified Hydra.Core.Lib.Strings as DefStrings


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.lists"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestGraph.ns, ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for hydra.core.lib.lists primitives"))}
  where
    definitions = [
      Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.lists primitives" $
    supergroup "hydra.core.lib.lists primitives" [
      listsApply,
      listsBind,
      listsConcat,
      listsConcat2,
      listsCons,
      listsDrop,
      listsDropWhile,
      listsElem,
      listsFilter,
      listsFind,
      listsFoldl,
      listsFoldr,
      listsGroup,
      listsIntercalate,
      listsIntersperse,
      listsLength,
      listsMap,
      listsMaybeAt,
      listsMaybeHead,
      listsMaybeInit,
      listsMaybeLast,
      listsMaybeTail,
      listsNub,
      listsIsEmpty,
      listsPartition,
      listsReplicate,
      listsReverse,
      listsSingleton,
      listsSort,
      listsSortOn,
      listsSpan,
      listsTake,
      listsTranspose,
      listsUncons,
      listsZip,
      listsZipWith]
    where
      listsApply = supergroup "apply" [
        subgroup "sequentialComposition" [
          testStr "string transformations" "sequentialComposition" [primitive DefStrings.toUpper, primitive DefStrings.toLower] ["One", "Two", "Three"] ["ONE", "TWO", "THREE", "one", "two", "three"]],
        subgroup "identityCases" [
          testStr "empty input list" "identityCases" [primitive DefStrings.toUpper] [] [],
          testStr "single function" "identityCases" [primitive DefStrings.toUpper] ["hello"] ["HELLO"],
          testStr "single input" "identityCases" [primitive DefStrings.toUpper, primitive DefStrings.toLower] ["Test"] ["TEST", "test"]]]
        where
          testStr name segment funs lst result = primCaseWithProvisions name [segment] DefLists.apply [list funs, stringList lst] (stringList result)

      listsBind = supergroup "bind" [
        subgroup "flatMapsOverElements" [
          test "negation function" "flatMapsOverElements" [1, 2, 3, 4] (lambda "x" (primitive DefLists.singleton @@ (primitive DefMath.negate @@ var "x"))) (negate <$> [1, 2, 3, 4]),
          test "single element" "flatMapsOverElements" [5] (lambda "x" (primitive DefLists.singleton @@ (primitive DefMath.negate @@ var "x"))) [-5],
          test "duplicate elements" "flatMapsOverElements" [1, 1, 2] (lambda "x" (primitive DefLists.singleton @@ (primitive DefMath.negate @@ var "x"))) [-1, -1, -2]],
        subgroup "emptyInput" [
          test "empty list" "emptyInput" [] (lambda "x" (primitive DefLists.singleton @@ (primitive DefMath.negate @@ var "x"))) []]]
        where
          test name segment lst fun result = primCaseWithProvisions name [segment] DefLists.bind [intList lst, fun] (intList result)

      listsConcat = supergroup "concat" [
        subgroup "flattensPreservingOrder" [
          test "multiple non-empty lists" "flattensPreservingOrder" [[1, 2, 3], [4, 5], [6, 7, 8]] [1, 2, 3, 4, 5, 6, 7, 8],
          test "empty lists included" "flattensPreservingOrder" [[], [1, 2], [], [3]] [1, 2, 3],
          test "single list" "flattensPreservingOrder" [[1, 2, 3]] [1, 2, 3]],
        subgroup "emptyInputs" [
          test "all empty lists" "emptyInputs" [[], [], []] [],
          test "empty list of lists" "emptyInputs" [] []]]
        where
          test name segment lists result = primCaseWithProvisions name [segment] DefLists.concat [intListList lists] (intList result)
  
      listsConcat2 = supergroup "concat2" [
        subgroup "appendsPreservingOrder" [
          testInt "two non-empty lists" "appendsPreservingOrder" [1, 2] [3, 4] [1, 2, 3, 4],
          testInt "single elements" "appendsPreservingOrder" [1] [2] [1, 2],
          testStr "string lists" "appendsPreservingOrder" ["a", "b"] ["c", "d"] ["a", "b", "c", "d"]],
        subgroup "identityOnEmpty" [
          testInt "first list empty" "identityOnEmpty" [] [1, 2] [1, 2],
          testInt "second list empty" "identityOnEmpty" [1, 2] [] [1, 2],
          testInt "both lists empty" "identityOnEmpty" [] [] []]]
        where
          testInt name segment lst1 lst2 result = primCaseWithProvisions name [segment] DefLists.concat2 [intList lst1, intList lst2] (intList result)
          testStr name segment lst1 lst2 result = primCaseWithProvisions name [segment] DefLists.concat2 [stringList lst1, stringList lst2] (stringList result)

      listsCons = subgroup "prependsElement" [
        testInt "cons to non-empty list" 1 [2, 3] [1, 2, 3],
        testInt "cons to empty list" 1 [] [1],
        testInt "cons negative number" (-1) [2, 3] [-1, 2, 3],
        testStr "cons string" "hello" ["world"] ["hello", "world"]]
        where
          testInt name x lst result = primCaseWithProvisions name ["prependsElement"] DefLists.cons [int32 x, intList lst] (intList result)
          testStr name x lst result = primCaseWithProvisions name ["prependsElement"] DefLists.cons [string x, stringList lst] (stringList result)

      listsDrop = supergroup "drop" [
        subgroup "dropsFromFront" [
          test "drop from beginning" "dropsFromFront" 2 [1, 2, 3, 4, 5] [3, 4, 5],
          test "drop all elements" "dropsFromFront" 3 [1, 2, 3] [],
          test "drop more than length" "dropsFromFront" 5 [1, 2] [],
          test "drop from empty list" "dropsFromFront" 3 [] []],
        subgroup "nonPositiveIsIdentity" [
          test "drop zero elements" "nonPositiveIsIdentity" 0 [1, 2, 3] [1, 2, 3],
          test "drop negative amount" "nonPositiveIsIdentity" (-1) [1, 2, 3] [1, 2, 3]]]
        where
          test name segment n lst result = primCaseWithProvisions name [segment] DefLists.drop [int32 n, intList lst] (intList result)

      listsDropWhile = subgroup "dropsLeadingMatches" [
        test "drop while less than 3" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 3)) [1, 2, 3, 2, 1] [3, 2, 1],
        test "drop all elements" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 10)) [1, 2, 3] [],
        test "drop no elements" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 0)) [1, 2, 3] [1, 2, 3],
        test "empty list" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 5)) [] []]
        where
          test name pred lst result = primCaseWithProvisions name ["dropsLeadingMatches"] DefLists.dropWhile [pred, intList lst] (intList result)

      listsElem = supergroup "elem" [
        subgroup "testsSetMembership" [
          testInt "element present" "testsSetMembership" 2 [1, 2, 3] True,
          testInt "single element present" "testsSetMembership" 1 [1] True,
          testInt "duplicate elements" "testsSetMembership" 2 [1, 2, 2, 3] True,
          testStr "string element present" "testsSetMembership" "hello" ["world", "hello", "test"] True],
        subgroup "falseWhenAbsentOrEmpty" [
          testInt "element not present" "falseWhenAbsentOrEmpty" 4 [1, 2, 3] False,
          testInt "empty list" "falseWhenAbsentOrEmpty" 1 [] False,
          testInt "single element not present" "falseWhenAbsentOrEmpty" 2 [1] False,
          testStr "string element not present" "falseWhenAbsentOrEmpty" "missing" ["world", "hello"] False]]
        where
          testInt name segment x lst result = primCaseWithProvisions name [segment] DefLists.member [int32 x, intList lst] (boolean result)
          testStr name segment x lst result = primCaseWithProvisions name [segment] DefLists.member [string x, stringList lst] (boolean result)

      listsFilter = supergroup "filter" [
        subgroup "keepsMatchingElements" [
          test "filter positive numbers" "keepsMatchingElements" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 0)) [-1, 2, -3, 4, 5] [2, 4, 5],
          test "filter all elements" "keepsMatchingElements" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 10)) [1, 2, 3] [1, 2, 3],
          test "filter no elements" "keepsMatchingElements" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 10)) [1, 2, 3] []],
        subgroup "emptyInput" [
          test "empty list" "emptyInput" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 0)) [] []]]
        where
          test name segment pred lst result = primCaseWithProvisions name [segment] DefLists.filter [pred, intList lst] (intList result)

      listsFind = supergroup "find" [
        subgroup "returnsFirstMatch" [
          test "find existing element" "returnsFirstMatch" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 3)) [1, 2, 4, 5] (Just 4),
          test "find first matching" "returnsFirstMatch" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 0)) [1, 2, 3] (Just 1),
          test "find single element" "returnsFirstMatch" (lambda "x" (primitive DefEquality.equal @@ var "x" @@ int32 42)) [42] (Just 42)],
        subgroup "noneWhenNoMatchOrEmpty" [
          test "find no match" "noneWhenNoMatchOrEmpty" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 10)) [1, 2, 3] Nothing,
          test "find in empty list" "noneWhenNoMatchOrEmpty" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 0)) [] Nothing]]
        where
          test name segment pred lst result = primCaseWithProvisions name [segment] DefLists.find [pred, intList lst] (optionalInt32 result)

      listsFoldl = supergroup "foldl" [
        subgroup "appliesLeftAssociatively" [
          test "sum with addition" "appliesLeftAssociatively" (primitive DefMath.add) 0 [1, 2, 3, 4] 10,
          test "product with multiplication" "appliesLeftAssociatively" (primitive DefMath.mul) 1 [2, 3, 4] 24,
          test "single element" "appliesLeftAssociatively" (primitive DefMath.add) 10 [5] 15,
          test "subtraction fold" "appliesLeftAssociatively" (primitive DefMath.sub) 10 [1, 2, 3] 4],
        subgroup "emptyListReturnsAccumulator" [
          test "empty list" "emptyListReturnsAccumulator" (primitive DefMath.add) 5 [] 5]]
        where
          test name segment op acc lst result = primCaseWithProvisions name [segment] DefLists.foldl [op, int32 acc, intList lst] (int32 result)

      listsFoldr = supergroup "foldr" [
        subgroup "appliesRightAssociatively" [
          test "subtraction fold right" "appliesRightAssociatively" (primitive DefMath.sub) 0 [1, 2, 3] 2,
          test "single element" "appliesRightAssociatively" (primitive DefMath.add) 10 [5] 15,
          test "sum with addition" "appliesRightAssociatively" (primitive DefMath.add) 0 [1, 2, 3, 4] 10,
          test "subtraction vs foldl" "appliesRightAssociatively" (primitive DefMath.sub) 10 [1, 2, 3] (-8)],
        subgroup "emptyListReturnsAccumulator" [
          test "empty list" "emptyListReturnsAccumulator" (primitive DefMath.add) 5 [] 5]]
        where
          test name segment op acc lst result = primCaseWithProvisions name [segment] DefLists.foldr [op, int32 acc, intList lst] (int32 result)

      listsGroup = supergroup "group" [
        subgroup "groupsConsecutiveDuplicates" [
          test "consecutive duplicates" "groupsConsecutiveDuplicates" [1, 1, 2, 2, 2, 3, 1] [[1, 1], [2, 2, 2], [3], [1]],
          test "no duplicates" "groupsConsecutiveDuplicates" [1, 2, 3] [[1], [2], [3]],
          test "all same" "groupsConsecutiveDuplicates" [1, 1, 1] [[1, 1, 1]],
          test "single element" "groupsConsecutiveDuplicates" [1] [[1]]],
        subgroup "emptyInput" [
          test "empty list" "emptyInput" [] []]]
        where
          test name segment lst result = primCaseWithProvisions name [segment] DefLists.group [intList lst] (intListList result)

      listsIntercalate = supergroup "intercalate" [
        subgroup "insertsSeparatorBetweenLists" [
          test "double zero separator" "insertsSeparatorBetweenLists" [0, 0] [[1, 2, 3], [4, 5], [6, 7, 8]] [1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8],
          test "empty separator" "insertsSeparatorBetweenLists" [] [[1, 2], [3, 4]] [1, 2, 3, 4],
          test "single element separator" "insertsSeparatorBetweenLists" [99] [[1], [2], [3]] [1, 99, 2, 99, 3],
          test "single list" "insertsSeparatorBetweenLists" [0] [[1, 2, 3]] [1, 2, 3],
          test "lists with empty lists" "insertsSeparatorBetweenLists" [0] [[], [1], []] [0, 1, 0]],
        subgroup "emptyListOfLists" [
          test "empty list of lists" "emptyListOfLists" [0] [] []]]
        where
          test name segment ifx lists result = primCaseWithProvisions name [segment] DefLists.join [intList ifx, intListList lists] (intList result)
  
      listsIntersperse = supergroup "intersperse" [
        subgroup "insertsElementBetweenItems" [
          testStr "string interspersion" "insertsElementBetweenItems" "and" ["one", "two", "three"] ["one", "and", "two", "and", "three"],
          testStr "two elements" "insertsElementBetweenItems" "+" ["a", "b"] ["a", "+", "b"],
          testInt "number interspersion" "insertsElementBetweenItems" 0 [1, 2, 3] [1, 0, 2, 0, 3]],
        subgroup "noInsertionBelowTwoElements" [
          testStr "single element" "noInsertionBelowTwoElements" "x" ["only"] ["only"],
          testStr "empty list" "noInsertionBelowTwoElements" "x" [] []]]
        where
          testStr name segment ifx lst result = primCaseWithProvisions name [segment] DefLists.intersperse [string ifx, stringList lst] (stringList result)
          testInt name segment ifx lst result = primCaseWithProvisions name [segment] DefLists.intersperse [int32 ifx, intList lst] (intList result)

      listsLength = subgroup "countsElements" [
        testInt "three elements" [1, 2, 3] 3,
        testInt "empty list" [] 0,
        testInt "single element" [42] 1,
        testInt "many elements" [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 10,
        testStr "string list" ["a", "b", "c"] 3]
        where
          testInt name lst result = primCaseWithProvisions name ["countsElements"] DefLists.length [intList lst] (int32 result)
          testStr name lst result = primCaseWithProvisions name ["countsElements"] DefLists.length [stringList lst] (int32 result)

      listsMap = supergroup "map" [
        subgroup "appliesFunctionElementwise" [
          testStr "string to uppercase" "appliesFunctionElementwise" (primitive DefStrings.toUpper) ["one", "two"] ["ONE", "TWO"],
          testStr "single element" "appliesFunctionElementwise" (primitive DefStrings.toUpper) ["hello"] ["HELLO"],
          testInt "number negation" "appliesFunctionElementwise" (primitive DefMath.negate) [1, 2, 3] [-1, -2, -3]],
        subgroup "identityAndEmptyCases" [
          testStr "empty list" "identityAndEmptyCases" (primitive DefStrings.toUpper) [] [],
          testInt "identity function" "identityAndEmptyCases" (primitive DefFunctions.identity) [1, 2, 3] [1, 2, 3]]]
        where
          testStr name segment fun lst result = primCaseWithProvisions name [segment] DefLists.map [fun, stringList lst] (stringList result)
          testInt name segment fun lst result = primCaseWithProvisions name [segment] DefLists.map [fun, intList lst] (intList result)

      listsMaybeAt = supergroup "maybeAt" [
        subgroup "returnsElementAtIndex" [
          test "valid index" "returnsElementAtIndex" 1 [10, 20, 30] (Just 20),
          test "first element" "returnsElementAtIndex" 0 [10, 20, 30] (Just 10),
          test "last element" "returnsElementAtIndex" 2 [10, 20, 30] (Just 30)],
        subgroup "noneWhenOutOfRange" [
          test "out of bounds" "noneWhenOutOfRange" 5 [10, 20, 30] Nothing,
          test "negative index" "noneWhenOutOfRange" (-1) [10, 20, 30] Nothing,
          test "empty list" "noneWhenOutOfRange" 0 [] Nothing]]
        where
          test name segment i lst result = primCaseWithProvisions name [segment] DefLists.at [int32 i, intList lst] (optionalInt32 result)

      listsMaybeHead = supergroup "maybeHead" [
        subgroup "returnsFirstElement" [
          testInt "non-empty int list" "returnsFirstElement" [1, 2, 3] (Just 1),
          testInt "single element" "returnsFirstElement" [42] (Just 42),
          testStr "non-empty string list" "returnsFirstElement" ["hello", "world"] (Just "hello")],
        subgroup "noneWhenEmpty" [
          testInt "empty int list" "noneWhenEmpty" [] Nothing,
          testStr "empty string list" "noneWhenEmpty" [] Nothing]]
        where
          testInt name segment lst result = primCaseWithProvisions name [segment] DefLists.head [intList lst] (optionalInt32 result)
          testStr name segment lst result = primCaseWithProvisions name [segment] DefLists.head [stringList lst] (optionalString result)

      listsMaybeInit = supergroup "maybeInit" [
        subgroup "returnsAllButLast" [
          testInt "three elements" "returnsAllButLast" [1, 2, 3] (Just [1, 2]),
          testInt "single element" "returnsAllButLast" [42] (Just []),
          testStr "string list" "returnsAllButLast" ["a", "b", "c"] (Just ["a", "b"])],
        subgroup "noneWhenEmpty" [
          testInt "empty list" "noneWhenEmpty" [] Nothing]]
        where
          testInt name segment lst result = primCaseWithProvisions name [segment] DefLists.init [intList lst] (optionalIntList result)
          testStr name segment lst result = primCaseWithProvisions name [segment] DefLists.init [stringList lst] (optionalStringList result)

      listsMaybeLast = supergroup "maybeLast" [
        subgroup "returnsLastElement" [
          testInt "three elements" "returnsLastElement" [1, 2, 3] (Just 3),
          testInt "single element" "returnsLastElement" [42] (Just 42),
          testStr "string list" "returnsLastElement" ["hello", "world"] (Just "world")],
        subgroup "noneWhenEmpty" [
          testInt "empty list" "noneWhenEmpty" [] Nothing]]
        where
          testInt name segment lst result = primCaseWithProvisions name [segment] DefLists.last [intList lst] (optionalInt32 result)
          testStr name segment lst result = primCaseWithProvisions name [segment] DefLists.last [stringList lst] (optionalString result)

      listsMaybeTail = supergroup "maybeTail" [
        subgroup "returnsAllButFirst" [
          testInt "three elements" "returnsAllButFirst" [1, 2, 3] (Just [2, 3]),
          testInt "single element" "returnsAllButFirst" [42] (Just []),
          testStr "string list" "returnsAllButFirst" ["a", "b", "c"] (Just ["b", "c"])],
        subgroup "noneWhenEmpty" [
          testInt "empty list" "noneWhenEmpty" [] Nothing]]
        where
          testInt name segment lst result = primCaseWithProvisions name [segment] DefLists.tail [intList lst] (optionalIntList result)
          testStr name segment lst result = primCaseWithProvisions name [segment] DefLists.tail [stringList lst] (optionalStringList result)

      listsNub = supergroup "nub" [
        subgroup "removesDuplicatesPreservingFirstOccurrence" [
          testInt "remove duplicates" "removesDuplicatesPreservingFirstOccurrence" [1, 2, 1, 3, 2, 4] [1, 2, 3, 4],
          testInt "no duplicates" "removesDuplicatesPreservingFirstOccurrence" [1, 2, 3] [1, 2, 3],
          testInt "all duplicates" "removesDuplicatesPreservingFirstOccurrence" [1, 1, 1] [1],
          testInt "single element" "removesDuplicatesPreservingFirstOccurrence" [1] [1],
          testStr "string duplicates" "removesDuplicatesPreservingFirstOccurrence" ["a", "b", "a", "c"] ["a", "b", "c"]],
        subgroup "emptyInput" [
          testInt "empty list" "emptyInput" [] []]]
        where
          testInt name segment lst result = primCaseWithProvisions name [segment] DefLists.distinct [intList lst] (intList result)
          testStr name segment lst result = primCaseWithProvisions name [segment] DefLists.distinct [stringList lst] (stringList result)

      listsIsEmpty = subgroup "testsZeroLength" [
        testInt "empty int list" [] True,
        testInt "single element" [1] False,
        testInt "multiple elements" [1, 2, 3] False,
        testStr "empty string list" [] True,
        testStr "non-empty string list" ["a"] False]
        where
          testInt name lst result = primCaseWithProvisions name ["testsZeroLength"] DefLists.isEmpty [intList lst] (boolean result)
          testStr name lst result = primCaseWithProvisions name ["testsZeroLength"] DefLists.isEmpty [stringList lst] (boolean result)

      listsPartition = supergroup "partition" [
        subgroup "splitsBySatisfaction" [
          test "partition greater than 3" "splitsBySatisfaction" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 3)) [1, 2, 3, 4, 5, 6] ([4, 5, 6], [1, 2, 3]),
          test "partition even numbers" "splitsBySatisfaction" (lambda "x" (primitive DefMath.even @@ var "x")) [1, 2, 3, 4, 5, 6] ([2, 4, 6], [1, 3, 5])],
        subgroup "boundaryCases" [
          test "partition all elements" "boundaryCases" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 10)) [1, 2, 3] ([1, 2, 3], []),
          test "partition no elements" "boundaryCases" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 10)) [1, 2, 3] ([], [1, 2, 3]),
          test "empty list" "boundaryCases" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 5)) [] ([], [])]]
        where
          test name segment pred lst (yes, no) = primCaseWithProvisions name [segment] DefLists.partition [pred, intList lst] (pair (intList yes) (intList no))

      listsReplicate = subgroup "repeatsValueNTimes" [
        testInt "replicate three times" 3 42 [42, 42, 42],
        testInt "replicate zero times" 0 1 [],
        testInt "replicate once" 1 99 [99],
        testStr "replicate string" 2 "hello" ["hello", "hello"]]
        where
          testInt name n x result = primCaseWithProvisions name ["repeatsValueNTimes"] DefLists.replicate [int32 n, int32 x] (intList result)
          testStr name n x result = primCaseWithProvisions name ["repeatsValueNTimes"] DefLists.replicate [int32 n, string x] (stringList result)

      listsReverse = subgroup "reversesElementOrder" [
        testInt "multiple elements" [1, 2, 3, 4] [4, 3, 2, 1],
        testInt "single element" [1] [1],
        testInt "empty list" [] [],
        testInt "two elements" [1, 2] [2, 1],
        testStr "string list" ["a", "b", "c"] ["c", "b", "a"]]
        where
          testInt name lst result = primCaseWithProvisions name ["reversesElementOrder"] DefLists.reverse [intList lst] (intList result)
          testStr name lst result = primCaseWithProvisions name ["reversesElementOrder"] DefLists.reverse [stringList lst] (stringList result)
  
      listsSingleton = subgroup "wrapsOneElement" [
        testInt "number element" 42 [42],
        testInt "negative number" (-1) [-1],
        testInt "zero" 0 [0],
        testStr "string element" "hello" ["hello"]]
        where
          testInt name x result = primCaseWithProvisions name ["wrapsOneElement"] DefLists.singleton [int32 x] (intList result)
          testStr name x result = primCaseWithProvisions name ["wrapsOneElement"] DefLists.singleton [string x] (stringList result)

      listsSort = subgroup "ordersAscending" [
        testInt "unsorted numbers" [3, 1, 4, 1, 5] [1, 1, 3, 4, 5],
        testInt "already sorted" [1, 2, 3] [1, 2, 3],
        testInt "reverse sorted" [3, 2, 1] [1, 2, 3],
        testInt "single element" [1] [1],
        testInt "empty list" [] [],
        testInt "duplicates" [2, 1, 2, 3, 1] [1, 1, 2, 2, 3],
        testStr "string sort" ["zebra", "apple", "banana"] ["apple", "banana", "zebra"]]
        where
          testInt name lst result = primCaseWithProvisions name ["ordersAscending"] DefLists.sort [intList lst] (intList result)
          testStr name lst result = primCaseWithProvisions name ["ordersAscending"] DefLists.sort [stringList lst] (stringList result)

      listsSortOn = supergroup "sortOn" [
       subgroup "ordersByKeyFunction" [
         testStr "sort by string length" "ordersByKeyFunction" (primitive DefStrings.length) ["hello", "hi", "world"] ["hi", "hello", "world"],
         testInt "sort by negation" "ordersByKeyFunction" (primitive DefMath.negate) [1, 3, 2] [3, 2, 1],
         testInt "sort by absolute value" "ordersByKeyFunction" (primitive DefMath.abs) [-1, -3, 2] [-1, 2, -3]],
       subgroup "boundaryInputs" [
         testStr "empty string list" "boundaryInputs" (primitive DefStrings.length) [] [],
         testStr "single string element" "boundaryInputs" (primitive DefStrings.length) ["test"] ["test"]]]
       where
         testStr name segment keyFn lst result = primCaseWithProvisions name [segment] DefLists.sortBy [keyFn, stringList lst] (stringList result)
         testInt name segment keyFn lst result = primCaseWithProvisions name [segment] DefLists.sortBy [keyFn, intList lst] (intList result)

      listsSpan = subgroup "splitsAtFirstNonMatch" [
        test "span less than 3" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 3)) [1, 2, 3, 1, 2] ([1, 2], [3, 1, 2]),
        test "span all elements" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 10)) [1, 2, 3] ([1, 2, 3], []),
        test "span no elements" (lambda "x" (primitive DefOrdering.gt @@ var "x" @@ int32 10)) [1, 2, 3] ([], [1, 2, 3]),
        test "empty list" (lambda "x" (primitive DefOrdering.lt @@ var "x" @@ int32 5)) [] ([], [])]
        where
          test name pred lst (prefix, suffix) = primCaseWithProvisions name ["splitsAtFirstNonMatch"] DefLists.span [pred, intList lst] (pair (intList prefix) (intList suffix))

      listsTake = supergroup "take" [
        subgroup "takesFromFront" [
          test "take from beginning" "takesFromFront" 2 [1, 2, 3, 4, 5] [1, 2],
          test "take all elements" "takesFromFront" 3 [1, 2, 3] [1, 2, 3],
          test "take more than length" "takesFromFront" 5 [1, 2] [1, 2],
          test "take from empty list" "takesFromFront" 3 [] []],
        subgroup "nonPositiveIsEmpty" [
          test "take zero elements" "nonPositiveIsEmpty" 0 [1, 2, 3] [],
          test "take negative amount" "nonPositiveIsEmpty" (-1) [1, 2, 3] []]]
        where
          test name segment n lst result = primCaseWithProvisions name [segment] DefLists.take [int32 n, intList lst] (intList result)

      listsTranspose = subgroup "swapsRowsAndColumns" [
        test "square matrix" [[1, 2, 3], [4, 5, 6]] [[1, 4], [2, 5], [3, 6]],
        test "empty lists" [] [],
        test "single row" [[1, 2, 3]] [[1], [2], [3]],
        test "single column" [[1], [2], [3]] [[1, 2, 3]],
        test "ragged matrix" [[1, 2], [3], [4, 5, 6]] [[1, 3, 4], [2, 5], [6]]]
        where
          test name matrix result = primCaseWithProvisions name ["swapsRowsAndColumns"] DefLists.transpose [intListList matrix] (intListList result)

      listsUncons = supergroup "uncons" [
        subgroup "splitsHeadAndTail" [
          test "three elements" "splitsHeadAndTail" [1, 2, 3] (Just (1, [2, 3])),
          test "single element" "splitsHeadAndTail" [42] (Just (42, []))],
        subgroup "noneWhenEmpty" [
          test "empty list" "noneWhenEmpty" [] Nothing]]
        where
          test name segment lst result = primCaseWithProvisions name [segment] DefLists.uncons [intList lst] (optionalIntAndIntList result)

      listsZip = supergroup "zip" [
        subgroup "pairsElementwise" [
          test "equal length lists" "pairsElementwise" [1, 2, 3] ["a", "b", "c"] [(1, "a"), (2, "b"), (3, "c")],
          test "first list shorter" "pairsElementwise" [1, 2] ["a", "b", "c"] [(1, "a"), (2, "b")],
          test "second list shorter" "pairsElementwise" [1, 2, 3] ["a", "b"] [(1, "a"), (2, "b")]],
        subgroup "emptyWhenEitherEmpty" [
          test "empty first list" "emptyWhenEitherEmpty" [] ["a", "b"] [],
          test "empty second list" "emptyWhenEitherEmpty" [1, 2] [] [],
          test "both empty lists" "emptyWhenEitherEmpty" [] [] []]]
        where
          test name segment lst1 lst2 result = primCaseWithProvisions name [segment] DefLists.zip [intList lst1, stringList lst2] (list ((\(x, y) -> pair (int32 x) (string y)) <$> result))

      listsZipWith = supergroup "zipWith" [
        subgroup "combinesElementwiseByFunction" [
          testInt "addition" "combinesElementwiseByFunction" (primitive DefMath.add) [1, 2, 3] [4, 5, 6] [5, 7, 9],
          testInt "first list shorter" "combinesElementwiseByFunction" (primitive DefMath.add) [1, 2] [4, 5, 6] [5, 7],
          testInt "second list shorter" "combinesElementwiseByFunction" (primitive DefMath.add) [1, 2, 3] [4, 5] [5, 7],
          testStr "string concatenation" "combinesElementwiseByFunction" (primitive DefStrings.concat2) ["a", "b"] ["1", "2"] ["a1", "b2"]],
        subgroup "emptyWhenEitherEmpty" [
          testInt "empty first list" "emptyWhenEitherEmpty" (primitive DefMath.add) [] [1, 2, 3] [],
          testInt "empty second list" "emptyWhenEitherEmpty" (primitive DefMath.add) [1, 2, 3] [] []]]
        where
          testInt name segment op lst1 lst2 result = primCaseWithProvisions name [segment] DefLists.zipWith [op, intList lst1, intList lst2] (intList result)
          testStr name segment op lst1 lst2 result = primCaseWithProvisions name [segment] DefLists.zipWith [op, stringList lst1, stringList lst2] (stringList result)

-- Helper functions for building test terms
intList :: [Int] -> TypedTerm Term
intList els = list (int32 <$> els)

intListList :: [[Int]] -> TypedTerm Term
intListList lists = list (intList <$> lists)

optionalInt32 :: Maybe Int -> TypedTerm Term
optionalInt32 Nothing = Core.termOptional nothing
optionalInt32 (Just x) = Core.termOptional  $ just (int32 x)

optionalIntAndIntList :: Maybe (Int, [Int]) -> TypedTerm Term
optionalIntAndIntList Nothing = Core.termOptional nothing
optionalIntAndIntList (Just (x, xs)) = Core.termOptional $ just (pair (int32 x) (intList xs))

optionalIntList :: Maybe [Int] -> TypedTerm Term
optionalIntList Nothing = Core.termOptional nothing
optionalIntList (Just xs) = Core.termOptional $ just (intList xs)

optionalString :: Maybe String -> TypedTerm Term
optionalString Nothing = Core.termOptional nothing
optionalString (Just x) = Core.termOptional  $ just (string x)

optionalStringList :: Maybe [String] -> TypedTerm Term
optionalStringList Nothing = Core.termOptional nothing
optionalStringList (Just xs) = Core.termOptional $ just (stringList xs)

stringList :: [String] -> TypedTerm Term
stringList els = list (string <$> els)

-- A list of optional int32 values (for mapOptional results)
intOptionalList :: [Maybe Int] -> TypedTerm Term
intOptionalList ms = list (opt <$> ms)
  where
    opt Nothing = Core.termOptional nothing
    opt (Just x) = Core.termOptional $ just (int32 x)

-- A single set of int32 values (as a term)
intSet :: [Int] -> TypedTerm Term
intSet els = set (int32 <$> els)

-- A list of int32 sets (for mapSet results)
intSetList :: [[Int]] -> TypedTerm Term
intSetList sets = list (intSet <$> sets)
