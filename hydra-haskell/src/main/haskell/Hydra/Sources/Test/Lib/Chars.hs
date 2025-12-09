module Hydra.Sources.Test.Lib.Chars where

import Data.Char (ord)
import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes
import qualified Hydra.Sources.Test.TestGraph as TestGraph


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.chars") elements [] [] $
    Just "Test cases for hydra.lib.chars primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Test groups for hydra.lib.chars primitives

charsIsAlphaNum :: TTerm TestGroup
charsIsAlphaNum = subgroup "isAlphaNum" [
  test "letter" (ord 'a') true,
  test "digit" (ord '5') true,
  test "space" (ord ' ') false,
  test "punctuation" (ord '.') false]
  where
    test name x result = primCase name _chars_isAlphaNum [int32 x] result

charsIsLower :: TTerm TestGroup
charsIsLower = subgroup "isLower" [
  test "lowercase" (ord 'a') true,
  test "uppercase" (ord 'A') false,
  test "digit" (ord '5') false]
  where
    test name x result = primCase name _chars_isLower [int32 x] result

charsIsSpace :: TTerm TestGroup
charsIsSpace = subgroup "isSpace" [
  test "space" (ord ' ') true,
  test "tab" (ord '\t') true,
  test "newline" (ord '\n') true,
  test "letter" (ord 'a') false]
  where
    test name x result = primCase name _chars_isSpace [int32 x] result

charsIsUpper :: TTerm TestGroup
charsIsUpper = subgroup "isUpper" [
  test "uppercase" (ord 'A') true,
  test "lowercase" (ord 'a') false,
  test "digit" (ord '5') false]
  where
    test name x result = primCase name _chars_isUpper [int32 x] result

charsToLower :: TTerm TestGroup
charsToLower = subgroup "toLower" [
  test "uppercase" (ord 'A') (ord 'a'),
  test "lowercase" (ord 'a') (ord 'a'),
  test "digit" (ord '5') (ord '5')]
  where
    test name x result = primCase name _chars_toLower [int32 x] (int32 result)

charsToUpper :: TTerm TestGroup
charsToUpper = subgroup "toUpper" [
  test "lowercase" (ord 'a') (ord 'A'),
  test "uppercase" (ord 'A') (ord 'A'),
  test "digit" (ord '5') (ord '5')]
  where
    test name x result = primCase name _chars_toUpper [int32 x] (int32 result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.chars primitives" $
    supergroup "hydra.lib.chars primitives" [
      charsIsAlphaNum,
      charsIsLower,
      charsIsSpace,
      charsIsUpper,
      charsToLower,
      charsToUpper]
