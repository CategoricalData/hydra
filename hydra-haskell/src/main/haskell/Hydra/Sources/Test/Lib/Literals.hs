module Hydra.Sources.Test.Lib.Literals where

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
module_ = Module (Namespace "hydra.test.lib.literals") elements [] [] $
    Just "Test cases for hydra.lib.literals primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Test groups for hydra.lib.literals primitives
-- Note: Testing a representative subset of the many literal conversion functions

literalsBigintToInt32 :: TTerm TestGroup
literalsBigintToInt32 = subgroup "bigintToInt32" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name _literals_bigintToInt32 [bigint x] (int32 result)

literalsInt32ToBigint :: TTerm TestGroup
literalsInt32ToBigint = subgroup "int32ToBigint" [
  test "positive" 42 42,
  test "negative" (-42) (-42),
  test "zero" 0 0]
  where
    test name x result = primCase name _literals_int32ToBigint [int32 x] (bigint result)

--literalsStringToBinary :: TTerm TestGroup
--literalsStringToBinary = subgroup "stringToBinary" [
--  test "simple string" "hello" "hello",
--  test "empty string" "" ""]
--  where
--    test name x result = primCase name _literals_stringToBinary [string x] (string result)
--
--literalsBinaryToString :: TTerm TestGroup
--literalsBinaryToString = subgroup "binaryToString" [
--  test "simple string" "hello" "hello",
--  test "empty string" "" ""]
--  where
--    test name x result = primCase name _literals_binaryToString [string x] (string result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.literals primitives" $
    supergroup "hydra.lib.literals primitives" [
      literalsBigintToInt32,
      literalsInt32ToBigint]
--      literalsStringToBinary, -- TODO: restore
--      literalsBinaryToString] -- TODO: restore
