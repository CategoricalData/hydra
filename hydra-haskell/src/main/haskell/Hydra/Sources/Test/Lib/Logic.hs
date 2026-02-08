module Hydra.Sources.Test.Lib.Logic where

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
ns = Namespace "hydra.test.lib.logic"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for hydra.lib.logic primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Test groups for hydra.lib.logic primitives

logicAnd :: TTerm TestGroup
logicAnd = subgroup "and" [
  test "true and true" true true true,
  test "true and false" true false false,
  test "false and true" false true false,
  test "false and false" false false false]
  where
    test name x y result = primCase name _logic_and [x, y] (result)

logicOr :: TTerm TestGroup
logicOr = subgroup "or" [
  test "true or true" true true true,
  test "true or false" true false true,
  test "false or true" false true true,
  test "false or false" false false false]
  where
    test name x y result = primCase name _logic_or [x, y] (result)

logicNot :: TTerm TestGroup
logicNot = subgroup "not" [
  test "not true" true false,
  test "not false" false true]
  where
    test name x result = primCase name _logic_not [x] (result)

logicIfElse :: TTerm TestGroup
logicIfElse = supergroup "ifElse" [
  subgroup "boolean values" [
    testBool "true condition returns then" true true false true,
    testBool "false condition returns else" false true false false],
  subgroup "integer values" [
    testInt "true selects first int" true 42 0 42,
    testInt "false selects second int" false 42 0 0],
  subgroup "string values" [
    testStr "true selects first string" true "yes" "no" "yes",
    testStr "false selects second string" false "yes" "no" "no"]]
  where
    testBool name cond thenVal elseVal result =
      primCase name _logic_ifElse [cond, thenVal, elseVal] (result)
    testInt name cond thenVal elseVal result =
      primCase name _logic_ifElse [cond, int32 thenVal, int32 elseVal] (int32 result)
    testStr name cond thenVal elseVal result =
      primCase name _logic_ifElse [cond, string thenVal, string elseVal] (string result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.logic primitives" $
    supergroup "hydra.lib.logic primitives" [
      logicAnd,
      logicIfElse,
      logicNot,
      logicOr]
