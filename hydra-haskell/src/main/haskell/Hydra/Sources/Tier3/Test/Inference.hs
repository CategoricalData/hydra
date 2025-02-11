module Hydra.Sources.Tier3.Test.Inference (inferenceTests) where

import Hydra.Kernel
import Hydra.Dsl.Tests
import Hydra.Dsl.ShorthandTypes
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


inferenceTests :: TestGroup
inferenceTests = TestGroup "inference tests" Nothing groups []
  where
    groups = [
      applicationTerms,
      functionTerms]

expectMono i term typ = infTest ("#" ++ show i) term $ Types.mono typ

expectPoly i term params typ = infTest ("#" ++ show i) term $ Types.poly params typ

infTest :: String -> Term -> TypeScheme -> TestCaseWithMetadata
infTest name term ts = TestCaseWithMetadata name (TestCaseInference $ InferenceTestCase term ts) Nothing []

---

applicationTerms :: TestGroup
applicationTerms = TestGroup "Application terms" (Just "Check a few hand-picked application terms") [] [
    expectMono 1
      ((lambda "x" $ var "x") @@ string "foo")
      tString,
    expectMono 2
      (lambda "x" $ primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1)
      (tFun tInt32 tInt32)]

functionTerms :: TestGroup
functionTerms = TestGroup "Function terms" (Just "Check a few hand-picked function terms") subgroups []
  where
    foldAdd = Terms.fold $ primitive _math_add
    subgroups = [
      TestGroup "Check lambdas" Nothing [] [
        expectPoly 1
          (lambda "x" (var "x"))
          ["t0"] (Types.function (Types.var "t0") (Types.var "t0")),
        expectPoly 2
          (lambda "x" (int16 137))
          ["t0"] (Types.function (Types.var "t0") Types.int16)],

      TestGroup "Check list eliminations" Nothing [] [
        expectMono 1
          foldAdd
          (Types.functionN [Types.int32, Types.list Types.int32, Types.int32]),
        expectMono 2
          (apply foldAdd $ int32 0)
          (Types.function (Types.list Types.int32) Types.int32),
        expectMono 3
          (apply (apply foldAdd $ int32 0) (list (int32 <$> [1, 2, 3, 4, 5])))
          Types.int32]]

--      TestGroup "Check projections" Nothing [] [
--        expectMono 1
--          (project testTypePersonName (Name "firstName"))
--          (Types.function testTypePerson Types.string)],

--      TestGroup "Check case statements" [] [
--        expectMono
--          (match testTypeFoobarValueName Nothing [
--            Field (Name "bool") (lambda "x" (boolean True)),
--            Field (Name "string") (lambda "x" (boolean False)),
--            Field (Name "unit") (lambda "x" (boolean False))])
--          (Types.function testTypeFoobarValue Types.boolean)]]
