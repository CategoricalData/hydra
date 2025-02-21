module Hydra.Sources.Tier3.Test.Inference (inferenceTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph

import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T


inferenceTests :: TTerm TestGroup
inferenceTests = tgroup "inference tests" Nothing groups []
  where
    groups = [
      applicationTerms,
      functionTerms]

applicationTerms :: TTerm TestGroup
applicationTerms = tgroup "Application terms" (Just "Check a few hand-picked application terms") [] [
    expectMono 1
      ((lambda "x" $ var "x") @@ string "foo")
      T.string,
    expectMono 2
      (lambda "x" $ prim _math_sub @@ (prim _math_add @@ var "x" @@ var "x") @@ int32 1)
      (T.function T.int32 T.int32)]

functionTerms :: TTerm TestGroup
functionTerms = tgroup "Function terms" (Just "Check a few hand-picked function terms") subgroups []
  where
    foldAdd = fold $ prim _math_add
    subgroups = [
      tgroup "Check lambdas" Nothing [] [
        expectPoly 1
          (lambda "x" $ var "x")
          ["t0"] (T.function (T.var "t0") (T.var "t0")),
        expectPoly 2
          (lambda "x" $ int16 137)
          ["t0"] (T.function (T.var "t0") T.int16)],

      tgroup "Check list eliminations" Nothing [] [
        expectMono 1
          foldAdd
          (T.functionN [T.int32, T.list T.int32, T.int32]),
        expectMono 2
          (apply foldAdd $ int32 0)
          (T.function (T.list T.int32) T.int32),
        expectMono 3
          (apply (apply foldAdd $ int32 0) (list (int32 <$> [1, 2, 3, 4, 5])))
          T.int32],

      tgroup "Check projections" Nothing [] [
        expectMono 1
          (project (ref testTypePersonNameDef) (name "firstName"))
          (T.function (ref testTypePersonDef) T.string)],

      tgroup "Check case statements" Nothing [] [
        expectMono 1
          (match (ref testTypeFoobarValueNameDef) nothing [
            "bool">: constant true,
            "string">: constant false,
            "unit">: constant false])
          (T.function (ref testTypeFoobarValueDef) T.boolean)]]
