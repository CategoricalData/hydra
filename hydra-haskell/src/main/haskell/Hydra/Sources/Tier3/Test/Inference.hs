module Hydra.Sources.Tier3.Test.Inference (inferenceTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph

import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T

import qualified Data.Map as M


subgroup :: String -> [TTerm TestCaseWithMetadata] -> TTerm TestGroup
subgroup name = tgroup name Nothing []

inferenceTests :: TTerm TestGroup
inferenceTests = tgroup "inference tests" Nothing groups []
  where
    groups = [
      applicationTerms,
      functionTerms,
      individualTerms]

applicationTerms :: TTerm TestGroup
applicationTerms = tgroup "Application terms" (Just "Check a few hand-picked application terms") [] [
    expectMono 1
      ((lambda "x" $ var "x") @@ string "foo")
      T.string,
    expectMono 2
      (lambda "x" $ primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1)
      (T.function T.int32 T.int32)]

functionTerms :: TTerm TestGroup
functionTerms = tgroup "Function terms" (Just "Check a few hand-picked function terms") subgroups []
  where
    foldAdd = fold $ primitive _math_add
    subgroups = [
      subgroup "Check lambdas" [
        expectPoly 1
          (lambda "x" $ var "x")
          ["t0"] (T.function (T.var "t0") (T.var "t0")),
        expectPoly 2
          (lambda "x" $ int16 137)
          ["t0"] (T.function (T.var "t0") T.int16)],

      subgroup "Check list eliminations" [
        expectMono 1
          foldAdd
          (T.functionN [T.int32, T.list T.int32, T.int32]),
        expectMono 2
          (apply foldAdd $ int32 0)
          (T.function (T.list T.int32) T.int32),
        expectMono 3
          (apply (apply foldAdd $ int32 0) (list (int32 <$> [1, 2, 3, 4, 5])))
          T.int32],

      subgroup "Check projections" [
        expectMono 1
          (project (ref testTypePersonNameDef) (name "firstName"))
          (T.function (ref testTypePersonDef) T.string)],

      subgroup "Check case statements" [
        expectMono 1
          (match (ref testTypeFoobarValueNameDef) nothing [
            "bool">: constant true,
            "string">: constant false,
            "unit">: constant false])
          (T.function (ref testTypeFoobarValueDef) T.boolean)]]

individualTerms :: TTerm TestGroup
individualTerms = tgroup "Individual terms" (Just "Check a few hand-picked terms") subgroups []
  where
    subgroups = [
      subgroup "Check literal values" [
        expectMono 1
          (int32 42)
          T.int32,
        expectMono 2
          (string "foo")
          T.string,
        expectMono 3
          false
          T.boolean,
        expectMono 4
          (float64 42.0)
          T.float64],
          
      subgroup "Check let terms" [
        expectPoly 1
          (let1 "x" (float32 42.0) $ lambdas ["y", "z"] $ var "x")
          ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") T.float32)),
        -- Example from https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
        expectMono 2
          (lets [
              "square">: lambda "z" $ primitive _math_mul @@ var "z" @@ var "z"] $
            lambdas ["f", "x", "y"] $ primitive _logic_ifElse
                @@ (var "f" @@ (var "square" @@ var "x") @@ var "y")
                @@ (var "f" @@ var "x" @@ (var "f" @@ var "x" @@ var "y"))
                @@ (var "f" @@ var "x" @@ var "y"))
          (T.functionN [
            T.functionN [T.int32, T.boolean, T.boolean], T.int32, T.boolean, T.boolean])],

      subgroup "Check optionals" [
        expectMono 1
          (optional $ just $ int32 42)
          (T.optional T.int32),
        expectPoly 2
          (optional nothing)
          ["t0"] (T.optional $ T.var "t0")],

      subgroup "Check products" [
        expectMono 1
          (tuple [])
          (T.product []),
        expectMono 2
          (pair (int32 42) (string "foo"))
          (T.product [T.int32, T.string])],

      subgroup "Check records" [
        expectMono 1
          (record (ref testTypeLatLonNameDef) [
            "lat">: float32 37.7749,
            "lon">: float32 $ negate 122.4194])
          (T.record (ref testTypeLatLonNameDef) [
            "lat">: T.float32,
            "lon">: T.float32]),
        expectMono 2
          (record (ref testTypeLatLonPolyNameDef) [
            "lat">: float32 37.7749,
            "lon">: float32 $ negate 122.4194])
          (T.record (ref testTypeLatLonPolyNameDef) [
            "lat">: T.float32,
            "lon">: T.float32]),
        expectMono 3
          (lambda "lon" (record (ref testTypeLatLonPolyNameDef) [
            "lat">: float32 37.7749,
            "lon">: var "lon"]))
          (T.function (T.float32)
            (T.record (ref testTypeLatLonPolyNameDef) [
              "lat">: T.float32,
              "lon">: T.float32])),
        expectPoly 4
          (lambda "latlon" (record (ref testTypeLatLonPolyNameDef) [
            "lat">: var "latlon",
            "lon">: var "latlon"]))
          ["t0"] (T.function (T.var "t0")
            (T.record (ref testTypeLatLonPolyNameDef) [
              "lat">: T.var "t0",
              "lon">: T.var "t0"]))],

      subgroup "Check unions" [
        expectMono 1
          (inject (ref testTypeTimestampNameDef) "unixTimeMillis" $ uint64 1638200308368)
          (ref testTypeTimestampDef)],

      subgroup "Check sets" [
        expectMono 1
          (set [true])
          (T.set T.boolean),
        expectPoly 2
          (set [set []])
          ["t0"] (T.set $ T.set $ T.var "t0")],

      subgroup "Check maps" [
        expectMono 1
          (mapTermCheat [
            (Terms.string "firstName", Terms.string "Arthur"),
            (Terms.string "lastName", Terms.string "Dent")])
          (T.map T.string T.string),
        expectPoly 2
          (mapTerm Maps.empty)
          ["t0", "t1"] (T.map (T.var "t0") (T.var "t1")),
        expectPoly 3
          (lambdas ["x", "y"] $ mapTermCheat
            [(Terms.var "x", Terms.float64 0.1), (Terms.var "y", Terms.float64 0.2)])
          ["t0"] (T.function (T.var "t0") (T.function (T.var "t0") (T.map (T.var "t0") T.float64)))]]

--    -- -- TODO: add a case for a recursive nominal type -- e.g. MyList := () + (int, Mylist)
--    -- subgroup "Check nominal (newtype) terms" [
--    --   expectMono
--    --     testDataArthur
--    --     (T.wrap "Person")
--    --   expectMono
--    --     (lambda "x" (record [
--    --       Field "firstName" $ var "x",
--    --       Field "lastName" $ var "x",
--    --       Field "age" $ int32 42]))
--    --     (T.function T.string testTypePerson)

-- Note: this is a cheat for an encoded map term; consider using the TTerms DSL
mapTermCheat :: [(Term, Term)] -> TTerm Term
mapTermCheat = TTerm . coreEncodeTerm . Terms.mapTerm . M.fromList
