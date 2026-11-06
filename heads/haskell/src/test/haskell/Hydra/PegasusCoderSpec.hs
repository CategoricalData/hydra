{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.PegasusCoderSpec.spec
-}

module Hydra.PegasusCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Pegasus.Coder as Coder

import Hydra.ExtCoderTestSupport
import Hydra.TestUtils

import qualified Data.Map as M
import qualified Test.Hspec as H


-- | Golden-output tests for the Pegasus generation coder (#816), rendered through the real
-- `moduleToPdl` entry point (encode definitions -> Serde -> printExpr).
spec :: H.Spec
spec = H.describe "Pegasus.Coder.moduleToPdl golden output (#816)" $ do

  H.it "generates a record type" $ do
    generated [personRecord] `H.shouldBe` M.fromList [("example/shapes/Person.pdl", expected [
      "namespace example.shapes",
      "",
      "record Person {",
      "  name: string,",
      "",
      "  age: int,",
      "",
      "  active: optional boolean",
      "}"])]

  H.it "generates a union type" $ do
    generated [shapeUnion] `H.shouldBe` M.fromList [("example/shapes/Shape.pdl", expected [
      "namespace example.shapes",
      "",
      "typeref Shape = union[",
      "  label: string,",
      "",
      "  size: int",
      "]"])]

  where
    generated defs = case mapError (Coder.moduleToPdl (fixtureModule defs) defs testContext testGraph) of
      Left e -> error e
      Right m -> m
