{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.RustCoderSpec.spec
-}

module Hydra.RustCoderSpec where

import Hydra.Kernel
import qualified Hydra.Rust.Coder as Coder

import Hydra.ExtCoderTestSupport
import Hydra.TestUtils

import qualified Data.Map as M
import qualified Test.Hspec as H


-- | Golden-output tests for the Rust generation coder (#816), rendered through the real
-- `moduleToRust` entry point (encode definitions -> Serde -> printExpr).
-- NOTE: output is pinned as-is, including `pub #[derive(...)]` (visibility emitted before the attribute) and
-- `Option <bool>` spacing, which look like printer bugs; see the #816 plan findings.
spec :: H.Spec
spec = H.describe "Rust.Coder.moduleToRust golden output (#816)" $ do

  H.it "generates a record type" $ do
    generated [personRecord] `H.shouldBe` M.fromList [("example/shapes.rs", expected [
      "pub #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]",
      "struct Person {",
      "  pub name: String,",
      "  pub age: i32,",
      "  pub active: Option <bool>}"])]

  H.it "generates a union type" $ do
    generated [shapeUnion] `H.shouldBe` M.fromList [("example/shapes.rs", expected [
      "pub #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]",
      "enum Shape {",
      "  Label (String),",
      "  Size (i32)}"])]

  where
    generated defs = case mapError (Coder.moduleToRust (fixtureModule defs) defs testContext testGraph) of
      Left e -> error e
      Right m -> m
