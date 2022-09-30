{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Dsl.TypesSpec where

import Hydra.Compute
import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Types

import qualified Test.Hspec as H


check :: Type Meta -> Type Meta -> H.Expectation
check = H.shouldBe

checkFunctionSyntax :: H.SpecWith ()
checkFunctionSyntax = do
  H.describe "Check function syntax" $ do

    H.it "Function arrows are supported" $ do
      check
        ("a" --> "b")
        (function (variable "a") (variable "b"))
      check
        (string --> int32)
        (function string int32)

    H.it "Function arrows are right-associative" $ do
      check
        ("a" --> "b" --> "c")
        ("a" --> ("b" --> "c"))

    H.it "Functions bind less tightly than application" $ do
      check
        ("a" @@ "b" --> "c" @@ "d")
        (("a" @@ "b") --> ("c" @@ "d"))

spec :: H.Spec
spec = do
  checkFunctionSyntax
