{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ShaclCoderSpec.spec
-}

module Hydra.ShaclCoderSpec where

import Hydra.Kernel
import Hydra.Overlay.Haskell.Dsl.Terms as Terms
import qualified Hydra.Shacl.Coder as ShaclCoder
import qualified Hydra.Rdf.Syntax as Rdf

import Hydra.TestUtils

import qualified Test.Hspec as H


-- | Regression test for #653: encodeTerm had no case for Term.Unit, so any injection whose field is
-- unit-valued (i.e. an enum instance, via injectUnit) failed with "unsupported term variant" instead
-- of encoding. See packages/hydra-rdf/src/main/haskell/Hydra/Sources/Shacl/Coder.hs, encodeTerm.
spec :: H.Spec
spec = H.describe "Shacl.Coder.encodeTerm" $ do

  H.it "encodes a bare Term.Unit as an empty description list, cx unchanged" $ do
    H.shouldBe
      (mapError $ ShaclCoder.encodeTerm testSubject Terms.unit 0 testGraph)
      (Right ([], 0))

  H.it "encodes an injection with a unit-valued field (the enum-instance idiom)" $ do
    shouldSucceed
      (mapError $ ShaclCoder.encodeTerm testSubject enumInstance 0 testGraph)
  where
    testSubject = Rdf.ResourceIri $ Rdf.Iri "urn:example:1"
    enumInstance = injectUnit (Name "test.Color") (Name "red")
