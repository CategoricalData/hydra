{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ShaclCoderSpec.spec
-}

module Hydra.ShaclCoderSpec where

import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Dsl.Terms as Terms
import qualified Hydra.Rdf.Shacl.Coder as ShaclCoder
import qualified Hydra.Rdf.Shacl.Model as ShaclModel
import qualified Hydra.Rdf.Syntax as Rdf

import Hydra.TestUtils

import qualified Data.Set as S
import qualified Test.Hspec as H


-- | Regression test for #653: encodeTerm had no case for Term.Unit, so any injection whose field is
-- unit-valued (i.e. an enum instance, via injectUnit) failed with "unsupported term variant" instead
-- of encoding. See packages/hydra-rdf/src/main/haskell/Hydra/Sources/Shacl/Coder.hs, encodeTerm.
spec :: H.Spec
spec = do
 H.describe "Shacl.Coder.encodeTerm" $ do

  H.it "encodes a bare Term.Unit as an empty description list, cx unchanged" $ do
    H.shouldBe
      (mapError $ ShaclCoder.encodeTerm testSubject Terms.unit 0 testGraph)
      (Right ([], 0))

  H.it "encodes an injection with a unit-valued field (the enum-instance idiom)" $ do
    shouldSucceed
      (mapError $ ShaclCoder.encodeTerm testSubject enumInstance 0 testGraph)

  -- Regression tests for #654 Bug A: encodeTerm's _Term_wrap case used to call withType
  -- directly on the wrapped body's Description, whose subject can be a literal (RDF forbids
  -- literal-subject triples) -- crashing on any wrapped literal. Fixed by normalizing the
  -- wrapped term to a single-field record (wrapTermToRecord) before encoding, so the subject
  -- is always the caller's own IRI/bnode, never a literal.
  H.describe "Shacl.Coder.encodeTerm on Term.Wrap (#654 Bug A)" $ do

    H.it "encodes a wrapped string literal without crashing, tagging the wrapper type" $ do
      let wrapped = TermWrap (WrappedTerm (Name "test.MyId") (Terms.string "hello"))
      case mapError $ ShaclCoder.encodeTerm testSubject wrapped 0 testGraph of
        Left e -> H.expectationFailure $ "encodeTerm failed: " ++ e
        Right (descs, _) -> do
          length descs `H.shouldBe` 1
          let triples = S.toList $ Rdf.unGraph $ Rdf.descriptionGraph $ head descs
          -- one triple for rdf:type (the wrapper name), one for the "value" field
          length triples `H.shouldBe` 2

    H.it "encodes a wrapped int32 literal without crashing" $ do
      let wrapped = TermWrap (WrappedTerm (Name "test.Count") (Terms.int32 42))
      shouldSucceed (mapError $ ShaclCoder.encodeTerm testSubject wrapped 0 testGraph)

 H.describe "Shacl.Coder.encodeType on Type.Wrap (#654 Bug B)" $ do

    -- Bug B: encodeType's _Type_wrap case collapsed to `any` (an unconstrained shape),
    -- discarding the inner type. Fixed by normalizing to a single-field record type
    -- (wrapTypeToRecord) and dispatching through the existing _Type_record path, so the
    -- inner type's constraint (e.g. xsd:string) is preserved on the "value" property.
    H.it "encodes wrap(string) as a constrained shape, not an empty 'any'" $ do
      let wrapped = TypeWrap (TypeLiteral LiteralTypeString)
      case mapError $ ShaclCoder.encodeType (Name "test.MyId") wrapped testContext of
        Left e -> H.expectationFailure $ "encodeType failed: " ++ e
        Right props -> ShaclModel.commonPropertiesConstraints props `H.shouldNotBe` S.empty

 -- Regression test for #751 Bug 3: a bare set<X> field inherited the enclosing field's minCount 1,
 -- so an empty set failed validation. encodeFieldType's _Type_set arm now forces minCount 0 (an
 -- empty set is valid), matching the optional arm. maxCount stays unbounded.
 H.describe "Shacl.Coder.encodeFieldType on bare set<X> (#751 Bug 3)" $ do

    H.it "gives a bare set<string> field minCount 0, not the inherited 1" $ do
      let ft = FieldType (Name "tags") (TypeSet (TypeLiteral LiteralTypeString))
      -- encodeFieldType's 4th arg (cx) is polymorphic (t0) and unused for this path; () avoids an
      -- ambiguous-type default. The 2nd arg (Just 0) is sh:order, unrelated to minCount.
      case mapError $ ShaclCoder.encodeFieldType (Name "test.MyRecord") (Just 0) ft () of
        Left e -> H.expectationFailure $ "encodeFieldType failed: " ++ e
        Right def -> do
          let constraints = S.toList $ ShaclModel.propertyShapeConstraints
                              $ ShaclModel.definitionTarget def
              minCounts = [n | ShaclModel.PropertyShapeConstraintMinCount n <- constraints]
          -- the only minCount present (if any) must be 0 — never the inherited 1
          minCounts `H.shouldSatisfy` all (== 0)
          -- and it must not carry minCount 1 (the bug's symptom)
          minCounts `H.shouldSatisfy` notElem 1
  where
    testSubject = Rdf.ResourceIri $ Rdf.Iri "urn:example:1"
    enumInstance = injectUnit (Name "test.Color") (Name "red")
