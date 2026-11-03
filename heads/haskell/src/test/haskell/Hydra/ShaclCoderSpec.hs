{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ShaclCoderSpec.spec
-}

module Hydra.ShaclCoderSpec where

import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Dsl.Terms as Terms
import qualified Hydra.Rdf.Shacl.Coder as ShaclCoder
import qualified Hydra.Rdf.Shacl.Model as ShaclModel
import qualified Hydra.Rdf.ShaclRdf as ShaclRdf
import qualified Hydra.Rdf.Syntax as Rdf

import Hydra.TestUtils

import qualified Data.Char as C
import qualified Data.Maybe as Y
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

 -- Regression test for #751 Bug 1: the serializer dropped anonymous shape references (default `[]`),
 -- and the union encoding consists solely of `sh:xone` over anonymous inline node shapes, so union
 -- shapes enforced nothing. Anonymous references now get deterministic blank nodes, and sh:xone (like
 -- sh:and/sh:or) is emitted as a single SHACL list rather than one triple per shape.
 H.describe "ShaclRdf serialization of union shapes (#751 Bug 1)" $ do

    H.it "emits sh:xone as one list of anonymous node shapes, each carrying its own constraints" $ do
      let ut = TypeUnion [
                 FieldType (Name "a") (TypeLiteral LiteralTypeString),
                 FieldType (Name "b") (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))]
      case mapError $ ShaclCoder.encodeType (Name "test.MyUnion") ut testContext of
        Left e -> H.expectationFailure $ "encodeType failed: " ++ e
        Right props -> do
          let triples = ShaclRdf.commonPropertiesToTriples testSubject props
              xones = objectsOf triples testSubject (sh "xone")
          -- exactly one sh:xone triple (the bug's symptom was zero)
          length xones `H.shouldBe` 1
          case listMembers triples (head xones) of
            Nothing -> H.expectationFailure "sh:xone object is not a well-formed RDF list"
            Just members -> do
              length members `H.shouldBe` 2
              mapM_ (\m -> case m of
                Rdf.NodeBnode b -> do
                  let r = Rdf.ResourceBnode b
                  objectsOf triples r (rdf "type") `H.shouldBe` [Rdf.NodeIri (sh "NodeShape")]
                  objectsOf triples r (sh "property") `H.shouldNotBe` []
                _ -> H.expectationFailure $ "expected a blank node member, got " ++ show m) members
          -- every blank node label is valid in N-Triples
          bnodeLabels triples `H.shouldSatisfy` all validLabel

    H.it "emits an anonymous sh:property shape on a blank node instead of dropping it" $ do
      let ps = ShaclModel.PropertyShape {
                 ShaclModel.propertyShapeCommon = emptyCommon,
                 ShaclModel.propertyShapeConstraints = S.empty,
                 ShaclModel.propertyShapeDefaultValue = Nothing,
                 ShaclModel.propertyShapeDescription = Rdf.LangStrings mempty,
                 ShaclModel.propertyShapeName = Rdf.LangStrings mempty,
                 ShaclModel.propertyShapeOrder = Nothing,
                 ShaclModel.propertyShapePath = Rdf.Iri "urn:example:p"}
          cc = ShaclModel.CommonConstraintProperty (S.singleton (ShaclModel.ReferenceAnonymous ps))
          triples = ShaclRdf.commonConstraintToTriples testSubject cc
      case objectsOf triples testSubject (sh "property") of
        [Rdf.NodeBnode b] ->
          objectsOf triples (Rdf.ResourceBnode b) (sh "path") `H.shouldBe` [Rdf.NodeIri (Rdf.Iri "urn:example:p")]
        os -> H.expectationFailure $ "expected one blank node sh:property object, got " ++ show os

 -- Regression tests for #751 Bug 2: sh:in was emitted as N parallel triples (a conjunction, hence
 -- unsatisfiable for N > 1), and sh:languageIn was dropped. Both must be a single SHACL list.
 H.describe "ShaclRdf serialization of list-valued constraints (#751 Bug 2)" $ do

    H.it "emits sh:in as a single RDF list, not one triple per value" $ do
      let vals = [lit "x", lit "y", lit "z"]
          triples = ShaclRdf.commonConstraintToTriples testSubject (ShaclModel.CommonConstraintIn vals)
          ins = objectsOf triples testSubject (sh "in")
      length ins `H.shouldBe` 1
      listMembers triples (head ins) `H.shouldBe` Just vals
      bnodeLabels triples `H.shouldSatisfy` all validLabel

    H.it "emits an empty sh:in as rdf:nil" $ do
      let triples = ShaclRdf.commonConstraintToTriples testSubject (ShaclModel.CommonConstraintIn [])
      triples `H.shouldBe` [Rdf.Triple testSubject (sh "in") (Rdf.NodeIri (rdf "nil"))]

    H.it "emits sh:languageIn as a single RDF list of language tags" $ do
      let tags = S.fromList [Rdf.LanguageTag "en", Rdf.LanguageTag "fr"]
          triples = ShaclRdf.commonConstraintToTriples testSubject (ShaclModel.CommonConstraintLanguageIn tags)
          lis = objectsOf triples testSubject (sh "languageIn")
      length lis `H.shouldBe` 1
      listMembers triples (head lis) `H.shouldBe` Just [lit "en", lit "fr"]
  where
    testSubject = Rdf.ResourceIri $ Rdf.Iri "urn:example:1"
    enumInstance = injectUnit (Name "test.Color") (Name "red")
    sh local = Rdf.Iri $ "http://www.w3.org/ns/shacl#" ++ local
    rdf local = Rdf.Iri $ "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ++ local
    lit s = Rdf.NodeLiteral $ Rdf.Literal s (Rdf.Iri "http://www.w3.org/2001/XMLSchema#string") Nothing
    objectsOf triples s p = [Rdf.tripleObject t | t <- triples, Rdf.tripleSubject t == s, Rdf.triplePredicate t == p]
    -- Follow an RDF collection from its head, returning its members if it is well-formed
    listMembers triples n
      | n == Rdf.NodeIri (rdf "nil") = Just []
      | otherwise = case n of
          Rdf.NodeBnode b -> case (objectsOf triples r (rdf "first"), objectsOf triples r (rdf "rest")) of
              ([x], [rest]) -> (x :) <$> listMembers triples rest
              _ -> Nothing
            where r = Rdf.ResourceBnode b
          _ -> Nothing
    bnodeLabels triples = Y.catMaybes $ concat
      [[bnodeOfResource (Rdf.tripleSubject t), bnodeOfNode (Rdf.tripleObject t)] | t <- triples]
    bnodeOfResource r = case r of
      Rdf.ResourceBnode (Rdf.BlankNode l) -> Just l
      _ -> Nothing
    bnodeOfNode n = case n of
      Rdf.NodeBnode (Rdf.BlankNode l) -> Just l
      _ -> Nothing
    validLabel l = not (null l) && all (\c -> C.isAscii c && (C.isAlphaNum c || c == '_' || c == '-')) l
      && head l /= '-' && last l /= '-'
    emptyCommon = ShaclModel.CommonProperties {
      ShaclModel.commonPropertiesConstraints = S.empty,
      ShaclModel.commonPropertiesDeactivated = Nothing,
      ShaclModel.commonPropertiesMessage = Rdf.LangStrings mempty,
      ShaclModel.commonPropertiesSeverity = ShaclModel.SeverityInfo,
      ShaclModel.commonPropertiesTargetClass = S.empty,
      ShaclModel.commonPropertiesTargetNode = S.empty,
      ShaclModel.commonPropertiesTargetObjectsOf = S.empty,
      ShaclModel.commonPropertiesTargetSubjectsOf = S.empty}
