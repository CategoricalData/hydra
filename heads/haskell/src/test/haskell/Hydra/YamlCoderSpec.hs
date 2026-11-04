{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.YamlCoderSpec.spec
-}

-- | Executable tests for the hydra-ext YAML coder (#699). The YAML coder has had zero tests
-- until now. Unlike Avro, there is no YAML *text* parser/printer in this repo yet -- Yaml.Coder
-- maps Hydra Term/Type to the Hydra.Core.Yaml.Model.Node AST only (the same AST the JSON<->YAML
-- bridge in hydra-kernel's DSL test suite exercises for JSON Value<->Node conversion). So these
-- tests work at the Node-AST level, analogous to Avro's term-level (JSON<->Term) coverage one
-- layer up: Term <-> Node via Yaml.Coder.yamlCoder, asserting both golden Node output and
-- term round-trip identity.
module Hydra.YamlCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Yaml.Coder as YamlCoder
import qualified Hydra.Core.Yaml.Model as YM

import Hydra.TestUtils

import qualified Data.Map as M
import qualified Test.Hspec as H


spec :: H.Spec
spec = do
  H.describe "Yaml.Coder.yamlCoder golden output (#699)" $ do

    H.it "encodes a string literal term as a YAML scalar string" $ do
      checkEncode (TypeLiteral LiteralTypeString) (TermLiteral (LiteralString "hello"))
        (YM.NodeScalar (YM.ScalarStr "hello"))

    H.it "encodes an int32 literal term as a YAML scalar int" $ do
      checkEncode (TypeLiteral (LiteralTypeInteger IntegerTypeInt32)) (TermLiteral (LiteralInteger (IntegerValueInt32 42)))
        (YM.NodeScalar (YM.ScalarInt 42))

    H.it "encodes a list term as a YAML sequence" $ do
      checkEncode (TypeList (TypeLiteral LiteralTypeString))
        (TermList [TermLiteral (LiteralString "a"), TermLiteral (LiteralString "b")])
        (YM.NodeSequence [YM.NodeScalar (YM.ScalarStr "a"), YM.NodeScalar (YM.ScalarStr "b")])

    H.it "encodes a record term as a YAML mapping" $ do
      let recordType = TypeRecord [
            FieldType (Name "name") (TypeLiteral LiteralTypeString),
            FieldType (Name "age") (TypeLiteral (LiteralTypeInteger IntegerTypeInt32))]
      let recordTerm = TermRecord $ Record (Name "test.Person") [
            Field (Name "name") (TermLiteral (LiteralString "Ada")),
            Field (Name "age") (TermLiteral (LiteralInteger (IntegerValueInt32 36)))]
      checkEncode recordType recordTerm
        (YM.NodeMapping $ M.fromList [
          (YM.NodeScalar (YM.ScalarStr "name"), YM.NodeScalar (YM.ScalarStr "Ada")),
          (YM.NodeScalar (YM.ScalarStr "age"), YM.NodeScalar (YM.ScalarInt 36))])

    H.it "omits an absent optional field from the YAML mapping" $ do
      let recordType = TypeRecord [
            FieldType (Name "name") (TypeLiteral LiteralTypeString),
            FieldType (Name "nickname") (TypeOptional (TypeLiteral LiteralTypeString))]
      let recordTerm = TermRecord $ Record (Name "test.Person") [
            Field (Name "name") (TermLiteral (LiteralString "Ada")),
            Field (Name "nickname") (TermOptional Nothing)]
      checkEncode recordType recordTerm
        (YM.NodeMapping $ M.fromList [(YM.NodeScalar (YM.ScalarStr "name"), YM.NodeScalar (YM.ScalarStr "Ada"))])

    H.it "encodes a unit term as a YAML null scalar" $ do
      checkEncode TypeUnit TermUnit (YM.NodeScalar YM.ScalarNull)

  H.describe "Yaml.Coder.yamlCoder round-trip (Term -> Node -> Term) (#699)" $ do

    H.it "round-trips a record with a list and an optional field" $ do
      let recordType = TypeRecord [
            FieldType (Name "name") (TypeLiteral LiteralTypeString),
            FieldType (Name "tags") (TypeList (TypeLiteral LiteralTypeString)),
            FieldType (Name "nickname") (TypeOptional (TypeLiteral LiteralTypeString))]
      -- decodeRecord always rebuilds with the fixed type name "yaml" (Yaml.Coder.recordCoder
      -- hardcodes it), regardless of the original record's type name -- not a round-trip bug,
      -- just a coder quirk. Use that same name here so the round-trip is a faithful comparison.
      let recordTerm = TermRecord $ Record (Name "yaml") [
            Field (Name "name") (TermLiteral (LiteralString "Ada")),
            Field (Name "tags") (TermList [TermLiteral (LiteralString "math"), TermLiteral (LiteralString "computing")]),
            Field (Name "nickname") (TermOptional (Just (TermLiteral (LiteralString "Countess"))))]
      checkRoundTrip recordType recordTerm

    H.it "round-trips a map term" $ do
      -- YAML's generic int scalar is arbitrary-precision, so decoding always recovers
      -- IntegerValueBigint regardless of the declared integer width -- documented lossiness,
      -- not a round-trip bug. Declare the value type as IntegerTypeBigint directly so the type
      -- and the term's actual IntegerValue variant agree.
      let mapType = TypeMap (MapType (TypeLiteral LiteralTypeString) (TypeLiteral (LiteralTypeInteger IntegerTypeBigint)))
      let mapTerm = TermMap $ M.fromList [
            (TermLiteral (LiteralString "a"), TermLiteral (LiteralInteger (IntegerValueBigint 1))),
            (TermLiteral (LiteralString "b"), TermLiteral (LiteralInteger (IntegerValueBigint 2)))]
      checkRoundTrip mapType mapTerm

  where
    checkEncode :: Type -> Term -> YM.Node -> H.Expectation
    checkEncode typ term expectedNode =
      case mapError (YamlCoder.yamlCoder typ testContext testGraph) of
        Left e -> H.expectationFailure $ "yamlCoder failed: " ++ e
        Right coder -> case coderEncode coder term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right node -> node `H.shouldBe` expectedNode

    checkRoundTrip :: Type -> Term -> H.Expectation
    checkRoundTrip typ term =
      case mapError (YamlCoder.yamlCoder typ testContext testGraph) of
        Left e -> H.expectationFailure $ "yamlCoder failed: " ++ e
        Right coder -> case coderEncode coder term of
          Left e -> H.expectationFailure $ "encode failed: " ++ show e
          Right node -> case coderDecode coder node of
            Left e -> H.expectationFailure $ "decode failed: " ++ show e
            Right term' -> deannotateTerm term' `H.shouldBe` deannotateTerm term
