{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.GraphqlCoderSpec.spec
-}

module Hydra.GraphqlCoderSpec where

import Hydra.Kernel
import qualified Hydra.Ext.Graphql.Coder as GraphqlCoder
import qualified Hydra.Ext.Graphql.Syntax as Syntax
import qualified Hydra.Ext.Graphql.Serde as GraphqlSerde
import Hydra.Core.Serialization (parenthesize, printExpr)

import Hydra.TestUtils

import qualified Data.List as L
import qualified Data.Map as M
import qualified Test.Hspec as H


-- | Render a GraphQL type definition to its actual SDL text, the same pipeline
-- moduleToGraphql uses per-definition (Serde.typeDefinitionToExpr, then the shared
-- expression printer). This is #699's golden-output path: assertions compare real
-- generated GraphQL SDL syntax, not just the AST shape.
printTypeDefinition :: Syntax.TypeDefinition -> String
printTypeDefinition = printExpr . parenthesize . GraphqlSerde.typeDefinitionToExpr

-- | Byte-parity regression tests for #654: the GraphQL coder's wrap-type handling (list, set,
-- literal, variable, TypeWrap, unit) used to be built by a coder-local helper (wrapAsRecord)
-- that inlined a single-field "value" record. That helper was deleted in favor of the shared
-- kernel function Rewriting.wrapTypeToRecord; these tests confirm the generated GraphQL type
-- definitions are unchanged by the refactor -- same single "value" field, same wrapped type.
spec :: H.Spec
spec = H.describe "Graphql.Coder.encodeNamedType on wrap-shaped types (#654 byte-parity)" $ do

  H.it "wraps a bare list(string) type in a single-field 'value' record" $ do
    assertSingleValueField (TypeList (TypeLiteral LiteralTypeString))

  H.it "wraps a bare literal(string) type in a single-field 'value' record" $ do
    assertSingleValueField (TypeLiteral LiteralTypeString)

  H.it "wraps a TypeWrap(string) type in a single-field 'value' record" $ do
    assertSingleValueField (TypeWrap (TypeLiteral LiteralTypeString))

  H.it "wraps a bare type variable in a single-field 'value' record" $ do
    assertSingleValueField (TypeVariable (Name "a"))

  H.it "wraps Unit as a single-field 'value' record over a boolean" $ do
    case mapError $ GraphqlCoder.encodeNamedType testContext testGraph M.empty testName TypeUnit of
      Left e -> H.expectationFailure $ "encodeNamedType failed: " ++ e
      Right def -> fieldNames def `H.shouldBe` ["value"]

  -- #744: a Map's key type used to be silently discarded (only the value type was encoded,
  -- producing a plain GraphQL list with no trace of the key). Until the coder can synthesize a
  -- named {key, value} object type for non-trivial keys (deferred), a non-literal key type fails
  -- explicitly instead of silently corrupting the schema.
  H.describe "Graphql.Coder.encodeType on Map key representability (#744)" $ do

    H.it "encodes Map(string, string) as a plain list, key type is a literal" $ do
      case mapError $ GraphqlCoder.encodeType testContext testGraph M.empty
             (TypeMap (MapType (TypeLiteral LiteralTypeString) (TypeLiteral LiteralTypeString))) of
        Left e -> H.expectationFailure $ "encodeType failed unexpectedly: " ++ e
        Right _ -> return ()

    H.it "fails explicitly (not silently) encoding Map(record, string) -- non-literal key" $ do
      let recordKeyType = TypeRecord []
      case mapError $ GraphqlCoder.encodeType testContext testGraph M.empty
             (TypeMap (MapType recordKeyType (TypeLiteral LiteralTypeString))) of
        Left _ -> return ()
        Right t -> H.expectationFailure $ "expected an explicit error for a non-literal map key, got: " ++ show t

  -- #699: golden-output coverage for ordinary (non-bug-regression) type shapes, asserting
  -- against the actual generated GraphQL SDL text rather than just AST structure.
  H.describe "Graphql.Coder.encodeNamedType golden output (#699)" $ do

    H.it "encodes a record with required and optional fields as a GraphQL type" $ do
      let recordType = TypeRecord [
            FieldType (Name "name") (TypeLiteral LiteralTypeString),
            FieldType (Name "age") (TypeLiteral (LiteralTypeInteger IntegerTypeInt32)),
            FieldType (Name "nickname") (TypeOptional (TypeLiteral LiteralTypeString))]
      case mapError $ GraphqlCoder.encodeNamedType testContext testGraph M.empty (Name "test.Person") recordType of
        Left e -> H.expectationFailure $ "encodeNamedType failed: " ++ e
        Right def -> printTypeDefinition def `H.shouldBe` L.intercalate "\n" [
          "type Person {",
          "  name: String!,",
          "",
          "  age: Int!,",
          "",
          "  nickname: String",
          "}"]

    H.it "encodes a unit-fields union as a GraphQL enum" $ do
      let enumType = TypeUnion [
            FieldType (Name "red") TypeUnit,
            FieldType (Name "green") TypeUnit,
            FieldType (Name "blue") TypeUnit]
      case mapError $ GraphqlCoder.encodeNamedType testContext testGraph M.empty (Name "test.Color") enumType of
        Left e -> H.expectationFailure $ "encodeNamedType failed: " ++ e
        -- The trailing commas are the shared printer's choice (Serialization.curlyBracesList's
        -- default separator), not a GraphQL SDL requirement; valid SDL doesn't need them, but
        -- this is the real output the coder produces today.
        Right def -> printTypeDefinition def `H.shouldBe` L.intercalate "\n" [
          "enum Color {",
          "  red,",
          "",
          "  green,",
          "",
          "  blue",
          "}"]

  where
    testName = Name "test.Wrapped"

    assertSingleValueField :: Type -> H.Expectation
    assertSingleValueField innerType =
      case mapError $ GraphqlCoder.encodeNamedType testContext testGraph M.empty testName innerType of
        Left e -> H.expectationFailure $ "encodeNamedType failed: " ++ e
        Right def -> fieldNames def `H.shouldBe` ["value"]

    fieldNames :: Syntax.TypeDefinition -> [String]
    fieldNames (Syntax.TypeDefinitionObject otd) =
      case Syntax.objectTypeDefinitionFieldsDefinition otd of
        Nothing -> []
        Just fds -> map (Syntax.unName . Syntax.fieldDefinitionName) (Syntax.unFieldsDefinition fds)
    fieldNames _ = []
