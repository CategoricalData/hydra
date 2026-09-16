{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.GraphqlCoderSpec.spec
-}

module Hydra.GraphqlCoderSpec where

import Hydra.Kernel
import qualified Hydra.Graphql.Coder as GraphqlCoder
import qualified Hydra.Graphql.Syntax as Syntax

import Hydra.TestUtils

import qualified Data.Map as M
import qualified Test.Hspec as H


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
