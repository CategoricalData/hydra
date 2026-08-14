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
