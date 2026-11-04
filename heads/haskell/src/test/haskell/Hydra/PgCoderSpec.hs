{-# LANGUAGE NoMonomorphismRestriction #-}

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.PgCoderSpec.spec
-}

module Hydra.PgCoderSpec where

import Hydra.Kernel
import qualified Hydra.Core.Coders as Coders
import qualified Hydra.Core.Model as CoreModel
import qualified Hydra.Pg.Coder as PgCoder
import qualified Hydra.Pg.Mapping as PgMapping
import qualified Hydra.Pg.Model as PgModel

import Hydra.TestUtils

import qualified Data.Set as S
import qualified Test.Hspec as H


-- | Regression test for #752 Bug 4: propertyAdapter/propertyTypes hardcoded
-- PropertyType.required = True regardless of whether the source field's type is optional. See
-- packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg/Coder.hs, propertyAdapter/propertyTypes.
spec :: H.Spec
spec = do
 H.describe "Pg.Coder.propertyAdapter" $ do

  H.it "marks a required (non-optional) field's PropertyType as required" $ do
    case mapError $ PgCoder.propertyAdapter testContext testGraph identitySchema (requiredField, (PgMapping.ValueSpecValue, Nothing)) of
      Left e -> H.expectationFailure $ "propertyAdapter failed: " ++ e
      Right adapter -> PgModel.propertyTypeRequired (Coders.adapterTarget adapter) `H.shouldBe` True

  H.it "marks an optional field's PropertyType as not required" $ do
    case mapError $ PgCoder.propertyAdapter testContext testGraph identitySchema (optionalField, (PgMapping.ValueSpecValue, Nothing)) of
      Left e -> H.expectationFailure $ "propertyAdapter failed: " ++ e
      Right adapter -> PgModel.propertyTypeRequired (Coders.adapterTarget adapter) `H.shouldBe` False

  -- #752 Bug 5, per Josh's 2026-11-01 decision: a collection-typed (list/set) field should
  -- become ONE property whose value is the WHOLE collection, not several single-valued
  -- properties. This test checks whether that already holds for the default (no ${...}
  -- pattern annotation) case, with no hydra-pg code changes.
  H.describe "Pg.Coder.propertyAdapter on a collection-typed field (#752 Bug 5)" $ do

    H.it "encodes a List<String> field as one property whose value is the whole list term" $ do
      case mapError $ PgCoder.propertyAdapter testContext testGraph identitySchema (listField, (PgMapping.ValueSpecValue, Nothing)) of
        Left e -> H.expectationFailure $ "propertyAdapter failed: " ++ e
        Right adapter ->
          case mapError $ Coders.coderEncode (Coders.adapterCoder adapter) (CoreModel.Field (CoreModel.Name "tags") listTerm) of
            Left e -> H.expectationFailure $ "encode failed: " ++ e
            Right prop -> PgModel.propertyValue prop `H.shouldBe` listTerm

    H.it "encodes a Set<String> field as one property whose value is the whole set term" $ do
      case mapError $ PgCoder.propertyAdapter testContext testGraph identitySchema (setField, (PgMapping.ValueSpecValue, Nothing)) of
        Left e -> H.expectationFailure $ "propertyAdapter failed: " ++ e
        Right adapter ->
          case mapError $ Coders.coderEncode (Coders.adapterCoder adapter) (CoreModel.Field (CoreModel.Name "tags") setTerm) of
            Left e -> H.expectationFailure $ "encode failed: " ++ e
            Right prop -> PgModel.propertyValue prop `H.shouldBe` setTerm
  where
    requiredField = CoreModel.FieldType (CoreModel.Name "name") (CoreModel.TypeLiteral CoreModel.LiteralTypeString)
    optionalField = CoreModel.FieldType (CoreModel.Name "nickname") (CoreModel.TypeOptional (CoreModel.TypeLiteral CoreModel.LiteralTypeString))
    listField = CoreModel.FieldType (CoreModel.Name "tags") (CoreModel.TypeList (CoreModel.TypeLiteral CoreModel.LiteralTypeString))
    setField = CoreModel.FieldType (CoreModel.Name "tags") (CoreModel.TypeSet (CoreModel.TypeLiteral CoreModel.LiteralTypeString))
    listTerm = CoreModel.TermList [CoreModel.TermLiteral (CoreModel.LiteralString "a"), CoreModel.TermLiteral (CoreModel.LiteralString "b")]
    setTerm = CoreModel.TermSet (S.fromList [CoreModel.TermLiteral (CoreModel.LiteralString "a"), CoreModel.TermLiteral (CoreModel.LiteralString "b")])
    identityCoder = Coders.Coder { Coders.coderEncode = Right, Coders.coderDecode = Right }
    identitySchema = PgMapping.Schema {
      PgMapping.schemaVertexIdTypes = identityCoder,
      PgMapping.schemaVertexIds = identityCoder,
      PgMapping.schemaEdgeIdTypes = identityCoder,
      PgMapping.schemaEdgeIds = identityCoder,
      PgMapping.schemaPropertyTypes = identityCoder,
      PgMapping.schemaPropertyValues = identityCoder,
      PgMapping.schemaAnnotations = PgMapping.AnnotationSchema {
        PgMapping.annotationSchemaVertexLabel = "vertexLabel",
        PgMapping.annotationSchemaEdgeLabel = "edgeLabel",
        PgMapping.annotationSchemaVertexId = "vertexId",
        PgMapping.annotationSchemaEdgeId = "edgeId",
        PgMapping.annotationSchemaPropertyKey = "propertyKey",
        PgMapping.annotationSchemaPropertyValue = "propertyValue",
        PgMapping.annotationSchemaOutVertex = "outVertex",
        PgMapping.annotationSchemaOutVertexLabel = "outVertexLabel",
        PgMapping.annotationSchemaInVertex = "inVertex",
        PgMapping.annotationSchemaInVertexLabel = "inVertexLabel",
        PgMapping.annotationSchemaOutEdge = "outEdge",
        PgMapping.annotationSchemaOutEdgeLabel = "outEdgeLabel",
        PgMapping.annotationSchemaInEdge = "inEdge",
        PgMapping.annotationSchemaInEdgeLabel = "inEdgeLabel",
        PgMapping.annotationSchemaIgnore = "ignore"},
      PgMapping.schemaDefaultVertexId = CoreModel.TermLiteral (CoreModel.LiteralString ""),
      PgMapping.schemaDefaultEdgeId = CoreModel.TermLiteral (CoreModel.LiteralString "")}
