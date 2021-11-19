-- Note: these tests are dependent on HsYaml, both because the Serde depends on HsYaml
--       and because of the particular serialization style.

module Hydra.Impl.Haskell.Ext.Yaml.SerdeSpec where

import Hydra.V1.Core
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import Hydra.Impl.Haskell.Dsl
import Hydra.Impl.Haskell.Ext.Yaml.Serde

import Hydra.TestData
import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Test.QuickCheck as QC
import qualified Data.Maybe as Y


checkAtomicValues :: H.SpecWith ()
checkAtomicValues = H.describe "Test atomic values" $ do
  
  H.it "Booleans become 'true' and 'false' (not 'y' and 'n')" $ do
    QC.property $ \b -> checkSerialization
      (TypedTerm booleanType $ booleanValue b)
      (if b then "true" else "false")

  H.it "int32's become ints, and are serialized in the obvious way" $ do
    QC.property $ \i -> checkSerialization
      (TypedTerm int32Type $ int32Value i)
      (show i)
      
  H.it "uint8's and other finite integer types become ints, and are serialized in the obvious way" $ do
    QC.property $ \i -> checkSerialization
      (TypedTerm uint8Type $ uint8Value i)
      (show i)

  H.it "bigints become ints" $ do
    QC.property $ \i -> checkSerialization
      (TypedTerm bigintType $ bigintValue i)
      (show i)

  -- TODO: examine quirks around floating-point serialization more closely. These could affect portability of the serialized YAML.

  -- TODO: binary string and character string serialization

checkOptionals :: H.SpecWith ()
checkOptionals = H.describe "Test and document serialization of optionals" $ do

  H.it "A 'nothing' becomes 'null' (except when it appears as a field)" $
    QC.property $ \mi -> checkSerialization
      (TypedTerm
        (TypeOptional int32Type)
        (TermOptional $ (Just . int32Value) =<< mi))
      (Y.maybe "null" show mi)

  H.it "Nested optionals case #1: just x? :: optional<optional<int32>>" $
    QC.property $ \mi -> checkSerialization
      (TypedTerm
        (TypeOptional $ TypeOptional int32Type)
        (TermOptional $ Just $ TermOptional $ (Just . int32Value) =<< mi))
      ("- " ++ Y.maybe "null" show mi)

  H.it "Nested optionals case #2: nothing :: optional<optional<int32>>" $
    QC.property $ \() -> checkSerialization
      (TypedTerm
        (TypeOptional $ TypeOptional int32Type)
        (TermOptional Nothing))
      "[]"

checkRecordsAndUnions :: H.SpecWith ()
checkRecordsAndUnions = H.describe "Test and document handling of optionals vs. nulls for record and union types" $ do

  H.it "Empty records become empty objects" $
    QC.property $ \() -> checkSerialization (TypedTerm unitType unitTerm) "{}"

  H.it "Simple records become simple objects" $
    QC.property $ \() -> checkSerialization
      (TypedTerm latLonType (latlonRecord 37 (negate 122)))
      "lat: 37\nlon: -122"

  H.it "Optionals are omitted from record objects if 'nothing'" $
    QC.property $ \() -> checkSerialization
      (TypedTerm
        (TypeRecord [FieldType "one" $ TypeOptional stringType, FieldType "two" $ TypeOptional int32Type])
        (TermRecord [Field "one" $ TermOptional $ Just $ stringValue "test", Field "two" $ TermOptional Nothing]))
      "one: test"
      
  H.it "Simple unions become simple objects, via records" $
    QC.property $ \() -> checkSerialization
      (TypedTerm
        (TypeUnion [FieldType "left" stringType, FieldType "right" int32Type])
        (TermUnion $ Field "left" $ stringValue "test"))
      "left: test"

yamlSerdeIsInformationPreserving :: H.SpecWith ()
yamlSerdeIsInformationPreserving = H.describe "Verify that a round trip from a type+term, to serialized YAML, and back again is a no-op" $ do

  H.it "Generate arbitrary type/term pairs, serialize the terms to YAML, deserialize them, and compare" $
    QC.property checkSerdeRoundTrip

checkSerialization :: TypedTerm -> String -> H.Expectation
checkSerialization (TypedTerm typ term) expected = do
    if Y.isNothing (qualifiedValue serde)
      then qualifiedWarnings serde `H.shouldBe` []
      else True `H.shouldBe` True
    (normalize <$> stepOut serde' term) `H.shouldBe` (ResultSuccess $ normalize expected)
  where
    normalize = unlines . L.filter (not . L.null) . lines
    serde = yamlSerdeStr testContext typ
    serde' = Y.fromJust $ qualifiedValue serde

checkSerdeRoundTrip :: TypedTerm -> H.Expectation
checkSerdeRoundTrip (TypedTerm typ term) = do
    Y.isJust (qualifiedValue serde) `H.shouldBe` True
    (stepOut serde' term >>= stepIn serde') `H.shouldBe` ResultSuccess term
  where
    serde = yamlSerde testContext typ
    serde' = Y.fromJust $ qualifiedValue serde

spec :: H.Spec
spec = do
  checkAtomicValues
  checkOptionals
  checkRecordsAndUnions
  yamlSerdeIsInformationPreserving
