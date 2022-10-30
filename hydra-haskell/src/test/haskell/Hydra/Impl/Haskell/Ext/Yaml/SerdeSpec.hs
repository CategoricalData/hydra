-- Note: these tests are dependent on HsYaml, both because the Serde depends on HsYaml
--       and because of the particular serialization style.

module Hydra.Impl.Haskell.Ext.Yaml.SerdeSpec where

import Hydra.All
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Ext.Yaml.Serde
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import Hydra.TestData
import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Data.List as L
import qualified Test.QuickCheck as QC
import qualified Data.Maybe as Y


checkLiterals :: H.SpecWith ()
checkLiterals = H.describe "Test literal values" $ do

  H.it "Booleans become 'true' and 'false' (not 'y' and 'n')" $ do
    QC.property $ \b -> checkSerialization yamlSerdeStr
      (TypedTerm Types.boolean $ boolean b)
      (if b then "true" else "false")

  H.it "int32's become ints, and are serialized in the obvious way" $ do
    QC.property $ \i -> checkSerialization yamlSerdeStr
      (TypedTerm Types.int32 $ int32 i)
      (show i)

  H.it "uint8's and other finite integer types become ints, and are serialized in the obvious way" $ do
    QC.property $ \i -> checkSerialization yamlSerdeStr
      (TypedTerm Types.uint8 $ uint8 i)
      (show i)

  H.it "bigints become ints" $ do
    QC.property $ \i -> checkSerialization yamlSerdeStr
      (TypedTerm Types.bigint $ bigint i)
      (show i)

  -- TODO: examine quirks around floating-point serialization more closely. These could affect portability of the serialized YAML.

  -- TODO: binary string and character string serialization

checkOptionals :: H.SpecWith ()
checkOptionals = H.describe "Test and document serialization of optionals" $ do

  H.it "A 'nothing' becomes 'null' (except when it appears as a field)" $
    QC.property $ \mi -> checkSerialization yamlSerdeStr
      (TypedTerm
        (Types.optional Types.int32)
        (optional $ (Just . int32) =<< mi))
      (Y.maybe "null" show mi)

  H.it "Nested optionals case #1: just x? :: optional<optional<int32>>" $
    QC.property $ \mi -> checkSerialization yamlSerdeStr
      (TypedTerm
        (Types.optional $ Types.optional Types.int32)
        (optional $ Just $ optional $ (Just . int32) =<< mi))
      ("- " ++ Y.maybe "null" show mi)

  H.it "Nested optionals case #2: nothing :: optional<optional<int32>>" $
    QC.property $ \() -> checkSerialization yamlSerdeStr
      (TypedTerm
        (Types.optional $ Types.optional Types.int32)
        (optional Nothing))
      "[]"

checkRecordsAndUnions :: H.SpecWith ()
checkRecordsAndUnions = H.describe "Test and document handling of optionals vs. nulls for record and union types" $ do

  H.it "Empty records become empty objects" $
    QC.property $ \() -> checkSerialization yamlSerdeStr
      (TypedTerm Types.unit unit)
      "{}"

  H.it "Simple records become simple objects" $
    QC.property $ \() -> checkSerialization yamlSerdeStr
      (TypedTerm latLonType (latlonRecord 37.0 (negate 122.0)))
      "lat: 37.0\nlon: -122.0"

  H.it "Optionals are omitted from record objects if 'nothing'" $
    QC.property $ \() -> checkSerialization yamlSerdeStr
      (TypedTerm
        (TypeRecord $ RowType testTypeName Nothing [Types.field "one" $ Types.optional Types.string, Types.field "two" $ Types.optional Types.int32])
        (record testTypeName [Field (FieldName "one") $ optional $ Just $ string "test", Field (FieldName "two") $ optional Nothing]))
      "one: test"

  H.it "Simple unions become simple objects, via records" $
    QC.property $ \() -> checkSerialization yamlSerdeStr
      (TypedTerm
        (TypeUnion $ RowType testTypeName Nothing [Types.field "left" Types.string, Types.field "right" Types.int32])
        (union testTypeName $ Field (FieldName "left") $ string "test"))
      "left: test\n"

yamlSerdeIsInformationPreserving :: H.SpecWith ()
yamlSerdeIsInformationPreserving = H.describe "Verify that a round trip from a type+term, to serialized YAML, and back again is a no-op" $ do

  H.it "Generate arbitrary type/term pairs, serialize the terms to YAML, deserialize them, and compare" $
    QC.property (checkSerdeRoundTrip yamlSerde)

spec :: H.Spec
spec = do
  checkLiterals
  checkOptionals
  checkRecordsAndUnions
--  yamlSerdeIsInformationPreserving -- TODO: restore me
