module Hydra.Ext.Json.CoderSpec where

import Hydra.Core
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Ext.Json.Coder
import Hydra.Impl.Haskell.Extras
import Hydra.Steps
import qualified Hydra.Ext.Json.Model as Json
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Lib.Literals
import Hydra.Impl.Haskell.Dsl.Standard

import Hydra.TestData
import Hydra.TestUtils

import qualified Data.Bifunctor as BF
import qualified Test.Hspec as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Test.QuickCheck as QC


literalTypeConstraintsAreRespected :: H.SpecWith ()
literalTypeConstraintsAreRespected = H.describe "Verify that JSON's literal type constraints are respected" $ do

  -- TODO: binary data

  H.it "Check booleans" $
    QC.property $ \b -> checkJsonCoder Types.boolean (Terms.boolean b) (Json.ValueBoolean b)

  H.it "Check 32-bit floats" $
    QC.property $ \f -> checkJsonCoder Types.float32 (Terms.float32 f) (jsonFloat $ realToFrac f)

  H.it "Check 64-bit floats (doubles)" $
    QC.property $ \d -> checkJsonCoder Types.float64 (Terms.float64 d) (jsonFloat $ realToFrac d)

  -- TODO: bigfloat

  H.it "Check 32-bit integers" $
    QC.property $ \i -> checkJsonCoder Types.int32 (Terms.int32 i) (jsonInt i)

  H.it "Check 16-bit unsigned integers" $
    QC.property $ \i -> checkJsonCoder Types.uint16 (Terms.uint16 i) (jsonInt i)

  H.it "Check arbitrary-precision integers" $
    QC.property $ \i -> checkJsonCoder Types.bigint (Terms.bigint i) (jsonInt i)

  H.it "Check strings" $
    QC.property $ \s -> checkJsonCoder Types.string (Terms.string s) (Json.ValueString s)

supportedTypesPassThrough :: H.SpecWith ()
supportedTypesPassThrough = H.describe "Verify that supported types are mapped directly" $ do

  H.it "Lists become JSON arrays" $
    QC.property $ \strings -> checkJsonCoder listOfStringsType
      (Terms.list $ Terms.string <$> strings) (Json.ValueArray $ Json.ValueString <$> strings)

  H.it "Maps become JSON objects" $
    QC.property $ \keyvals -> checkJsonCoder mapOfStringsToIntsType
      (makeMap keyvals) (jsonMap $ BF.bimap id jsonInt <$> keyvals)

  H.it "Optionals become JSON null or type-specific values" $
    QC.property $ \ms -> checkJsonCoder optionalStringType
      (Terms.optional $ Terms.string <$> ms) (Y.maybe Json.ValueNull Json.ValueString ms)

  H.it "Records become JSON objects" $
    QC.property $ \lat lon -> checkJsonCoder latLonType
      (latlonRecord lat lon) (jsonMap [
        ("lat", jsonInt lat),
        ("lon", jsonInt lon)])

unsupportedTypesAreTransformed :: H.SpecWith ()
unsupportedTypesAreTransformed = H.describe "Verify that unsupported types are transformed appropriately" $ do

  -- TODO: functions

  H.it "Element references become strings" $
    QC.property $ \name -> checkJsonCoder int32ElementType
      (Terms.element name)
      (Json.ValueString $ unName name)

  H.it "Sets become arrays" $
    QC.property $ \strings -> checkJsonCoder setOfStringsType
      (Terms.stringSet strings)
      (Json.ValueArray $ Json.ValueString <$> S.toList strings)

  H.it "Nominal types are dereferenced" $
    QC.property $ \s -> checkJsonCoder stringAliasType
      (Terms.string s)
      (Json.ValueString s)

  H.it "Unions become JSON objects (as records)" $
    QC.property $ \int -> checkJsonCoder stringOrIntType
      (Terms.variant (FieldName "right") $ Terms.int32 int)
      (jsonMap [("right", jsonInt int)])

nominalTypesAreSupported :: H.SpecWith ()
nominalTypesAreSupported = H.describe "Verify that nominal types are supported" $ do
  H.it "Nominal unions become single-attribute objects" $
    QC.property $ \() -> checkJsonCoder (Types.nominal $ Name "Color")
      (Terms.union $ Terms.field "bool" $ Terms.boolean True)
      (jsonMap [("bool", jsonBool True)])
  
  H.it "Nominal enums become single-attribute objects with empty-object values, and type annotations are transparent" $
    QC.property $ \() -> checkJsonCoder (Types.nominal $ Name "Comparison")
      (Terms.union $ Terms.field "equalTo" Terms.unit)
      (jsonMap [("equalTo", jsonMap [])])

spec :: H.Spec
spec = do
  literalTypeConstraintsAreRespected
  supportedTypesPassThrough
  unsupportedTypesAreTransformed
  nominalTypesAreSupported

checkJsonCoder :: Type Meta -> Term Meta -> Json.Value -> H.Expectation
checkJsonCoder typ term node = do
    (if Y.isJust step' then [] else warnings) `H.shouldBe` []
    coderEncode step term `H.shouldBe` ResultSuccess node
    (coderEncode step term >>= coderDecode step) `H.shouldBe` ResultSuccess term
  where
    (Qualified step' warnings) = jsonCoder testContext typ
    step = Y.fromJust step'

jsonBool :: Bool -> Json.Value
jsonBool = Json.ValueBoolean

jsonFloat :: Double -> Json.Value
jsonFloat = Json.ValueNumber

jsonInt :: Integral i => i -> Json.Value
jsonInt = Json.ValueNumber . bigintToBigfloat . fromIntegral

jsonMap :: [(String, Json.Value)] -> Json.Value
jsonMap = Json.ValueObject . M.fromList
