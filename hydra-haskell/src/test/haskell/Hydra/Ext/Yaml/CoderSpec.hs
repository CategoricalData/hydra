module Hydra.Ext.Yaml.CoderSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Ext.Yaml.Coder
import Hydra.Impl.Haskell.Extras
import Hydra.Steps
import qualified Hydra.Ext.Yaml.Model as YM
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import Hydra.TestData
import Hydra.TestUtils
import Hydra.ArbitraryCore (untyped)

import qualified Data.Bifunctor as BF
import qualified Test.Hspec as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Test.QuickCheck as QC


literalTypeConstraintsAreRespected :: H.SpecWith ()
literalTypeConstraintsAreRespected = H.describe "Verify that YAML's literal type constraints are respected" $ do

  -- TODO: binary data

  H.it "Check booleans" $
    QC.property $ \b -> checkYamlCoder Types.boolean (booleanValue b) (yamlBool b)

  H.it "Check 32-bit floats" $
    QC.property $ \f -> checkYamlCoder Types.float32 (float32Value f) (yamlFloat $ realToFrac f)

  H.it "Check 64-bit floats (doubles)" $
    QC.property $ \d -> checkYamlCoder Types.float64 (float64Value d) (yamlFloat $ realToFrac d)

  -- TODO: bigfloat

  H.it "Check 32-bit integers" $
    QC.property $ \i -> checkYamlCoder Types.int32 (int32Value i) (yamlInt i)

  H.it "Check 16-bit unsigned integers" $
    QC.property $ \i -> checkYamlCoder Types.uint16 (uint16Value i) (yamlInt i)

  H.it "Check arbitrary-precision integers" $
    QC.property $ \i -> checkYamlCoder Types.bigint (bigintValue i) (yamlInt i)

  H.it "Check strings" $
    QC.property $ \s -> checkYamlCoder Types.string (stringValue s) (yamlStr s)

supportedTypesPassThrough :: H.SpecWith ()
supportedTypesPassThrough = H.describe "Verify that supported types are mapped directly" $ do

  H.it "Lists become YAML sequences" $
    QC.property $ \strings -> checkYamlCoder listOfStringsType
      (list $ stringValue <$> strings) (YM.NodeSequence $ yamlStr <$> strings)

  H.it "Maps become YAML mappings" $
    QC.property $ \keyvals -> checkYamlCoder mapOfStringsToIntsType
      (makeMap keyvals) (yamlMap $ BF.bimap yamlStr yamlInt <$> keyvals)

  H.it "Optionals become YAML null or type-specific nodes" $
    QC.property $ \ms -> checkYamlCoder optionalStringType
      (optional $ stringValue <$> ms) (YM.NodeScalar $ Y.maybe YM.ScalarNull YM.ScalarStr ms)

  H.it "Records become YAML mappings" $
    QC.property $ \lat lon -> checkYamlCoder latLonType
      (latlonRecord lat lon) (yamlMap [
        (yamlStr "lat", yamlInt lat),
        (yamlStr "lon", yamlInt lon)])

unsupportedTypesAreTransformed :: H.SpecWith ()
unsupportedTypesAreTransformed = H.describe "Verify that unsupported types are transformed appropriately" $ do

  -- TODO: functions

  H.it "Element references become strings" $
    QC.property $ \name -> checkYamlCoder int32ElementType
      (element name) (yamlStr name)

  H.it "Sets become sequences" $
    QC.property $ \strings -> checkYamlCoder setOfStringsType
      (stringSet strings) (YM.NodeSequence $ yamlStr <$> S.toList strings)

  H.it "Nominal types are dereferenced" $
    QC.property $ \s -> checkYamlCoder stringAliasType
      (stringValue s) (yamlStr s)

  H.it "Unions become YAML mappings (as records)" $
    QC.property $ \int -> checkYamlCoder stringOrIntType
      (variant "right" $ int32Value int)
      (yamlMap [
        (yamlStr "context", yamlStr untyped),
        (yamlStr "record", yamlMap [(yamlStr "right", yamlInt int)])])

spec :: H.Spec
spec = do
  literalTypeConstraintsAreRespected
  supportedTypesPassThrough
  unsupportedTypesAreTransformed

checkYamlCoder :: Type -> Term Meta -> YM.Node -> H.Expectation
checkYamlCoder typ term node = do
    (if Y.isJust step' then [] else warnings) `H.shouldBe` []
    stepOut step term `H.shouldBe` ResultSuccess node
    (stepOut step term >>= stepIn step) `H.shouldBe` ResultSuccess term
  where
    (Qualified step' warnings) = yamlCoder testContext typ
    step = Y.fromJust step'

yamlBool :: Bool -> YM.Node
yamlBool = YM.NodeScalar . YM.ScalarBool

yamlFloat :: Double -> YM.Node
yamlFloat = YM.NodeScalar . YM.ScalarFloat

yamlInt :: Integral i => i -> YM.Node
yamlInt = YM.NodeScalar . YM.ScalarInt . fromIntegral

yamlMap :: [(YM.Node, YM.Node)] -> YM.Node
yamlMap = YM.NodeMapping . M.fromList

yamlNull :: YM.Node
yamlNull = YM.NodeScalar YM.ScalarNull

yamlStr :: String -> YM.Node
yamlStr = YM.NodeScalar . YM.ScalarStr
