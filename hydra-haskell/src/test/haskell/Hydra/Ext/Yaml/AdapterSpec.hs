module Hydra.Ext.Yaml.AdapterSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.Steps
import qualified Hydra.Ext.Yaml.Adapter as YA

import Hydra.ArbitraryCore
import Hydra.TestGraph

import qualified Test.Hspec as H
import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as QC


-- Note: in a real application, you wouldn't create the adapter just to use it once;
--       it should be created once, then applied to many terms.
adapt typ dir term = do
  adapter <- YA.termAdapter testContext typ
  dir adapter term

booleanElementType = TypeElement $ TypeAtomic AtomicTypeBoolean
booleanElementDataType = functionType booleanElementType booleanType
concatType = functionType stringType $ functionType stringType stringType
compareStringsType = functionType stringType stringType
exampleProjectionType = functionType latLonType int64Type
latLonType = TypeRecord [FieldType "lat" $ int64Type, FieldType "lon" $ int64Type]
listOfStringsType = TypeList stringType
latlonRecord lat lon = TermRecord [Field "lat" $ int32Value lat, Field "lon" $ int32Value lon]
makeMap keyvals = TermMap $ M.fromList $ ((\(k, v) -> (stringValue k, int32Value v)) <$> keyvals)
mapOfStringsToIntsType = mapType stringType int32Type
setOfStringsType = TypeSet stringType
stringAliasType = TypeNominal "StringTypeAlias"
stringList strings = TermList $ stringValue <$> strings
stringOrIntType = TypeUnion [FieldType "left" stringType, FieldType "right" int32Type]
stringSet strings = TermSet $ S.fromList $ stringValue <$> (S.toList strings)

supportedConstructorsAreUnchanged = do
  H.describe "Verify that supported term constructors are unchanged" $ do
    
    H.it "Strings (and other supported atomic values) pass through without change" $
      QC.property $ \s
        -> adapt stringType stepOut (stringValue s)
        == Right (stringValue s)  

    H.it "Lists pass through without change" $
      QC.property $ \strings
        -> adapt listOfStringsType stepOut (TermList $ stringValue <$> strings)
        == Right (TermList $ stringValue <$> strings)  
        
    H.it "Maps pass through without change" $
      QC.property $ \keyvals
        -> adapt mapOfStringsToIntsType stepOut (makeMap keyvals)
        == Right (makeMap keyvals)  
        
    H.it "Records pass through without change" $
      QC.property $ \(lat, lon)
        -> adapt latLonType stepOut (latlonRecord lat lon)
        == Right (latlonRecord lat lon)

    H.it "Unions pass through without change" $
      QC.property $ \int
        -> adapt stringOrIntType stepOut (variant "right" int)
        == Right (variant "right" int)
        
unsupportedConstructorsAreModified = do
  H.describe "Verify that unsupported term constructors are changed in the expected ways" $ do

    H.it "Sets become lists" $
      QC.property $ \strings
        -> adapt setOfStringsType stepOut (stringSet strings)
        == Right (stringList $ S.toList strings)  

    H.it "Element references become strings" $
      QC.property $ \name
        -> adapt booleanElementType stepOut (TermElement name) -- Note: the element name is not dereferenced
        == Right (stringValue name)  

    H.it "CompareTo terms become variant terms" $
      QC.property $ \s
        -> adapt compareStringsType stepOut (TermCompareTo $ stringValue s)
        == Right (TermUnion $ Field "compareTo" $ stringValue s)  

    H.it "Data terms become variant terms" $ do
      adapt booleanElementDataType stepOut TermData
      `H.shouldBe` Right (TermUnion $ Field "data" unitTerm)  

    H.it "Primitive function references become variant terms" $
      QC.property $ \name
        -> adapt concatType stepOut (TermFunction name)  -- Note: the function name is not dereferenced
        == Right (TermUnion $ Field "function" $ stringValue name)  

    H.it "Projections become variant terms" $ do
      QC.property $ \fname
        -> adapt exampleProjectionType stepOut (TermProjection fname)-- Note: the field name is not dereferenced
        == Right (TermUnion $ Field "projection" $ stringValue fname)  

nominalTypesPassThrough = do
  H.describe "Verify that nominal types behave like the types they reference" $ do

    H.it "A term typed by StringTypeAlias just behaves like a string" $ do
      adapt stringAliasType stepOut (stringValue "Arthur Dent")
        `H.shouldBe` Right (stringValue "Arthur Dent")  

termsAreAdaptedRecursively = do
  H.describe "Verify that the adapter descends into subterms and transforms them appropriately" $ do
    
    H.it "A list of sets of strings becomes a list of lists of strings" $ do
      H.it "Projections become variant terms" $ do
        QC.property $ \fname
          -> adapt exampleProjectionType stepOut (TermProjection fname)-- Note: the field name is not dereferenced
          == Right (TermUnion $ Field "projection" $ stringValue fname)  
              
-- adapterIsInformationPreserving

spec :: H.Spec
spec = do
  supportedConstructorsAreUnchanged
  unsupportedConstructorsAreModified
  nominalTypesPassThrough
  