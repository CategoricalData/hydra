module Hydra.Prototyping.Adapters.TermSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import Hydra.Adapter

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as QC


-- Use a YAML-like language (but supporting unions) as the default target language
testLanguage :: Language
testLanguage = Language "hydra/test" $ Language_Constraints {
  languageConstraintsAtomicVariants = S.fromList [
    AtomicVariantBoolean, AtomicVariantFloat, AtomicVariantInteger, AtomicVariantString],
  languageConstraintsFloatVariants = S.fromList [FloatVariantBigfloat],
  languageConstraintsIntegerVariants = S.fromList [IntegerVariantBigint],
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAtomic, TypeVariantList, TypeVariantMap, TypeVariantRecord, TypeVariantUnion] }

transContext :: AdapterContext
transContext = AdapterContext testContext hydraCoreLanguage testLanguage

-- Note: in a real application, you wouldn't create the adapter just to use it once;
--       it should be created once, then applied to many terms.
adapt :: Type -> (Step Term Term -> t -> Result b) -> t -> Result b
adapt typ dir term = do
  ad <- qualifiedToResult $ termAdapter transContext typ
  dir (adapterStep ad) term

concatType :: Type
concatType = functionType stringType $ functionType stringType stringType

compareStringsType :: Type
compareStringsType = functionType stringType stringType

exampleProjectionType :: Type
exampleProjectionType = functionType latLonType int32Type

int32ElementType :: Type
int32ElementType = TypeElement int32Type

int32ElementDataType :: Type
int32ElementDataType = functionType int32ElementType int32Type

latLonType :: Type
latLonType = TypeRecord [FieldType "lat" int32Type, FieldType "lon" int32Type]

listOfInt8sType :: Type
listOfInt8sType = TypeList int8Type

listOfInt16sType :: Type
listOfInt16sType = TypeList int16Type

listOfListsOfStringsType :: Type
listOfListsOfStringsType = TypeList $ TypeList stringType

listOfSetOfInt32ElementReferencesType :: Type
listOfSetOfInt32ElementReferencesType = TypeList $ TypeSet $ TypeElement int32Type

listOfSetOfStringsType :: Type
listOfSetOfStringsType = TypeList $ TypeSet stringType

listOfStringsType :: Type
listOfStringsType = TypeList stringType

latlonRecord :: Int -> Int -> Term
latlonRecord lat lon = TermRecord [Field "lat" $ int32Value lat, Field "lon" $ int32Value lon]

makeMap :: [(String, Int)] -> Term
makeMap keyvals = TermMap $ M.fromList $ ((\(k, v) -> (stringValue k, int32Value v)) <$> keyvals)

mapOfStringsToIntsType :: Type
mapOfStringsToIntsType = mapType stringType int32Type

setOfStringsType :: Type
setOfStringsType = TypeSet stringType

stringAliasType :: Type
stringAliasType = TypeNominal "StringTypeAlias"

stringList :: [String] -> Term
stringList strings = TermList $ stringValue <$> strings

stringOrIntType :: Type
stringOrIntType = TypeUnion [FieldType "left" stringType, FieldType "right" int32Type]

stringSet :: S.Set String -> Term
stringSet strings = TermSet $ S.fromList $ stringValue <$> S.toList strings

unionTypeForFunctions :: Type -> Type
unionTypeForFunctions dom = TypeUnion [
  FieldType _Function_cases stringType, -- TODO (TypeRecord cases)
  FieldType _Function_compareTo dom,
  FieldType _Function_data unitType,
  FieldType _Function_lambda stringType, -- TODO (TypeRecord [FieldType _Lambda_parameter stringType, FieldType _Lambda_body cod]),
  FieldType _Function_primitive stringType,
  FieldType _Function_projection stringType,
  FieldType _Term_variable stringType] -- TODO

supportedConstructorsAreUnchanged :: H.SpecWith ()
supportedConstructorsAreUnchanged = H.describe "Verify that supported term constructors are unchanged" $ do

  H.it "Strings (and other supported atomic values) pass through without change" $
    QC.property $ \b -> checkTermAdapter
      [TypeVariantAtomic]
      stringType stringType False
      (stringValue b) (stringValue b)

  H.it "Lists (when supported) pass through without change" $
    QC.property $ \strings -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantList]
      listOfStringsType listOfStringsType False
      (TermList $ stringValue <$> strings) (TermList $ stringValue <$> strings)

  H.it "Maps (when supported) pass through without change" $
    QC.property $ \keyvals -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantMap]
      mapOfStringsToIntsType mapOfStringsToIntsType False
      (makeMap keyvals) (makeMap keyvals)

  H.it "Records (when supported) pass through without change" $
    QC.property $ \lat lon -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantRecord]
      latLonType latLonType False
      (latlonRecord lat lon) (latlonRecord lat lon)

  H.it "Unions (when supported) pass through without change" $
    QC.property $ \int -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantUnion]
      stringOrIntType stringOrIntType False
      (variant "right" int) (variant "right" int)

  H.it "Sets (when supported) pass through without change" $
    QC.property $ \strings -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantSet]
      setOfStringsType setOfStringsType False
      (stringSet strings) (stringSet strings)

  H.it "Element references (when supported) pass through without change" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantElement]
      int32ElementType int32ElementType False
      (TermElement name) (TermElement name)

  H.it "CompareTo terms (when supported) pass through without change" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantFunction]
      compareStringsType compareStringsType False
      (compareTo $ stringValue s) (compareTo $ stringValue s)

  H.it "Data terms (when supported) pass through without change" $
    QC.property $ \() -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantFunction, TypeVariantElement]
      int32ElementDataType int32ElementDataType False
      dataTerm dataTerm

  H.it "Primitive function references (when supported) pass through without change" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantFunction]
      concatType concatType False
      (primitive name) (primitive name)

  H.it "Projections (when supported) pass through without change" $
    QC.property $ \fname -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantFunction, TypeVariantRecord]
      exampleProjectionType exampleProjectionType False
      (projection fname) (projection fname)

  H.it "Nominal types (when supported) pass through without change" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantNominal]
      stringAliasType stringAliasType False
      (stringValue s) (stringValue s)

unsupportedConstructorsAreModified :: H.SpecWith ()
unsupportedConstructorsAreModified = H.describe "Verify that unsupported term constructors are changed in the expected ways" $ do

  H.it "Sets (when unsupported) become lists" $
    QC.property $ \strings -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantList]
      setOfStringsType listOfStringsType False
      (stringSet strings) (stringList $ S.toList strings)

  H.it "Element references (when unsupported) become strings" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantAtomic]
      int32ElementType stringType False
      (TermElement name) (stringValue name) -- Note: the element name is not dereferenced

  H.it "CompareTo terms (when unsupported) become variant terms" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantUnion, TypeVariantRecord]
      compareStringsType (unionTypeForFunctions stringType) False
      (compareTo $ stringValue s) (TermUnion $ Field "compareTo" $ stringValue s)

  H.it "Data terms (when unsupported) become variant terms" $
    QC.property $ \() -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantUnion, TypeVariantRecord]
      int32ElementDataType (unionTypeForFunctions stringType) False
      dataTerm (TermUnion $ Field "data" unitTerm)

  H.it "Primitive function references (when unsupported) become variant terms" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantUnion, TypeVariantRecord]
      concatType (unionTypeForFunctions stringType) False
      (primitive name) (TermUnion $ Field "primitive" $ stringValue name) -- Note: the function name is not dereferenced

  H.it "Projections (when unsupported) become variant terms" $
    QC.property $ \fname -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantUnion, TypeVariantRecord]
      exampleProjectionType (unionTypeForFunctions latLonType) False
      (projection fname) (TermUnion $ Field "projection" $ stringValue fname) -- Note: the field name is not dereferenced

  H.it "Nominal types (when unsupported) are dereferenced" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantAtomic]
      stringAliasType stringType False
      (stringValue s) (stringValue s)

termsAreAdaptedRecursively :: H.SpecWith ()
termsAreAdaptedRecursively = H.describe "Verify that the adapter descends into subterms and transforms them appropriately" $ do

  H.it "A list of int8's becomes a list of int32's" $
    QC.property $ \ints -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantList]
      listOfInt8sType listOfInt16sType False
      (TermList $ int8Value <$> ints)
      (TermList $ int16Value <$> ints)

  H.it "A list of sets of strings becomes a list of lists of strings" $
    QC.property $ \lists -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantList]
      listOfSetOfStringsType listOfListsOfStringsType False
      (TermList $ (\l -> TermSet $ S.fromList $ stringValue <$> l) <$> lists)
      (TermList $ (\l -> TermList $ stringValue <$> S.toList (S.fromList l)) <$> lists)

  H.it "A list of sets of element references becomes a list of lists of strings" $
    QC.property $ \names -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantList]
      listOfSetOfInt32ElementReferencesType listOfListsOfStringsType False
      (TermList $ (\l -> TermSet $ S.fromList $ TermElement <$> l) <$> names)
      (TermList $ (\l -> TermList $ stringValue <$> S.toList (S.fromList l)) <$> names)

adapterIsInformationPreserving :: H.SpecWith ()
adapterIsInformationPreserving = H.describe "Verify that the adapter is information preserving, i.e. that round-trips are no-ops" $ do

  H.it "Check strings (pass-through)" $
    QC.property $ \s -> roundTripIsNoop stringType (stringValue s)

  H.it "Check lists (pass-through)" $
    QC.property $ \strings -> roundTripIsNoop listOfStringsType (TermList $ stringValue <$> strings)

  H.it "Check sets (which map to lists)" $
    QC.property $ \strings -> roundTripIsNoop setOfStringsType (stringSet strings)

  H.it "Check element references (which map to strings)" $
    QC.property $ \name -> roundTripIsNoop int32ElementType (TermElement name)

  H.it "Check compareTo terms (which map to variants)" $
    QC.property $ \s -> roundTripIsNoop compareStringsType (compareTo $ stringValue s)

  H.it "Check data terms (which map to variants)" $
    roundTripIsNoop int32ElementDataType dataTerm `H.shouldBe` True

  H.it "Check primitive function references (which map to variants)" $
    QC.property $ \name -> roundTripIsNoop concatType (primitive name)

  H.it "Check projection terms (which map to variants)" $
    QC.property $ \fname -> roundTripIsNoop exampleProjectionType (projection fname)

  H.it "Check nominally typed terms (which pass through as instances of the aliased type)" $
    QC.property $ \s -> roundTripIsNoop stringAliasType (stringValue s)

roundTripIsNoop :: Type -> Term -> Bool
roundTripIsNoop typ term = (step stepOut term >>= step stepIn) == pure term
  where
    step = adapt typ

spec :: H.Spec
spec = do
  supportedConstructorsAreUnchanged
  unsupportedConstructorsAreModified
  termsAreAdaptedRecursively
  adapterIsInformationPreserving
