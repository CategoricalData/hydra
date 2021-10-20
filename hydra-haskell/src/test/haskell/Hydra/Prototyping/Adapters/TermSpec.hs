module Hydra.Prototyping.Adapters.TermSpec where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Adapters.Utils
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Extras
import Hydra.Prototyping.Steps
import Hydra.Adapter

import Hydra.TestData
import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Set as S
import qualified Data.Maybe as Y


constraintsAreAsExpected :: H.SpecWith ()
constraintsAreAsExpected = H.describe "Verify that the language constraints include/exclude the appropriate types" $ do

    H.it "int16 and int32 are supported in the test context" $ do
      typeIsSupported (context [TypeVariantAtomic]) int16Type `H.shouldBe` True
      typeIsSupported (context [TypeVariantAtomic]) int32Type `H.shouldBe` True
      
    H.it "int8 and bigint are unsupported in the test context" $ do
      typeIsSupported (context [TypeVariantAtomic]) int8Type `H.shouldBe` False
      typeIsSupported (context [TypeVariantAtomic]) bigintType `H.shouldBe` False
      
    H.it "Records are supported, but unions are not" $ do
      typeIsSupported (context [TypeVariantAtomic, TypeVariantRecord]) latLonType `H.shouldBe` True
      typeIsSupported (context [TypeVariantAtomic, TypeVariantRecord]) stringOrIntType `H.shouldBe` False

    H.it "Records are supported if and only if each of their fields are supported" $ do
      typeIsSupported (context [TypeVariantAtomic, TypeVariantRecord])
        (TypeRecord [FieldType "first" stringType, FieldType "second" int16Type])
        `H.shouldBe` True
      typeIsSupported (context [TypeVariantAtomic, TypeVariantRecord])
        (TypeRecord [FieldType "first" stringType, FieldType "second" int8Type])
        `H.shouldBe` False

    H.it "Lists are supported if the list element type is supported" $ do
      typeIsSupported (context [TypeVariantAtomic, TypeVariantList]) listOfStringsType `H.shouldBe` True
      typeIsSupported (context [TypeVariantAtomic, TypeVariantList]) listOfListsOfStringsType `H.shouldBe` True
      typeIsSupported (context [TypeVariantAtomic, TypeVariantList]) listOfSetOfStringsType `H.shouldBe` False

  where
    context = languageConstraints . adapterContextTarget . termTestContext

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

  H.it "Optionals (when supported) pass through without change" $
    QC.property $ \mi -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantOptional]
      optionalInt8Type optionalInt16Type False
      (TermOptional $ int8Value <$> mi) (TermOptional $ int16Value <$> mi)

  H.it "Records (when supported) pass through without change" $
    QC.property $ \a1 a2 -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantRecord]
      (TypeRecord [FieldType "first" stringType, FieldType "second" int8Type])
      (TypeRecord [FieldType "first" stringType, FieldType "second" int16Type])
      False
      (TermRecord [Field "first" $ stringValue a1, Field "second" $ int8Value a2])
      (TermRecord [Field "first" $ stringValue a1, Field "second" $ int16Value a2])

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

  H.it "Optionals (when unsupported) become unions" $
    QC.property $ \ms -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantRecord, TypeVariantUnion]
      optionalStringType (TypeUnion [FieldType "nothing" unitType, FieldType "just" stringType]) False
      (TermOptional $ stringValue <$> ms)
      (TermUnion $ Y.maybe (Field "nothing" unitTerm) (Field "just" . stringTerm) ms)

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

  H.it "Unions (when unsupported) become records" $
    QC.property $ \i -> checkTermAdapter
      [TypeVariantAtomic, TypeVariantOptional, TypeVariantRecord]
      eitherStringOrInt8Type
      (TypeRecord [
        FieldType "left" $ TypeOptional stringType,
        FieldType "right" $ TypeOptional int16Type]) False
      (TermUnion $ Field "right" $ int8Value i)
      (TermRecord [
        Field "left" $ TermOptional Nothing,
        Field "right" $ TermOptional $ Just $ int16Value i])

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

fieldAdaptersAreAsExpected :: H.SpecWith ()
fieldAdaptersAreAsExpected = H.describe "Check that field adapters are as expected" $ do

  H.it "An int8 field becomes an int16 field" $
    QC.property $ \i -> checkFieldAdapter
      [TypeVariantAtomic, TypeVariantRecord]
      (FieldType "second" int8Type)
      (FieldType "second" int16Type)
      False
      (Field "second" $ int8Value i)
      (Field "second" $ int16Value i)
      
roundTripIsNoop :: Type -> Term -> Bool
roundTripIsNoop typ term = (step stepOut term >>= step stepIn) == pure term
  where
    step = adapt typ

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

spec :: H.Spec
spec = do
  constraintsAreAsExpected
  supportedConstructorsAreUnchanged
  unsupportedConstructorsAreModified
  termsAreAdaptedRecursively
  adapterIsInformationPreserving
  fieldAdaptersAreAsExpected
