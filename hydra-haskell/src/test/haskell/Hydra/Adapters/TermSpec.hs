module Hydra.Adapters.TermSpec where

import Hydra.Adapter
import Hydra.Adapters.Term
import Hydra.Adapters.Utils
import Hydra.Adapters.UtilsEtc
import Hydra.Basics
import Hydra.Core
import Hydra.CoreLanguage
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras
import Hydra.Steps
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import Hydra.TestData
import Hydra.TestUtils
import Hydra.ArbitraryCore (untyped)

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Set as S
import qualified Data.Maybe as Y


constraintsAreAsExpected :: H.SpecWith ()
constraintsAreAsExpected = H.describe "Verify that the language constraints include/exclude the appropriate types" $ do

    H.it "int16 and int32 are supported in the test context" $ do
      typeIsSupported (context [TypeVariantLiteral]) Types.int16 `H.shouldBe` True
      typeIsSupported (context [TypeVariantLiteral]) Types.int32 `H.shouldBe` True

    H.it "int8 and bigint are unsupported in the test context" $ do
      typeIsSupported (context [TypeVariantLiteral]) Types.int8 `H.shouldBe` False
      typeIsSupported (context [TypeVariantLiteral]) Types.bigint `H.shouldBe` False

    H.it "Records are supported, but unions are not" $ do
      typeIsSupported (context [TypeVariantLiteral, TypeVariantRecord]) latLonType `H.shouldBe` True
      typeIsSupported (context [TypeVariantLiteral, TypeVariantRecord]) stringOrIntType `H.shouldBe` False

    H.it "Records are supported if and only if each of their fields are supported" $ do
      typeIsSupported (context [TypeVariantLiteral, TypeVariantRecord])
        (TypeRecord [Types.field "first" Types.string, Types.field "second" Types.int16])
        `H.shouldBe` True
      typeIsSupported (context [TypeVariantLiteral, TypeVariantRecord])
        (TypeRecord [Types.field "first" Types.string, Types.field "second" Types.int8])
        `H.shouldBe` False

    H.it "Lists are supported if the list element type is supported" $ do
      typeIsSupported (context [TypeVariantLiteral, TypeVariantList]) listOfStringsType `H.shouldBe` True
      typeIsSupported (context [TypeVariantLiteral, TypeVariantList]) listOfListsOfStringsType `H.shouldBe` True
      typeIsSupported (context [TypeVariantLiteral, TypeVariantList]) listOfSetOfStringsType `H.shouldBe` False

  where
    context = languageConstraints . adapterContextTarget . termTestContext

supportedConstructorsAreUnchanged :: H.SpecWith ()
supportedConstructorsAreUnchanged = H.describe "Verify that supported term constructors are unchanged" $ do

  H.it "Strings (and other supported atomic values) pass through without change" $
    QC.property $ \b -> checkTermAdapter
      [TypeVariantLiteral]
      Types.string
      Types.string
      False
      (stringValue b)
      (stringValue b)

  H.it "Lists (when supported) pass through without change" $
    QC.property $ \strings -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantList]
      listOfStringsType
      listOfStringsType
      False
      (list $ stringValue <$> strings)
      (list $ stringValue <$> strings)

  H.it "Maps (when supported) pass through without change" $
    QC.property $ \keyvals -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantMap]
      mapOfStringsToIntsType
      mapOfStringsToIntsType
      False
      (makeMap keyvals)
      (makeMap keyvals)

  H.it "Optionals (when supported) pass through without change" $
    QC.property $ \mi -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantOptional]
      optionalInt8Type
      optionalInt16Type
      False
      (optional $ int8Value <$> mi)
      (optional $ int16Value <$> mi)

  H.it "Records (when supported) pass through without change" $
    QC.property $ \a1 a2 -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantRecord]
      (TypeRecord [Types.field "first" Types.string, Types.field "second" Types.int8])
      (TypeRecord [Types.field "first" Types.string, Types.field "second" Types.int16])
      False
      (record [Field "first" $ stringValue a1, Field "second" $ int8Value a2])
      (record [Field "first" $ stringValue a1, Field "second" $ int16Value a2])

  H.it "Unions (when supported) pass through without change" $
    QC.property $ \int -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantUnion]
      stringOrIntType
      stringOrIntType
      False
      (variant "right" $ int32Value int)
      (variant "right" $ int32Value int)

  H.it "Sets (when supported) pass through without change" $
    QC.property $ \strings -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantSet]
      setOfStringsType
      setOfStringsType
      False
      (stringSet strings)
      (stringSet strings)

  H.it "Element references (when supported) pass through without change" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantElement]
      int32ElementType
      int32ElementType
      False
      (element name)
      (element name)

  H.it "CompareTo terms (when supported) pass through without change" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantFunction]
      compareStringsType
      compareStringsType
      False
      (compareTo $ stringValue s)
      (compareTo $ stringValue s)

  H.it "Data terms (when supported) pass through without change" $
    QC.property $ \() -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantFunction, TypeVariantElement]
      int32ElementDataType
      int32ElementDataType
      False
      dataTerm
      dataTerm

  H.it "Primitive function references (when supported) pass through without change" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantFunction]
      concatType
      concatType
      False
      (primitive name)
      (primitive name)

  H.it "Projections (when supported) pass through without change" $
    QC.property $ \fname -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantFunction, TypeVariantRecord]
      exampleProjectionType
      exampleProjectionType
      False
      (projection fname)
      (projection fname)

  H.it "Nominal types (when supported) pass through without change" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantNominal]
      stringAliasType
      stringAliasType
      False
      (stringValue s)
      (stringValue s)

unsupportedConstructorsAreModified :: H.SpecWith ()
unsupportedConstructorsAreModified = H.describe "Verify that unsupported term constructors are changed in the expected ways" $ do

  H.it "Sets (when unsupported) become lists" $
    QC.property $ \strings -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantList]
      setOfStringsType
      listOfStringsType
      False
      (stringSet strings)
      (stringList $ S.toList strings)

  H.it "Element references (when unsupported) become strings" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantLiteral]
      int32ElementType
      Types.string
      False
      (element name)
      (stringValue name) -- Note: the element name is not dereferenced

  H.it "CompareTo terms (when unsupported) become variant terms" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantUnion, TypeVariantRecord]
      compareStringsType
      (unionTypeForFunctions Types.string)
      False
      (compareTo $ stringValue s)
      (nominalUnion testContext _Function $ Field "compareTo" $ stringValue s)

  H.it "Data terms (when unsupported) become variant terms" $
    QC.property $ \() -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantUnion, TypeVariantRecord]
      int32ElementDataType
      (unionTypeForFunctions Types.string)
      False
      dataTerm
      (nominalUnion testContext _Function $ Field "data" unitTerm)

  H.it "Optionals (when unsupported) become lists" $
    QC.property $ \ms -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantList]
      (TypeOptional Types.string)
      (TypeList Types.string)
      False
      (optional $ stringValue <$> ms)
      (list $ Y.maybe [] (\s -> [stringValue s]) ms)

  H.it "Primitive function references (when unsupported) become variant terms" $
    QC.property $ \name -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantUnion, TypeVariantRecord]
      concatType
      (unionTypeForFunctions Types.string)
      False
      (primitive name)
      (nominalUnion testContext _Function $ Field "primitive" $ stringValue name) -- Note: the function name is not dereferenced

  H.it "Projections (when unsupported) become variant terms" $
    QC.property $ \fname -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantUnion, TypeVariantRecord]
      exampleProjectionType
      (unionTypeForFunctions testTypePerson)
      False
      (projection fname)
      (nominalUnion testContext _Function $ Field "projection" $ stringValue fname) -- Note: the field name is not dereferenced

  H.it "Nominal types (when unsupported) are dereferenced" $
    QC.property $ \s -> checkTermAdapter
      [TypeVariantLiteral]
      stringAliasType
      Types.string
      False
      (stringValue s)
      (stringValue s)

  H.it "Unions (when unsupported) become records" $
    QC.property $ \i -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantOptional, TypeVariantRecord]
      eitherStringOrInt8Type
      (TypeRecord [Types.field "context" Types.string, Types.field "record" (TypeRecord [
        Types.field "left" $ TypeOptional Types.string,
        Types.field "right" $ TypeOptional Types.int16])])
      False
      (union $ Field "right" $ int8Value i)
      (record [
        Field "context" $ stringValue untyped,
        Field "record" (record [
          Field "left" $ optional Nothing,
          Field "right" $ optional $ Just $ int16Value i])])

termsAreAdaptedRecursively :: H.SpecWith ()
termsAreAdaptedRecursively = H.describe "Verify that the adapter descends into subterms and transforms them appropriately" $ do

  H.it "A list of int8's becomes a list of int32's" $
    QC.property $ \ints -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantList]
      listOfInt8sType
      listOfInt16sType
      False
      (list $ int8Value <$> ints)
      (list $ int16Value <$> ints)

  H.it "A list of sets of strings becomes a list of lists of strings" $
    QC.property $ \lists -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantList]
      listOfSetOfStringsType
      listOfListsOfStringsType
      False
      (list $ (\l -> set $ S.fromList $ stringValue <$> l) <$> lists)
      (list $ (\l -> list $ stringValue <$> S.toList (S.fromList l)) <$> lists)

  H.it "A list of sets of element references becomes a list of lists of strings" $
    QC.property $ \names -> checkTermAdapter
      [TypeVariantLiteral, TypeVariantList]
      listOfSetOfInt32ElementReferencesType
      listOfListsOfStringsType
      False
      (list $ (\l -> set $ S.fromList $ element <$> l) <$> names)
      (list $ (\l -> list $ stringValue <$> S.toList (S.fromList l)) <$> names)

roundTripsPreserveSelectedTypes :: H.SpecWith ()
roundTripsPreserveSelectedTypes = H.describe "Verify that the adapter is information preserving, i.e. that round-trips are no-ops" $ do

  H.it "Check strings (pass-through)" $
    QC.property $ \s -> roundTripIsNoop Types.string (stringValue s)

  H.it "Check lists (pass-through)" $
    QC.property $ \strings -> roundTripIsNoop listOfStringsType (list $ stringValue <$> strings)

  H.it "Check sets (which map to lists)" $
    QC.property $ \strings -> roundTripIsNoop setOfStringsType (stringSet strings)

  H.it "Check element references (which map to strings)" $
    QC.property $ \name -> roundTripIsNoop int32ElementType (element name)

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

roundTripsPreserveArbitraryTypes :: H.SpecWith ()
roundTripsPreserveArbitraryTypes = H.describe "Verify that the adapter is information preserving for arbitrary typed terms" $ do

  H.it "Check arbitrary type/term pairs" $
    QC.property $ \(TypedTerm typ term) -> roundTripIsNoop typ term

fieldAdaptersAreAsExpected :: H.SpecWith ()
fieldAdaptersAreAsExpected = H.describe "Check that field adapters are as expected" $ do

  H.it "An int8 field becomes an int16 field" $
    QC.property $ \i -> checkFieldAdapter
      [TypeVariantLiteral, TypeVariantRecord]
      (Types.field "second" Types.int8)
      (Types.field "second" Types.int16)
      False
      (Field "second" $ int8Value i)
      (Field "second" $ int16Value i)

roundTripIsNoop :: Type -> Term Meta -> Bool
roundTripIsNoop typ term = (step stepOut term >>= step stepIn) == pure term
  where
    step = adapt typ

    -- Use a YAML-like language (but supporting unions) as the default target language
    testLanguage :: Language
    testLanguage = Language "hydra/test" $ Language_Constraints {
      languageConstraintsLiteralVariants = S.fromList [
        LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
      languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
      languageConstraintsFunctionVariants = S.empty,
      languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
      languageConstraintsTermVariants = S.fromList termVariants,
      languageConstraintsTypeVariants = S.fromList [
        TypeVariantLiteral, TypeVariantList, TypeVariantMap, TypeVariantRecord, TypeVariantUnion],
      languageConstraintsTypes = \typ -> case typ of
        TypeOptional (TypeOptional _) -> False
        _ -> True }

    transContext = AdapterContext testContext hydraCoreLanguage testLanguage

    -- Note: in a real application, you wouldn't create the adapter just to use it once;
    --       it should be created once, then applied to many terms.
    adapt typ dir term = do
      ad <- qualifiedToResult $ termAdapter transContext typ
      dir (adapterStep ad) term

spec :: H.Spec
spec = do
  constraintsAreAsExpected
  supportedConstructorsAreUnchanged
  unsupportedConstructorsAreModified
  termsAreAdaptedRecursively
  roundTripsPreserveSelectedTypes
  roundTripsPreserveArbitraryTypes
  fieldAdaptersAreAsExpected
