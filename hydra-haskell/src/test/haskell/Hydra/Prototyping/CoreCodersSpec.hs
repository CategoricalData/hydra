module Hydra.Prototyping.CoreCodersSpec where

import Hydra.Core
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.CoreDecoding
import Hydra.Prototyping.CoreEncoding

import Hydra.ArbitraryCore
import Hydra.TestGraph

import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


individualEncoderTestCases = do
  H.describe "Individual encoder test cases" $ do
    
    H.it "string atomic type" $ do
      (encodeAtomicType $ AtomicTypeString) `H.shouldBe` (unitVariant _AtomicType_string)

    H.it "string type" $ do
      (encodeType stringType) `H.shouldBe` (variant _Term_atomic $ unitVariant _AtomicType_string)

    H.it "int32 type" $ do
      (encodeType int32Type) `H.shouldBe`
        (variant _Term_atomic $ variant _AtomicType_integer $ unitVariant _IntegerType_int32)

    H.it "record type" $ do
      (encodeType $ TypeRecord [FieldType "something" stringType, FieldType "nothing" unitType]) `H.shouldBe`
        (variant _Type_record $ TermList [
          (TermRecord [
            Field _FieldType_name $ stringValue "something",
            Field _FieldType_type $ variant _Type_atomic $ unitVariant _AtomicType_string]),
          (TermRecord [
            Field _FieldType_name $ stringValue "nothing",
            Field _FieldType_type $ variant _Type_record $ TermList []])])

individualDecoderTestCases = do
  H.describe "Individual decoder test cases" $ do
    
    H.it "float32 atomic type" $ do
      (decodeAtomicType $ variant _AtomicType_float $ unitVariant _FloatType_float32) `H.shouldBe`
        (Right $ AtomicTypeFloat $ FloatTypeFloat32)

    H.it "float32 type" $ do
      (decodeType $ variant _Type_atomic $ variant _AtomicType_float $ unitVariant _FloatType_float32) `H.shouldBe`
        (Right float32Type)
        
    H.it "union type" $ do
      (decodeType $ variant _Type_union $ TermList [
        (TermRecord [
          Field _FieldType_name $ stringValue "left",
          Field _FieldType_type $ variant _Type_atomic $ variant _AtomicType_integer $ unitVariant _IntegerType_int64]),
        (TermRecord [
          Field _FieldType_name $ stringValue "right",
          Field _FieldType_type $ variant _Type_atomic $ variant _AtomicType_float $ unitVariant _FloatType_float64])])
        `H.shouldBe` (Right $ TypeUnion [FieldType "left" int64Type, FieldType "right" float64Type])
        
decodeInvalidTerms = do
  H.describe "Decode invalid terms" $ do
      
    H.it "Try to decode a term with wrong fields for Type" $ do
      E.isLeft (decodeType $ variant "unknownField" $ TermList []) `H.shouldBe` True

    H.it "Try to decode an incomplete representation of a Type" $ do
      E.isLeft (decodeType $ variant _Type_atomic $ unitVariant _AtomicType_integer) `H.shouldBe` True
      
testRoundTripsFromType = do
  H.describe "Check that encoding, then decoding random types is a no-op" $ do
    
    H.it "Try random types" $
      QC.property $ \typ -> ((decodeType $ encodeType typ) == Right typ)

spec :: H.Spec
spec = do
  individualEncoderTestCases
  individualDecoderTestCases
  decodeInvalidTerms
  testRoundTripsFromType
