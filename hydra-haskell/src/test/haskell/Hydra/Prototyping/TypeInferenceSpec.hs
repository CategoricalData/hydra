module Hydra.Prototyping.TypeInferenceSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.Basics
import Hydra.Prototyping.TypeInference
import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


expectMonotype :: Term Meta -> Type -> H.Expectation
expectMonotype term = expectPolytype term []

expectPolytype :: Term Meta-> [TypeVariable] -> Type -> H.Expectation
expectPolytype term vars typ = inferType testContext term `H.shouldBe` ResultSuccess (TypeScheme vars typ)

checkIndividualTerms :: H.SpecWith ()
checkIndividualTerms = do
  H.describe "Check a few hand-picked terms" $ do

    H.it "Check literal values" $ do
      expectMonotype
        (int32Value 42)
        int32Type
      expectMonotype
        (stringValue "foo")
        stringType
      expectMonotype
        (booleanValue False)
        booleanType
      expectMonotype
        (float64Value 42.0)
        float64Type

    H.it "Check lambdas" $ do
      expectPolytype
        (lambda "x" (variable "x"))
        ["v1"] (functionType (typeVariable "v1") (typeVariable "v1"))
      expectPolytype
        (lambda "x" (int16Value 137))
        ["v1"] (functionType (typeVariable "v1") int16Type)

    H.it "Check application terms" $ do
      expectMonotype
        (apply (lambda "x" (variable "x")) (stringValue "foo"))
        stringType

    H.it "Check let terms" $ do
      expectPolytype
        (letTerm "x" (float32Value 42.0) (lambda "y" (lambda "z" (variable "x"))))
        ["v1", "v2"] (functionType (typeVariable "v1") (functionType (typeVariable "v2") float32Type))

    H.it "Check elements" $ do
      expectMonotype
        (element "ArthurDent")
        (elementType testTypePerson) -- Note: the resolved element type is the raw record type associated with "Person", not the nominal type "Person".

    H.it "Check primitive functions" $ do
      expectMonotype
        (primitive "hydra/lib/strings.length")
        (functionType stringType int32Type)
      expectMonotype
        (primitive "hydra/lib/math/int32.sub")
        (functionType int32Type (functionType int32Type int32Type))        

    H.it "Check mixed expressions with lambdas, constants, and primitive functions" $ do
      expectMonotype
        (lambda "x" $
          apply
            (apply (primitive "hydra/lib/math/int32.sub") (apply (apply (primitive "hydra/lib/math/int32.add") (variable "x")) (variable "x")))
            (int32Value 1))     
        (functionType int32Type int32Type)

    H.it "Check optionals" $ do
      expectMonotype
        (optional $ Just $ int32Value 42)
        (optionalType int32Type)
      expectPolytype
        (optional Nothing)
        ["v1"] (optionalType $ typeVariable "v1")
        
    H.it "Check records" $ do
      expectMonotype
        (record [Field "lat" $ float64Value 37.7749, Field "lon" $ float64Value $ negate 122.4194])
        (recordType [FieldType "lat" float64Type, FieldType "lon" float64Type])
      expectPolytype
        (lambda "lon" (record [Field "lat" $ float64Value 37.7749, Field "lon" $ variable "lon"]))
        ["v1"] (functionType (typeVariable "v1") (recordType [FieldType "lat" float64Type, FieldType "lon" $ typeVariable "v1"]))
        
    H.it "Check unions" $ do
      -- Note that type inference only guesses the "top" type, even if this union "really" should have more than one field
      expectMonotype
        (union $ Field "lat" $ float64Value 37.7749)
        (unionType [FieldType "lat" float64Type])
        
    H.it "Check 'compareTo' terms" $ do
      expectMonotype
        (compareTo $ record [Field "fst" $ booleanValue True, Field "snd" $ stringValue "Betelgeuse"])
        (functionType (recordType [FieldType "fst" booleanType, FieldType "snd" stringType]) booleanType)      
      expectPolytype
        (lambda "x" $ compareTo (variable "x"))
        ["v1"] (functionType (typeVariable "v1") (functionType (typeVariable "v1") booleanType))

    H.it "Check lists" $ do
      expectMonotype
        (list [stringValue "foo", stringValue "bar"])
        (listType stringType)
      expectPolytype
        (list [])
        ["v1"] (listType $ typeVariable "v1")
      expectMonotype
        (lambda "x" (list [variable "x", int32Value 42]))
        (functionType int32Type $ listType int32Type)
        
checkLiterals :: H.SpecWith ()
checkLiterals = do
  H.describe "Check arbitrary literals" $ do

    H.it "Verify that type inference preserves the literal to literal type mapping" $
      QC.property $ \l -> expectMonotype
        (defaultTerm $ ExpressionLiteral l)
        (TypeLiteral $ literalType l)

checkTypedTerms :: H.SpecWith ()
checkTypedTerms = do
  H.describe "Check that term/type pairs are consistent with type inference" $ do

    H.it "Check arbitrary typed terms" $
      QC.property $ \(TypedTerm typ term) -> expectMonotype term typ

spec :: H.Spec
spec = do
  checkIndividualTerms
  checkLiterals
--  checkTypedTerms
