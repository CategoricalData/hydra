module Hydra.Types.InferenceSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Basics
import Hydra.Types.Inference
import Hydra.TestUtils
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import qualified Data.Set as S


checkType :: Data (Meta, Type Meta, [Constraint Meta]) -> Type Meta -> H.Expectation
checkType term typ = typeAnn term `H.shouldBe` typ
  where
    typeAnn (Data _ (_, typ, _)) = typ

expectMonotype :: Data Meta -> Type Meta -> H.Expectation
expectMonotype term = expectPolytype term []

expectPolytype :: Data Meta-> [TypeVariable] -> Type Meta -> H.Expectation
expectPolytype term vars typ = do
    let result = inferType testContext term
    snd <$> result `H.shouldBe` ResultSuccess (TypeScheme vars typ)
--    if (snd <$> result) == ResultSuccess (TypeScheme vars typ)
--      then True `H.shouldBe` True
--      else "foo" `H.shouldBe` (show result)

checkApplicationTerms :: H.SpecWith ()
checkApplicationTerms = do
  H.describe "Check a few hand-picked application terms" $ do

    H.it "Check lambda applications" $ do
      expectMonotype
        (apply (lambda "x" (variable "x")) (stringValue "foo"))
        Types.string

    H.it "Check data (delta) applications" $ do
      expectMonotype
        (apply delta (element "ArthurDent"))
        testTypePerson
--      expectMonotype
--        (apply dataTerm describeType)
--        (Types.function (Types.nominal _Type) Types.string)

    H.it "Check mixed expressions with lambdas, constants, and primitive functions" $ do
      expectMonotype
        (lambda "x" $
          apply
            (apply (primitive _math_sub) (apply (apply (primitive _math_add) (variable "x")) (variable "x")))
            (int32Value 1))
        (Types.function Types.int32 Types.int32)

checkFunctionTerms :: H.SpecWith ()
checkFunctionTerms = do
  H.describe "Check a few hand-picked function terms" $ do

    H.it "Check lambdas" $ do
      expectPolytype
        (lambda "x" (variable "x"))
        ["v1"] (Types.function (Types.variable "v1") (Types.variable "v1"))
      expectPolytype
        (lambda "x" (int16Value 137))
        ["v1"] (Types.function (Types.variable "v1") Types.int16)

    H.it "Check 'compareTo' terms" $ do
      expectMonotype
        (compareTo $ record [Field "fst" $ booleanValue True, Field "snd" $ stringValue "Betelgeuse"])
        (Types.function (Types.record [FieldType "fst" Types.boolean, FieldType "snd" Types.string]) Types.int8)
      expectPolytype
        (lambda "x" $ compareTo (variable "x"))
        ["v1"] (Types.function (Types.variable "v1") (Types.function (Types.variable "v1") Types.int8))

    H.it "Check projections" $ do
      expectMonotype
        (nominalProjection testContext "Person" "firstName" Types.string)
        (Types.function (Types.nominal "Person") Types.string)

    H.it "Check case statements" $ do
      expectPolytype
        (cases [
          Field "left" (lambda "x" (booleanValue True)),
          Field "right" (lambda "x" (booleanValue False))])
        ["v1", "v2"] (Types.function
          (Types.union [
            FieldType "left" (Types.variable "v1"),
            FieldType "right" (Types.variable "v2")])
          Types.boolean)
      expectPolytype
        (cases [
          Field "person" (apply delta (element "firstName")),
          Field "other" (lambda "x" (stringValue "NONE"))])
        ["v1"] (Types.function
          (Types.union [
            FieldType "person" (Types.nominal "Person"),
            FieldType "other" (Types.variable "v1")])
          Types.string)

checkIndividualTerms :: H.SpecWith ()
checkIndividualTerms = do
  H.describe "Check a few hand-picked terms" $ do

    H.it "Check literal values" $ do
      expectMonotype
        (int32Value 42)
        Types.int32
      expectMonotype
        (stringValue "foo")
        Types.string
      expectMonotype
        (booleanValue False)
        Types.boolean
      expectMonotype
        (float64Value 42.0)
        Types.float64

    H.it "Check let terms" $ do
      expectPolytype
        (letData "x" (float32Value 42.0) (lambda "y" (lambda "z" (variable "x"))))
        ["v1", "v2"] (Types.function (Types.variable "v1") (Types.function (Types.variable "v2") Types.float32))

    H.it "Check elements" $ do
      expectMonotype
        (element "ArthurDent")
        (Types.element testTypePerson) -- Note: the resolved element type is the raw record type associated with "Person", not the nominal type "Person".
      expectMonotype
        (element "firstName")
        (Types.element (Types.function (Types.nominal "Person") Types.string))

    H.it "Check optionals" $ do
      expectMonotype
        (optional $ Just $ int32Value 42)
        (Types.optional Types.int32)
      expectPolytype
        (optional Nothing)
        ["v1"] (Types.optional $ Types.variable "v1")

    H.it "Check records" $ do
      expectMonotype
        (record [Field "lat" $ float64Value 37.7749, Field "lon" $ float64Value $ negate 122.4194])
        (Types.record [FieldType "lat" Types.float64, FieldType "lon" Types.float64])
      expectPolytype
        (lambda "lon" (record [Field "lat" $ float64Value 37.7749, Field "lon" $ variable "lon"]))
        ["v1"] (Types.function (Types.variable "v1") (Types.record [FieldType "lat" Types.float64, FieldType "lon" $ Types.variable "v1"]))

    H.it "Check unions" $ do
      expectMonotype
        (nominalUnion testContext "Timestamp" $ Field "unixTimeMillis" $ uint64Value 1638200308368)
        (Types.nominal "Timestamp")

    H.it "Check sets" $ do
      expectMonotype
        (set $ S.fromList [booleanValue True])
        (Types.set Types.boolean)
      expectPolytype
        (set $ S.fromList [set S.empty])
        ["v1"] (Types.set $ Types.set $ Types.variable "v1")

    H.it "Check maps" $ do
      expectMonotype
        (mapData $ M.fromList [(stringValue "firstName", stringValue "Arthur"), (stringValue "lastName", stringValue "Dent")])
        (Types.map Types.string Types.string)
      expectPolytype
        (mapData M.empty)
        ["v1", "v2"] (Types.map (Types.variable "v1") (Types.variable "v2"))
      expectPolytype
        (lambda "x" (lambda "y" (mapData $ M.fromList
          [(variable "x", float64Value 0.1), (variable "y", float64Value 0.2)])))
        ["v1"] (Types.function (Types.variable "v1") (Types.function (Types.variable "v1") (Types.map (Types.variable "v1") Types.float64)))

    -- TODO: restore me, and add a case for a recursive nominal type -- e.g. MyList := () + (int, Mylist)
--    H.it "Check nominal (newtype) terms" $ do
--      expectMonotype
--        testDataArthur
--        (Types.nominal "Person")
--      expectMonotype
--        (lambda "x" (record [
--          Field "firstName" $ variable "x",
--          Field "lastName" $ variable "x",
--          Field "age" $ int32Value 42]))
--        (Types.function Types.string testTypePerson)

checkLists :: H.SpecWith ()
checkLists = do
  H.describe "Check a few hand-picked list terms" $ do

    H.it "Check list of strings" $ do
      expectMonotype
        (list [stringValue "foo", stringValue "bar"])
        (Types.list Types.string)
    H.it "Check list of lists of strings" $ do
      expectMonotype
        (list [list [stringValue "foo"], list []])
        (Types.list $ Types.list Types.string)
    H.it "Check empty list" $ do
      expectPolytype
        (list [])
        ["v1"] (Types.list $ Types.variable "v1")
    H.it "Check list containing an empty list" $ do
      expectPolytype
        (list [list []])
        ["v1"] (Types.list $ Types.list $ Types.variable "v1")
    H.it "Check lambda producing a list of integers" $ do
      expectMonotype
        (lambda "x" (list [variable "x", int32Value 42]))
        (Types.function Types.int32 $ Types.list Types.int32)
    H.it "Check list with bound variables" $ do
      expectMonotype
        (lambda "x" (list [variable "x", stringValue "foo", variable "x"]))
        (Types.function Types.string (Types.list Types.string))

checkLiterals :: H.SpecWith ()
checkLiterals = do
  H.describe "Check arbitrary literals" $ do

    H.it "Verify that type inference preserves the literal to literal type mapping" $
      QC.property $ \l -> expectMonotype
        (defaultData $ DataTermLiteral l)
        (Types.literal $ literalType l)

checkPrimitiveFunctions :: H.SpecWith ()
checkPrimitiveFunctions = do
  H.describe "Check a few hand-picked terms with primitive functions" $ do

    H.it "Check monomorphic primitive functions" $ do
      expectMonotype
        (primitive "hydra/lib/strings.length")
        (Types.function Types.string Types.int32)
      expectMonotype
        (primitive _math_sub)
        (Types.function Types.int32 (Types.function Types.int32 Types.int32))

    H.it "Check polymorphic primitive functions" $ do
      expectPolytype
        (lambda "els" (apply (primitive _lists_length) (apply (primitive _lists_concat) $ variable "els")))
        ["v1"] (Types.function (Types.list $ Types.list $ Types.variable "v1") Types.int32)

checkTypeAnnotations :: H.SpecWith ()
checkTypeAnnotations = do
  H.describe "Check that type annotations are added to terms and subterms" $ do

    H.it "Check literals" $
      QC.property $ \l -> do
        let term = defaultData $ DataTermLiteral l
        let (ResultSuccess term1) = fst <$> inferType testContext term
        checkType term1 (Types.literal $ literalType l)

    H.it "Check lists of literals" $
      QC.property $ \l -> do
        let term = defaultData $ DataTermList [defaultData $ DataTermLiteral l]
        let (ResultSuccess term1) = fst <$> inferType testContext term
        checkType term1 (Types.list $ Types.literal $ literalType l)
        let (DataTermList [term2]) = dataTerm term1
        checkType term2 (Types.literal $ literalType l)

checkTypedDatas :: H.SpecWith ()
checkTypedDatas = do
  H.describe "Check that term/type pairs are consistent with type inference" $ do

    H.it "Check arbitrary typed terms" $
      QC.property $ \(TypedData typ term) -> expectMonotype term typ

spec :: H.Spec
spec = do
  checkApplicationTerms
  checkFunctionTerms
  checkIndividualTerms
  checkLists
  checkLiterals
  checkPrimitiveFunctions
  checkTypeAnnotations
--  checkTypedDatas
