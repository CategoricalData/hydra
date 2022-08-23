module Hydra.Types.InferenceSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Basics
import Hydra.Types.Inference
import Hydra.TestUtils
import Hydra.TestData
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import qualified Data.Set as S


checkType :: Term (Meta, Type Meta, [Constraint Meta]) -> Type Meta -> H.Expectation
checkType term typ = typeAnn term `H.shouldBe` typ
  where
    typeAnn (TermAnnotated (Annotated _ (_, typ, _))) = typ

expectMonotype :: Term Meta -> Type Meta -> H.Expectation
expectMonotype term = expectPolytype term []

expectPolytype :: Term Meta-> [VariableType] -> Type Meta -> H.Expectation
expectPolytype term vars typ = do
    shouldSucceedWith
      (snd <$> inferType term)
      (TypeScheme vars typ)

checkApplicationTerms :: H.SpecWith ()
checkApplicationTerms = do
  H.describe "Check a few hand-picked application terms" $ do

    H.it "Check lambda applications" $ do
      expectMonotype
        (apply (lambda "x" (variable "x")) (string "foo"))
        Types.string

    H.it "Check data (delta) applications" $ do
      expectMonotype
        (apply delta (element $ Name "ArthurDent"))
        testTypePerson
--      expectMonotype
--        (apply termExpr describeType)
--        (Types.function (Types.nominal _Type) Types.string)

    H.it "Check mixed expressions with lambdas, constants, and primitive functions" $ do
      expectMonotype
        (lambda "x" $
          apply
            (apply (primitive _math_sub) (apply (apply (primitive _math_add) (variable "x")) (variable "x")))
            (int32 1))
        (Types.function Types.int32 Types.int32)

checkFunctionTerms :: H.SpecWith ()
checkFunctionTerms = do
  H.describe "Check a few hand-picked function terms" $ do

    H.it "Check lambdas" $ do
      expectPolytype
        (lambda "x" (variable "x"))
        [VariableType "v1"] (Types.function (Types.variable "v1") (Types.variable "v1"))
      expectPolytype
        (lambda "x" (int16 137))
        [VariableType "v1"] (Types.function (Types.variable "v1") Types.int16)

    H.it "Check 'compareTo' terms" $ do
      expectMonotype
        (compareTo $ optional (Just $ string "Betelgeuse"))
        (Types.function (Types.optional Types.string) Types.int8)
      expectPolytype
        (lambda "x" $ compareTo (variable "x"))
        [VariableType "v1"] (Types.function (Types.variable "v1") (Types.function (Types.variable "v1") Types.int8))

    H.it "Check projections" $ do
      expectMonotype
        (projection testTypePersonName (FieldName "firstName"))
        (Types.function testTypePerson Types.string)

    H.it "Check case statements" $ do
      expectMonotype
        (cases testTypeFoobarValueName [
          Field (FieldName "bool") (lambda "x" (boolean True)),
          Field (FieldName "string") (lambda "x" (boolean False)),
          Field (FieldName "unit") (lambda "x" (boolean False))])
        (Types.function testTypeFoobarValue Types.boolean)
      expectPolytype
        (cases testTypePersonOrSomethingName [
          Field (FieldName "person") (apply delta (element $ Name "firstName")),
          Field (FieldName "other") (lambda "x" (string "NONE"))])
        [VariableType "v1"] (Types.function
          (TypeUnion $ RowType testTypePersonOrSomethingName [
            Types.field "person" $ TypeRecord $ RowType testTypePersonName [
              Types.field "firstName" Types.string,
              Types.field "lastName" Types.string,
              Types.field "age" Types.int32],
            Types.field "other" $ Types.variable "v1"])
          Types.string)

checkIndividualTerms :: H.SpecWith ()
checkIndividualTerms = do
  H.describe "Check a few hand-picked terms" $ do

    H.it "Check literal values" $ do
      expectMonotype
        (int32 42)
        Types.int32
      expectMonotype
        (string "foo")
        Types.string
      expectMonotype
        (boolean False)
        Types.boolean
      expectMonotype
        (float64 42.0)
        Types.float64

    H.it "Check let terms" $ do
      expectPolytype
        (letTerm (Variable "x") (float32 42.0) (lambda "y" (lambda "z" (variable "x"))))
        [VariableType "v1", VariableType "v2"] (Types.function (Types.variable "v1") (Types.function (Types.variable "v2") Types.float32))

    H.it "Check elements" $ do
      expectMonotype
        (element $ Name "ArthurDent")
        (Types.element testTypePerson) -- Note: the resolved element type is the raw record type associated with "Person", not the nominal type "Person".
      expectMonotype
        (element $ Name "firstName")
        (Types.element (Types.function (Types.nominal $ Name "Person") Types.string))

    H.it "Check optionals" $ do
      expectMonotype
        (optional $ Just $ int32 42)
        (Types.optional Types.int32)
      expectPolytype
        (optional Nothing)
        [VariableType "v1"] (Types.optional $ Types.variable "v1")

    H.it "Check records" $ do
      expectMonotype
        (record latLonName [Field (FieldName "lat") $ float32 37.7749, Field (FieldName "lon") $ float32 $ negate 122.4194])
        (TypeRecord $ RowType latLonName [FieldType (FieldName "lat") Types.float32, FieldType (FieldName "lon") Types.float32])
--      expectPolytype
--        (lambda "lon" (record latLonName [Field (FieldName "lat") $ float32 37.7749, Field (FieldName "lon") $ variable "lon"]))
--        [VariableType "v1"] (Types.function (Types.variable "v1")
--          (TypeRecord $ RowType latLonName [FieldType (FieldName "lat") Types.float32, FieldType (FieldName "lon") $ Types.variable "v1"]))

    H.it "Check unions" $ do
      expectMonotype
        (union testTypeTimestampName $ Field (FieldName "unixTimeMillis") $ uint64 1638200308368)
        testTypeTimestamp

    H.it "Check sets" $ do
      expectMonotype
        (set $ S.fromList [boolean True])
        (Types.set Types.boolean)
      expectPolytype
        (set $ S.fromList [set S.empty])
        [VariableType "v1"] (Types.set $ Types.set $ Types.variable "v1")

    H.it "Check maps" $ do
      expectMonotype
        (mapTerm $ M.fromList [(string "firstName", string "Arthur"), (string "lastName", string "Dent")])
        (Types.map Types.string Types.string)
      expectPolytype
        (mapTerm M.empty)
        [VariableType "v1", VariableType "v2"] (Types.map (Types.variable "v1") (Types.variable "v2"))
      expectPolytype
        (lambda "x" (lambda "y" (mapTerm $ M.fromList
          [(variable "x", float64 0.1), (variable "y", float64 0.2)])))
        [VariableType "v1"] (Types.function (Types.variable "v1") (Types.function (Types.variable "v1") (Types.map (Types.variable "v1") Types.float64)))

    -- TODO: restore me, and add a case for a recursive nominal type -- e.g. MyList := () + (int, Mylist)
--    H.it "Check nominal (newtype) terms" $ do
--      expectMonotype
--        testDataArthur
--        (Types.nominal "Person")
--      expectMonotype
--        (lambda "x" (record [
--          Field "firstName" $ variable "x",
--          Field "lastName" $ variable "x",
--          Field "age" $ int32 42]))
--        (Types.function Types.string testTypePerson)

checkLists :: H.SpecWith ()
checkLists = do
  H.describe "Check a few hand-picked list terms" $ do

    H.it "Check list of strings" $ do
      expectMonotype
        (list [string "foo", string "bar"])
        (Types.list Types.string)
    H.it "Check list of lists of strings" $ do
      expectMonotype
        (list [list [string "foo"], list []])
        (Types.list $ Types.list Types.string)
    H.it "Check empty list" $ do
      expectPolytype
        (list [])
        [VariableType "v1"] (Types.list $ Types.variable "v1")
    H.it "Check list containing an empty list" $ do
      expectPolytype
        (list [list []])
        [VariableType "v1"] (Types.list $ Types.list $ Types.variable "v1")
    H.it "Check lambda producing a list of integers" $ do
      expectMonotype
        (lambda "x" (list [variable "x", int32 42]))
        (Types.function Types.int32 $ Types.list Types.int32)
    H.it "Check list with bound variables" $ do
      expectMonotype
        (lambda "x" (list [variable "x", string "foo", variable "x"]))
        (Types.function Types.string (Types.list Types.string))

checkLiterals :: H.SpecWith ()
checkLiterals = do
  H.describe "Check arbitrary literals" $ do

    H.it "Verify that type inference preserves the literal to literal type mapping" $
      QC.property $ \l -> expectMonotype
        (TermLiteral l)
        (Types.literal $ literalType l)

checkNominalTerms :: H.SpecWith ()
checkNominalTerms = do
  H.describe "Check nominal introductions and eliminations" $ do

    H.it "Check nominal introductions" $ do
      expectMonotype
        (nominal (Name "StringTypeAlias") $ string "foo")
        stringAliasType
      expectMonotype
        (lambda "v" $ nominal (Name "StringTypeAlias") $ variable "v")
        (Types.function (Standard.doc "An alias for the string type" Types.string) stringAliasType)

    H.it "Check nominal eliminations" $ do
      expectMonotype
        (eliminateNominal $ Name "StringTypeAlias")
        (Types.function stringAliasType (Standard.doc "An alias for the string type" Types.string))
      expectMonotype
        (apply (eliminateNominal $ Name "StringTypeAlias") (nominal (Name "StringTypeAlias") $ string "foo"))
        (Standard.doc "An alias for the string type" Types.string)

checkPrimitiveFunctions :: H.SpecWith ()
checkPrimitiveFunctions = do
  H.describe "Check a few hand-picked terms with primitive functions" $ do

    H.it "Check monomorphic primitive functions" $ do
      expectMonotype
        (primitive $ Name "hydra/lib/strings.length")
        (Types.function Types.string Types.int32)
      expectMonotype
        (primitive _math_sub)
        (Types.function Types.int32 (Types.function Types.int32 Types.int32))

    H.it "Check polymorphic primitive functions" $ do
      expectPolytype
        (lambda "els" (apply (primitive _lists_length) (apply (primitive _lists_concat) $ variable "els")))
        [VariableType "v1"] (Types.function (Types.list $ Types.list $ Types.variable "v1") Types.int32)

checkTypeAnnotations :: H.SpecWith ()
checkTypeAnnotations = do
  H.describe "Check that type annotations are added to terms and subterms" $ do

    H.it "Check literals" $
      QC.property $ \l -> do
        let term = TermLiteral l
        let term1 = fromFlow testContext (fst <$> inferType term)
        checkType term1 (Types.literal $ literalType l)

    H.it "Check lists of literals" $
      QC.property $ \l -> do
        let term = TermList [TermLiteral l]
        let term1 = fromFlow testContext (fst <$> inferType term)
        checkType term1 (Types.list $ Types.literal $ literalType l)
        let (TermAnnotated (Annotated (TermList [term2]) _)) = term1
        checkType term2 (Types.literal $ literalType l)

checkTypedTerms :: H.SpecWith ()
checkTypedTerms = do
  H.describe "Check that term/type pairs are consistent with type inference" $ do

    H.it "Check arbitrary typed terms" $
      QC.property $ \(TypedTerm typ term) -> expectMonotype term typ

spec :: H.Spec
spec = do
  checkApplicationTerms
  checkFunctionTerms
  checkIndividualTerms
  checkLists
  checkLiterals
  checkNominalTerms
  checkPrimitiveFunctions
  checkTypeAnnotations
--  checkTypedTerms
