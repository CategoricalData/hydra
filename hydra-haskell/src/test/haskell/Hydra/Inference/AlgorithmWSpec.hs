{-# LANGUAGE OverloadedStrings #-}

module Hydra.Inference.AlgorithmWSpec where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.TestUtils
import Hydra.TestData
import qualified Hydra.Dsl.Expect as Expect
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Staging.AlgorithmWBridge as W
import Hydra.Lib.Io


import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

t0 = "t0"
t1 = "t1"
t2 = "t2"
t3 = "t3"
t0T = Types.var t0
t1T = Types.var t1
t2T = Types.var t2
t3T = Types.var t3

testHydraContext = W.HydraContext $ graphPrimitives testGraph

inferType :: Term -> IO (Term, TypeScheme)
inferType = W.termToInferredTerm testHydraContext

expectType :: Term -> TypeScheme -> H.Expectation
expectType term ts = H.shouldReturn (snd <$> inferType term) ts

expectMonotype :: Term -> Type -> H.Expectation
expectMonotype term typ = expectType term $ TypeScheme [] typ

expectPolytype :: Term -> [String] -> Type -> H.Expectation
expectPolytype term vars typ = expectType term $ TypeScheme (Name <$> vars) typ

expectFailure :: Term -> H.Expectation
expectFailure term = H.shouldThrow (inferType term) H.anyException

-- Placeholders for the primitives in @wisnesky's test cases; they are not necessarily the same functions,
-- but they have the same types.
primPred = primitive _math_neg
primSucc = primitive _math_neg

-- @wisnesky's original Algorithm W test cases, modified so as to normalize type variables
checkAlgorithmW :: H.SpecWith ()
checkAlgorithmW = H.describe "Check System F syntax" $ do
  --Untyped input:
  --	(\x. x)
  --System F type:
  -- 	(v0 -> v0)
  H.it "#0" $ expectPolytype
    (lambda "x" $ var "x")
    [t0] $ tFun t0T t0T

  --Untyped input:
  --	letrecs foo = (\x. x)
  --		in 42
  --System F type:
  -- 	Nat
  H.it "#1" $ expectMonotype
    (int32 32 `with` [
      "foo">: lambda "x" $ var "x"])
    tInt32

  --Untyped input:
  --	let f = (\x. x) in (f 0)
  --System F type:
  -- 	Nat
  H.it "testLet3" $ expectMonotype
    ((var "f" @@ int32 0) `with` [
      "f">: lambda "x" $ var "x"])
    tInt32

  --Untyped input:
  --	let f = ((\x. x) 0) in f
  --System F type:
  -- 	Nat
  H.it "testLet4" $ expectMonotype
    (var "f" `with` [
      "f">: (lambda "x" $ var "x") @@ int32 0])
    tInt32


-- TODO
--testAdt3 = TestCase "testAdt3" $ Abs "z" $ ExprConst (Fold "List")
--  @@ Var "z"
--  @@ ExprConst (Con "Nil")
--  @@ ExprConst (Con "Cons")
--
--foldl :: TTerm ((b -> a -> b) -> b -> [a] -> b)
--cons :: TTerm (a -> [a] -> [a])
--
--[testAdt3]
--Untyped input:
--	(\z. (fold_List z Nil Cons))
--Type inferred by Hindley-Milner:
--	(List v6 -> List v6)
--
--System F translation:
--	(\z:List v6 . ((fold_List [List v6 ,v6]) z (Nil [v6]) (Cons [v6])))
--  H.it "testAdt3" $ expectPolytype
--    ()
--    [t0] (tFun t0 t0)




  --Untyped input:
  --	let sng = (\x. (cons x nil)) in sng
  --System F type:
  -- 	(v5 -> (List v5))
  H.it "testLet5" $ expectPolytype
    (var "sng" `with` [
      "sng">: lambda "x" $ list [var "x"]])
    [t0] $ tFun t0T (tList t0T)

  --Untyped input:
  --	let sng = (\x. (cons x nil)) in (pair (sng 0) (sng alice))
  --System F type:
  -- 	((List Nat) * (List String))
  H.it "testLet6" $ expectMonotype
    (pair (var "sng" @@ int32 0) (var "sng" @@ string "alice") `with` [
      "sng">: lambda "x" $ list [var "x"]])
    (Types.pair (tList tInt32) (tList tString))

  --Untyped input:
  --	letrecs + = (\x. (\y. (S (+ (P x) y))))
  --		in (+ (S (S 0)) (S 0))
  --System F type:
  -- 	Nat
  H.it "#6" $ expectMonotype
    ((var "+" @@ (primSucc @@ (primSucc @@ int32 0)) @@ (primSucc @@ int32 0)) `with` [
      "+" >: lambda "x" $ lambda "y" (primSucc @@ (var "+" @@ (primPred @@ var "x") @@ var "y"))])
    tInt32

  --Untyped input:
  --	letrecs f = (\x. (\y. (f 0 x)))
  --		in f
  --System F type:
  -- 	(Nat -> (Nat -> v5))
  H.it "testLet9" $ expectPolytype
    (var "f" `with` [
      "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")])
    [t0] $ tFun tInt32 (tFun tInt32 t0T)

--Untyped input:
--	letrec f = (\x. (\y. (f 0 x)))
--		g = (\xx. (\yy. (g 0 xx)))
--		in (pair f g)
--Type inferred by Hindley-Milner:
--	((Int32 -> (Int32 -> v12)) * (Int32 -> (Int32 -> v14)))
  H.it "testLet10" $ expectPolytype
    ((pair (var "f") (var "g")) `with` [
      "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x"),
      "g">: lambda "xx" $ lambda "yy" (var "g" @@ int32 0 @@ var "xx")])
    [t0, t1] $ Types.pair
      (tFunN [tInt32, tInt32, t0T])
      (tFunN [tInt32, tInt32, t1T])

  --Untyped input:
  --	letrecs f = (\x. (\y. (g 0 x)))
  --		g = (\u. (\v. (f v 0)))
  --		in (pair f g)
  --System F type:
  -- 	((v12 -> (Nat -> v13)) * (Nat -> (v15 -> v16)))
  H.it "testLet11" $ expectPolytype
    ((pair (var "f") (var "g")) `with` [
      "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
      "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)])
    [t0, t1, t2, t3] $ Types.pair
      (tFun t0T (tFun tInt32 t1T))
      (tFun tInt32 (tFun t2T t3T))

  --Untyped input:
  --	letrecs f = (\x. (\y. (g 0 0)))
  --		g = (\u. (\v. (f v 0)))
  --		in (pair f g)
  --System F type:
  -- 	((Nat -> (Nat -> v12)) * (Nat -> (Nat -> v14)))
  H.it "testLet12" $ expectPolytype
    ((pair (var "f") (var "g")) `with` [
      "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ int32 0),
      "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)])
    [t0, t1] $ Types.pair
      (tFun tInt32 (tFun tInt32 t0T))
      (tFun tInt32 (tFun tInt32 t1T))

  --Untyped input:
  --	letrecs f = (\x. (\y. (g 0 x)))
  --		g = (\u. (\v. (f 0 0)))
  --		in (pair f g)
  --System F type:
  -- 	((Nat -> (Nat -> v12)) * (Nat -> (Nat -> v14)))
  H.it "testLet13" $ expectPolytype
    ((pair (var "f") (var "g")) `with` [
      "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
      "g">: lambda "u" $ lambda "v" (var "f" @@ int32 0 @@ int32 0)])
    [t0, t1] $ Types.pair
      (tFun tInt32 (tFun tInt32 t0T))
      (tFun tInt32 (tFun tInt32 t1T))

checkEliminations :: H.SpecWith ()
checkEliminations = H.describe "Check a few hand-picked elimination terms" $ do

  H.it "Match statements" $ do
    expectMonotype
      (match testTypeSimpleNumberName Nothing [
        Field (Name "int") $ lambda "x" $ var "x",
        Field (Name "float") $ lambda "x" $ int32 42])
      (tFun (TypeVariable testTypeSimpleNumberName) tInt32)

checkIndividualTerms :: H.SpecWith ()
checkIndividualTerms = H.describe "Check a few hand-picked terms" $ do

  H.describe "Literal values" $ do
    H.it "test #1" $
      expectMonotype
        (int32 42)
        tInt32
    H.it "test #2" $
      expectMonotype
        (string "foo")
        tString
    H.it "test #3" $
      expectMonotype
        (boolean False)
        Types.boolean
    H.it "test #4" $
      expectMonotype
        (float64 42.0)
        Types.float64

  H.it "Let terms" $ do
    expectPolytype
      (letTerm (Name "x") (float32 42.0) (lambda "y" (lambda "z" (var "x"))))
      [t0, t1] (tFun t0T (tFun t1T Types.float32))

  H.describe "Optionals" $ do
    H.it "test #1" $
      expectMonotype
        (optional $ Just $ int32 42)
        (Types.optional tInt32)
    H.it "test #2" $
      expectPolytype
        (optional Nothing)
        [t0] (Types.optional t0T)

  H.describe "Records" $ do
    H.it "test #1" $
      expectMonotype
        (record testTypeLatLonName [
          Field (Name "lat") $ float32 37.7749,
          Field (Name "lon") $ float32 $ negate 122.4194])
        (TypeVariable testTypeLatLonName)
    H.it "test #2" $
      expectMonotype
        (record testTypeLatLonPolyName [
          Field (Name "lat") $ float32 37.7749,
          Field (Name "lon") $ float32 $ negate 122.4194])
        (Types.apply (TypeVariable testTypeLatLonPolyName) Types.float32)
    H.it "test #3" $
      expectMonotype
        (lambda "lon" (record testTypeLatLonPolyName [
          Field (Name "lat") $ float32 37.7749,
          Field (Name "lon") $ var "lon"]))
        (tFun (Types.float32) (Types.apply (TypeVariable testTypeLatLonPolyName) Types.float32))
    H.it "test #4" $
      expectPolytype
        (lambda "latlon" (record testTypeLatLonPolyName [
          Field (Name "lat") $ var "latlon",
          Field (Name "lon") $ var "latlon"]))
        [t0] (tFun t0T (Types.apply (TypeVariable testTypeLatLonPolyName) t0T))

  H.describe "Record instances of simply recursive record types" $ do
    H.it "test #1" $
      expectMonotype
        (record testTypeIntListName [
          Field (Name "head") $ int32 42,
          Field (Name "tail") $ optional $ Just $ record testTypeIntListName [
            Field (Name "head") $ int32 43,
            Field (Name "tail") $ optional Nothing]])
        (TypeVariable testTypeIntListName)
    H.it "test #2" $
      expectMonotype
        ((lambda "x" $ record testTypeIntListName [
          Field (Name "head") $ var "x",
          Field (Name "tail") $ optional $ Just $ record testTypeIntListName [
            Field (Name "head") $ var "x",
            Field (Name "tail") $ optional Nothing]]) @@ int32 42)
        (TypeVariable testTypeIntListName)
    H.it "test #3" $
      expectMonotype
        (record testTypeListName [
          Field (Name "head") $ int32 42,
          Field (Name "tail") $ optional $ Just $ record testTypeListName [
            Field (Name "head") $ int32 43,
            Field (Name "tail") $ optional Nothing]])
        (Types.apply (TypeVariable testTypeListName) tInt32)
    H.it "test #4" $
      expectMonotype
        ((lambda "x" $ record testTypeListName [
          Field (Name "head") $ var "x",
          Field (Name "tail") $ optional $ Just $ record testTypeListName [
            Field (Name "head") $ var "x",
            Field (Name "tail") $ optional Nothing]]) @@ int32 42)
        (Types.apply (TypeVariable testTypeListName) tInt32)
    H.it "test #5" $
      expectPolytype
        (lambda "x" $ record testTypeListName [
          Field (Name "head") $ var "x",
          Field (Name "tail") $ optional $ Just $ record testTypeListName [
            Field (Name "head") $ var "x",
            Field (Name "tail") $ optional Nothing]])
        [t0] (tFun t0T (Types.apply (TypeVariable testTypeListName) t0T))

  H.describe "Record instances of mutually recursive record types" $ do
    H.it "test #1" $
      expectMonotype
        ((lambda "x" $ record testTypeBuddyListAName [
          Field (Name "head") $ var "x",
          Field (Name "tail") $ optional $ Just $ record testTypeBuddyListBName [
            Field (Name "head") $ var "x",
            Field (Name "tail") $ optional Nothing]]) @@ int32 42)
        (Types.apply (TypeVariable testTypeBuddyListAName) tInt32)
    H.it "test #2" $
      expectPolytype
        (lambda "x" $ record testTypeBuddyListAName [
          Field (Name "head") $ var "x",
          Field (Name "tail") $ optional $ Just $ record testTypeBuddyListBName [
            Field (Name "head") $ var "x",
            Field (Name "tail") $ optional Nothing]])
        [t0] (tFun t0T (Types.apply (TypeVariable testTypeBuddyListAName) t0T))

  H.it "Unions" $ do
    expectMonotype
      (inject testTypeTimestampName $ Field (Name "unixTimeMillis") $ uint64 1638200308368)
      (TypeVariable testTypeTimestampName)

  H.describe "Sets" $ do
    H.it "test #1" $
      expectMonotype
        (set $ S.fromList [boolean True])
        (Types.set Types.boolean)
    H.it "test #2" $
      expectPolytype
        (set $ S.fromList [set S.empty])
        [t0] (Types.set $ Types.set t0T)

  H.describe "Maps" $ do
    H.it "test #1" $
      expectMonotype
        (mapTerm $ M.fromList [(string "firstName", string "Arthur"), (string "lastName", string "Dent")])
        (Types.map tString tString)
    H.it "test #2" $
      expectPolytype
        (mapTerm M.empty)
        [t0, t1] (Types.map t0T t1T)
    H.it "test #3" $
      expectPolytype
        (lambda "x" (lambda "y" (mapTerm $ M.fromList
          [(var "x", float64 0.1), (var "y", float64 0.2)])))
        [t0] (tFun t0T (tFun t0T (Types.map t0T Types.float64)))

--    H.it "Check nominal (newtype) terms" $ do
--      expectMonotype
--        testDataArthur
--        (Types.wrap "Person")
    --   expectMonotype
    --     (lambda "x" (record [
    --       Field "firstName" $ var "x",
    --       Field "lastName" $ var "x",
    --       Field "age" $ int32 42]))
    --     (tFun tString testTypePerson)

checkLetTerms :: H.SpecWith ()
checkLetTerms = H.describe "Check a few hand-picked let terms" $ do

  H.it "Empty let" $ do
    expectMonotype
      ((int32 42) `with` [])
      tInt32

  H.it "Trivial let" $ do
    expectMonotype
      (var "foo" `with` [
        "foo">: int32 42])
      tInt32

  H.it "Multiple references to a let-bound term" $
    expectMonotype
      (list [var "foo", var "bar", var "foo"] `with` [
        "foo">: int32 42,
        "bar">: int32 137])
      (tList tInt32)

  H.describe "Let-polymorphism" $ do
    H.it "test #1" $
      expectPolytype
        ((lambda "x" $ var "id" @@ (var "id" @@ var "x")) `with` [
          "id">: lambda "x" $ var "x"])
        [t0] (tFun t0T t0T)
    H.it "test #2" $
      expectMonotype
        ((var "id" @@ (list [var "id" @@ int32 42])) `with` [
          "id">: lambda "x" $ var "x"])
        (tList tInt32)
    H.it "test #3" $
      expectPolytype
        ((lambda "x" (var "id" @@ (list [var "id" @@ var "x"])))
          `with` [
            "id">: lambda "x" $ var "x"])
        [t0] (tFun t0T (tList t0T))
    H.it "test #4" $
      expectMonotype
        ((pair (var "id" @@ int32 42) (var "id" @@ string "foo"))
          `with` [
            "id">: lambda "x" $ var "x"])
        (Types.pair tInt32 tString)
    H.it "test #5" $
      expectMonotype
        ((pair (var "list" @@ int32 42) (var "list" @@ string "foo"))
          `with` [
            "list">: lambda "x" $ list [var "x"]])
        (Types.pair (tList tInt32) (tList tString))
--    H.it "test #6" $
--      expectPolytype
--        ((var "f") `with` [
--          "sng">: lambda "x" $ list [var "x"],
--          "f">: lambda "x" $ lambda "y" $ Terms.primitive _lists_cons
--            @@ (pair (var "sng" @@ var "x") (var "sng" @@ var "y"))
--            @@ (var "g" @@ var "x" @@ var "y"),
--          "g">: lambda "x" $ lambda "y" $ var "f" @@ int32 42 @@ var "y"])
--        [t0] (tList $ Types.pair tInt32 t0T)

  H.describe "Recursive and mutually recursive let (@wisnesky's test cases)" $ do
    H.it "test #1" $
      expectPolytype
        ((var "f") `with` [
          "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")])
        [t0] (tFun tInt32 (tFun tInt32 t0T))
    H.it "test #2" $
      expectPolytype
        ((pair (var "f") (var "g")) `with` [
          "f">: var "g",
          "g">: var "f"])
        -- Note: Hydra's original type inference algorithm finds (a, a) rather than (a, b)
        --       GHC finds (a, b), as is the case here
        -- Try: :t (let (f, g) = (g, f) in (f, g))
        [t0, t1] (Types.pair t0T t1T)
--    H.it "test #3" $
--      expectPolytype
--        ((pair (var "f") (var "g")) `with` [
--          "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
--          "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)])
--        [t0, t1] (Types.pair
--          (tFun t0T (tFun tInt32 t1T))
--          (tFun tInt32 (tFun t0T t1T)))

    -- letrec + = (\x . (\y . (S (+ (P x) y)))) in (+ (S (S 0)) (S 0))
--    let s = primitive _math_neg
--        p = primitive _math_neg
--    H.it "test #4" $
--      expectPolytype
--        ((var "plus" @@ (s @@ (s @@ int32 0)) @@ (s @@ int32 0)) `with` [
--          "plus">: lambda "x" $ lambda "y" (s @@ (var "plus" @@ (p @@ var "x") @@ var "y"))])
--        [t0] (tFun tInt32 $ tFun t0T tInt32)

--    H.it "test #3" $
--      expectMonotype
--        (int32 0 `with` [
--          "id">: lambda "z" $ var "z",
--          "f">: lambda "p0" $ pair (var "id" @@ var "p0") (var "id" @@ var "p0")])
--        tInt32
--    letrecs id = (\z. z)
--        f = (\p0. (pair (id p0) (id p0)))
--        in 0

checkLists :: H.SpecWith ()
checkLists = H.describe "Check a few hand-picked list terms" $ do

  H.it "List of strings" $
    expectMonotype
      (list [string "foo", string "bar"])
      (tList tString)
  H.it "List of lists of strings" $
    expectMonotype
      (list [list [string "foo"], list []])
      (tList $ tList tString)
  H.it "Empty list" $
    expectPolytype
      (list [])
      [t0] (tList t0T)
  H.it "List containing an empty list" $
    expectPolytype
      (list [list []])
      [t0] (tList $ tList t0T)
  H.it "Lambda producing a polymorphic list" $
    expectPolytype
      (lambda "x" (list [var "x"]))
      [t0] (tFun t0T (tList t0T))
  H.it "Lambda producing a list of integers" $
    expectMonotype
      (lambda "x" (list [var "x", int32 42]))
      (tFun tInt32 $ tList tInt32)
  H.it "List with repeated variables" $
    expectMonotype
      (lambda "x" (list [var "x", string "foo", var "x"]))
      (tFun tString (tList tString))

  H.describe "Lists and lambdas" $ do
    H.it "test #1" $
      expectPolytype
        (lambda "x" $ var "x")
        [t0] (tFun t0T t0T)
    H.it "test #3" $
      expectPolytype
        (lambda "x" $ list [var "x"])
        [t0] (tFun t0T (tList t0T))
    H.it "test #4" $
      expectPolytype
        (list [lambda "x" $ var "x", lambda "y" $ var "y"])
        [t0] (tList $ tFun t0T t0T)

checkLiterals :: H.SpecWith ()
checkLiterals = H.describe "Check arbitrary literals" $ do

  H.it "Verify that type inference preserves the literal to literal type mapping" $
    QC.property $ \l -> expectMonotype
      (TermLiteral l)
      (Types.literal $ literalType l)

checkLambdas :: H.SpecWith()
checkLambdas = H.describe "Check lambdas" $ do

  H.describe "Simple lambdas" $ do
    H.it "test #1" $
      expectPolytype
        (lambda "x" $ var "x")
        [t0] (tFun t0T t0T)
    H.it "test #2" $
      expectPolytype
        (lambda "x" $ int16 137)
        [t0] (tFun t0T Types.int16)

  H.describe "Lambdas and application" $ do
    H.it "test #1" $
      expectMonotype
        (lambda "x" (var "x") @@ string "foo")
        tString

checkOtherFunctionTerms :: H.SpecWith ()
checkOtherFunctionTerms = H.describe "Check a few hand-picked function terms" $ do

  H.describe "List eliminations (folds)" $ do
    let fun = Terms.fold $ primitive _math_add
    H.it "test #1" $
      expectMonotype
        fun
        (tFunN [tInt32, tList tInt32, tInt32])
    H.it "test #2" $
      expectMonotype
        (fun @@ int32 0)
        (tFun (tList tInt32) tInt32)
    H.it "test #3" $
      expectMonotype
        (fun @@ int32 0 @@ (list (int32 <$> [1, 2, 3, 4, 5])))
        tInt32

  H.it "Projections" $ do
    expectMonotype
      (project testTypePersonName (Name "firstName"))
      (tFun (TypeVariable testTypePersonName) tString)

  H.it "Union eliminations (case statements)" $ do
    expectMonotype
      (match testTypeUnionMonomorphicName Nothing [
        Field (Name "bool") (lambda "x" (boolean True)),
        Field (Name "string") (lambda "x" (boolean False)),
        Field (Name "unit") (lambda "x" (boolean False))])
      (tFun (TypeVariable testTypeUnionMonomorphicName) Types.boolean)

checkPathologicalTerms :: H.SpecWith ()
checkPathologicalTerms = H.describe "Check pathological terms" $ do

  H.describe "Infinite lists" $ do
    H.it "test #1" $
      expectMonotype
        ((var "self") `with` [
          "self">: primitive _lists_cons @@ (int32 42) @@ (var "self")])
        (tList tInt32)
    H.it "test #2" $
      expectPolytype
        (lambda "x" ((var "self") `with` [
          "self">: primitive _lists_cons @@ (var "x") @@ (var "self")]))
        [t0] (tFun t0T (tList t0T))
    H.it "test #3" $
      expectPolytype
        ((lambda "x" $ var "self" @@ var "x") `with` [
          "self">: lambda "e" $ primitive _lists_cons @@ (var "e") @@ (var "self" @@ var "e")])
        [t0] (tFun t0T (tList t0T))
    H.it "test #4" $
      expectMonotype
        ((var "build" @@ int32 0) `with` [
          "build">: lambda "x" $ primitive _lists_cons @@ var "x" @@ (var "build" @@
            (primitive _math_add @@ var "x" @@ int32 1))])
        (tList tInt32)

    H.it "Check self-application" $ do
      expectFailure
        (lambda "x" $ var "x" @@ var "x")

checkPolymorphism :: H.SpecWith ()
checkPolymorphism = H.describe "Check polymorphism" $ do

  H.describe "Simple optionals" $ do
    H.it "test #2" $
      expectPolytype
        (optional Nothing)
        [t0] (Types.optional t0T)
    H.it "test #3" $
      expectMonotype
        (optional $ Just $ int32 42)
        (Types.optional tInt32)

  H.describe "Lambdas, lists, and products" $ do
    H.it "test #2" $
      expectPolytype
        (lambda "x" $ pair (var "x") (var "x"))
        [t0] (tFun t0T (Types.pair t0T t0T))
    H.it "test #5" $
      expectPolytype
        (list [lambda "x" $ lambda "y" $ pair (var "y") (var "x")])
        [t0, t1] (tList $ tFun t0T (tFun t1T (Types.pair t1T t0T)))

  H.describe "Mixed expressions with lambdas, constants, and primitive functions" $ do
    H.it "test #1" $
      expectMonotype
        (lambda "x" $
            (primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1))
        (tFun tInt32 tInt32)

checkPrimitives :: H.SpecWith ()
checkPrimitives = H.describe "Check a few hand-picked terms with primitive functions" $ do

  H.describe "Monomorphic primitive functions" $ do
    H.it "test #1" $
      expectMonotype
        (primitive $ Name "hydra.lib.strings.length")
        (tFun tString tInt32)
    H.it "test #2" $
      expectMonotype
        (primitive _math_sub)
        (tFun tInt32 (tFun tInt32 tInt32))

  H.describe "Polymorphic primitive functions" $ do
    H.it "test #1" $
      expectPolytype
        (lambda "el" (primitive _lists_length @@ (list [var "el"])))
        [t0] (tFun t0T tInt32)
    H.it "test #2" $
      expectMonotype
        (lambda "el" (primitive _lists_length @@ (list [int32 42, var "el"])))
        (tFun tInt32 tInt32)
    H.it "test #3" $
      expectPolytype
        (primitive _lists_concat)
        [t0] (tFun (tList $ tList t0T) (tList t0T))
    H.it "test #4" $
      expectPolytype
        (lambda "lists" (primitive _lists_concat @@ var "lists"))
        [t0] (tFun (tList $ tList t0T) (tList t0T))
    H.it "test #5" $
      expectPolytype
        (lambda "lists" (primitive _lists_length @@ (primitive _lists_concat @@ var "lists")))
        [t0] (tFun (tList $ tList t0T) tInt32)
    H.it "test #6" $
      expectPolytype
        (lambda "list" (primitive _lists_length @@ (primitive _lists_concat @@ list[var "list", list []])))
        [t0] (tFun (tList t0T) tInt32)
    H.it "test #7" $
      expectPolytype
        (lambda "list" (primitive _math_add
          @@ int32 1
          @@ (primitive _lists_length @@ (primitive _lists_concat @@ list[var "list", list []]))))
        [t0] (tFun (tList t0T) tInt32)
    H.it "test #8" $
      expectPolytype
        (lambda "lists" (primitive _lists_length @@ (primitive _lists_concat @@ var "lists")))
        [t0] (tFun (tList $ tList t0T) tInt32)

  H.describe "Primitives and application" $ do
    H.it "test #1" $
      expectMonotype
        (primitive _lists_concat @@ list [list [int32 42], list []])
        (tList tInt32)

  H.describe "Lambdas and primitives" $ do
    H.it "test #1" $
      expectMonotype
        (primitive _math_add)
        (tFunN [tInt32, tInt32, tInt32])
    H.it "test #2" $
      expectMonotype
        (lambda "x" (primitive _math_add @@ var "x"))
        (tFunN [tInt32, tInt32, tInt32])
    H.it "test #3" $
      expectMonotype
        (lambda "x" (primitive _math_add @@ var "x" @@ var "x"))
        (tFun tInt32 tInt32)

-- TODO: restore nullary and unary product test cases
checkProducts :: H.SpecWith ()
checkProducts = H.describe "Check a few hand-picked product terms" $ do

  H.it "Empty product" $ do
    expectMonotype
      (Terms.product [])
      (Types.product [])

  H.it "Unary product" $
    expectMonotype
      (Terms.product [int32 42])
      (Types.product [tInt32])

  H.describe "Non-empty, monotyped products" $ do
    H.it "test #1" $
      expectMonotype
        (Terms.product [string "foo", int32 42])
        (Types.product [tString, tInt32])
    H.it "test #2" $
      expectMonotype
        (Terms.product [string "foo", list [float32 42.0, float32 137.0]])
        (Types.product [tString, tList Types.float32])
    H.it "test #3" $
      expectMonotype
        (Terms.product [string "foo", int32 42, list [float32 42.0, float32 137.0]])
        (Types.product [tString, tInt32, tList Types.float32])

  H.describe "Polytyped products" $ do
    H.it "test #1" $
      expectPolytype
        (Terms.product [list [], string "foo"])
        [t0] (Types.product [tList t0T, tString])
    H.it "test #2" $
      expectPolytype
        (Terms.product [int32 42, "foo", list []])
        [t0] (Types.product [tInt32, tString, tList t0T])

  H.describe "Pairs" $ do
    H.it "test #1" $
      expectMonotype
        (pair (int32 42) "foo")
        (Types.pair tInt32 tString)
    H.it "test #2" $
      expectPolytype
        (pair (list []) "foo")
        [t0] (Types.pair (tList t0T) tString)
    H.it "test #3" $
      expectPolytype
        (pair (list []) (list []))
        [t0, t1] (Types.pair (tList t0T) (tList t1T))

  H.describe "Products with polymorphic components" $ do
    H.it "test #1" $
      expectPolytype
        (pair (int32 42) (lambda "x" $ var "x"))
        [t0] (Types.pair tInt32 (tFun t0T t0T))
    H.it "test #2" $
      expectPolytype
        (pair (lambda "x" $ var "x") (lambda "x" $ var "x"))
        [t0, t1] (Types.pair
          (tFun t0T t0T)
          (tFun t1T t1T))
    H.it "test #3" $
      expectPolytype
        (lambda "x" $ pair (var "x") (list [var "x"]))
        [t0] (tFun t0T $ Types.pair t0T (tList t0T))

checkSums :: H.SpecWith ()
checkSums = H.describe "Check a few hand-picked sum terms" $ do

  H.describe "Singleton sum terms" $ do
    H.it "test #1" $
      expectMonotype
        (Terms.sum 0 1 $ string "foo")
        (tSum [tString])
    H.it "test #2" $
      expectPolytype
        (Terms.sum 0 1 $ list [])
        [t0] (tSum [tList t0T])

  H.describe "Non-singleton sum terms" $ do
    H.it "test #1" $
      expectPolytype
        (Terms.sum 0 2 $ string "foo")
        [t0] (tSum [tString, t0T])
    H.it "test #2" $
      expectPolytype
        (Terms.sum 1 2 $ string "foo")
        [t0] (tSum [t0T, tString])

checkWrappedTerms :: H.SpecWith ()
checkWrappedTerms = H.describe "Check nominal introductions and eliminations" $ do

  H.describe "Nominal introductions" $ do
    H.it "test #1" $
      expectMonotype
        (wrap testTypeStringAliasName $ string "foo")
        (TypeVariable testTypeStringAliasName)
    H.it "test #2" $
      expectMonotype
        (lambda "v" $ wrap testTypeStringAliasName $ var "v")
        (tFun tString (TypeVariable testTypeStringAliasName))

  H.it "Nominal eliminations" $ do
--    expectMonotype
--      (unwrap testTypeStringAliasName)
--      (tFun testTypeStringAlias (Ann.doc "An alias for the string type" tString))
    expectMonotype
      (unwrap testTypeStringAliasName @@ (wrap testTypeStringAliasName $ string "foo"))
      tString

-- TODO: restore the test cases which are commented out
spec :: H.Spec
spec = do
  checkAlgorithmW
--  checkEliminations
--  checkIndividualTerms
  checkLetTerms
  checkLists
  checkLiterals
--  checkOtherFunctionTerms
  checkPathologicalTerms
--  checkPolymorphism
  checkPrimitives
  checkProducts
--  checkSums
--  checkWrappedTerms
