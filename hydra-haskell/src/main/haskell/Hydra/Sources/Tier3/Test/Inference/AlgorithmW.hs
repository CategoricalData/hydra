module Hydra.Sources.Tier3.Test.Inference.AlgorithmW (algorithmWTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Phantoms as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Tier3.Test.Inference.Fundamentals

import qualified Data.Map as M
import Prelude hiding (map, sum)


algorithmWTests :: TTerm TestGroup
algorithmWTests = supergroup "Algorithm W test cases" [
  testGroupForSystemF]

-- @wisnesky's original Algorithm W test cases, modified so as to normalize type variables
testGroupForSystemF :: TTerm TestGroup
testGroupForSystemF = subgroup "STLC to System F" [

--  --Untyped input:
--  --	(\x. x)
--  --System F type:
--  -- 	(v0 -> v0)
    expectPoly 1 []
      (lambda "x" $ var "x")
      ["t0"] (T.functionMany [T.var "t0", T.var "t0"]),

--  --Untyped input:
--  --	letrecs foo = (\x. x)
--  --		in 42
--  --System F type:
--  -- 	Nat
    expectMono 2 []
      (lets [
        "foo" >: lambda "x" $ var "x"]
        $ int32 42)
      T.int32,

--  --Untyped input:
--  --	let f = (\x. x) in (f 0)
--  --System F type:
--  -- 	Nat
    expectMono 3 []
      (lets [
        "f" >: lambda "x" $ var "x"]
        $ var "f" @@ int32 0)
      T.int32,

--  --Untyped input:
--  --	let f = ((\x. x) 0) in f
--  --System F type:
--  -- 	Nat
    expectMono 4 []
      (lets [
        "f" >: (lambda "x" $ var "x") @@ int32 0]
        $ var "f")
      T.int32,

---- TODO
----testAdt3 = TestCase "testAdt3" $ Abs "z" $ ExprConst (Fold "List")
----  @@ Var "z"
----  @@ ExprConst (Con "Nil")
----  @@ ExprConst (Con "Cons")
----
----foldl :: TTerm ((b -> a -> b) -> b -> [a] -> b)
----cons :: TTerm (a -> [a] -> [a])
----
----[testAdt3]
----Untyped input:
----	(\z. (fold_List z Nil Cons))
----Type inferred by Hindley-Milner:
----	(List v6 -> List v6)
----
----System F translation:
----	(\z:List v6 . ((fold_List [List v6 ,v6]) z (Nil [v6]) (Cons [v6])))
----  H.it "testAdt3" $ expectPolytype
----    ()
----    [t0] (tFun t0 t0)
--
--  --Untyped input:
--  --	let sng = (\x. (cons x nil)) in sng
--  --System F type:
--  -- 	(v5 -> (List v5))
    expectPoly 5 []
      (lets [
        "sng" >: lambda "x" $ list [var "x"]]
        $ var "sng")
      ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),

--  --Untyped input:
--  --	let sng = (\x. (cons x nil)) in (pair (sng 0) (sng alice))
--  --System F type:
--  -- 	((List Nat) * (List String))
    expectMono 6 []
      (lets [
        "sng" >: lambda "x" $ list [var "x"]]
        $ pair (var "sng" @@ int32 0) (var "sng" @@ string "alice"))
      (T.pair (T.list T.int32) (T.list T.string)),

--  --Untyped input:
--  --	letrecs + = (\x. (\y. (S (+ (P x) y))))
--  --		in (+ (S (S 0)) (S 0))
--  --System F type:
--  -- 	Nat
    expectMono 7 []
      (lets [
        "+" >: lambdas ["x", "y"] (primSucc @@ (var "+" @@ (primPred @@ var "x") @@ var "y"))]
        $ var "+" @@ (primSucc @@ (primSucc @@ int32 0)) @@ (primSucc @@ int32 0))
      T.int32,

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (f 0 x)))
--  --		in f
--  --System F type:
--  -- 	(Nat -> (Nat -> v5))
    expectPoly 9 []
      (lets [
        "f" >: lambdas ["x", "y"] (var "f" @@ int32 0 @@ var "x")]
        $ var "f")
      ["t0"] (T.functionMany [T.int32, T.int32, T.var "t0"]),

----Untyped input:
----	letrec f = (\x. (\y. (f 0 x)))
----		g = (\xx. (\yy. (g 0 xx)))
----		in (pair f g)
----Type inferred by Hindley-Milner:
----	((Int32 -> (Int32 -> v12)) * (Int32 -> (Int32 -> v14)))
    expectPoly 10 []
      (lets [
        "f">: lambdas ["x", "y"] (var "f" @@ int32 0 @@ var "x"),
        "g">: lambda "xx" $ lambda "yy" (var "g" @@ int32 0 @@ var "xx")]
        $ pair (var "f") (var "g"))
      ["t0", "t1"] (T.pair
        (T.functionMany [T.int32, T.int32, T.var "t0"])
        (T.functionMany [T.int32, T.int32, T.var "t1"])),

    -- Note: in the following three test cases, the original Algorithm W implementation and the new inference
    --       implementation find slightly different results. This is a result of tradeoffs between stronger support
    --       for monomorphic recursion vs. stronger support for wide letrecs with polymorphism.
--  --Untyped input:
--  --	letrecs f = (\x. (\y. (g 0 x)))
--  --		g = (\u. (\v. (f v 0)))
--  --		in (pair f g)
--  --System F type:
--  -- 	((v12 -> (Nat -> v13)) * (Nat -> (v15 -> v16)))
    expectPoly 11 []
      (lets [
        "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
        "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
        $ pair (var "f") (var "g"))
      ["t0", "t1", "t2", "t3"] (T.pair
        (T.functionMany [T.var "t0", T.int32, T.var "t1"])
        (T.functionMany [T.int32, T.var "t2", T.var "t3"])),

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (g 0 0)))
--  --		g = (\u. (\v. (f v 0)))
--  --		in (pair f g)
--  --System F type:
--  -- 	((Nat -> (Nat -> v12)) * (Nat -> (Nat -> v14)))
    expectPoly 12 []
      (lets [
        "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ int32 0),
        "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
        $ pair (var "f") (var "g"))
      ["t0", "t1"] (T.pair
        (T.functionMany [T.int32, T.int32, T.var "t0"])
        (T.functionMany [T.int32, T.int32, T.var "t1"])),

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (g 0 x)))
--  --		g = (\u. (\v. (f 0 0)))
--  --		in (pair f g)
--  --System F type:
--  -- 	((Nat -> (Nat -> v12)) * (Nat -> (Nat -> v14)))
    expectPoly 13 []
      (lets [
        "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
        "g">: lambda "u" $ lambda "v" (var "f" @@ int32 0 @@ int32 0)]
        $ pair (var "f") (var "g"))
      ["t0", "t1"] (T.pair
        (T.functionMany [T.int32, T.int32, T.var "t0"])
        (T.functionMany [T.int32, T.int32, T.var "t1"]))]
  where
    -- Placeholders for the primitives in @wisnesky's test cases; they are not necessarily the same functions,
    -- but they have the same types.
    primPred = primitive _math_neg
    primSucc = primitive _math_neg
