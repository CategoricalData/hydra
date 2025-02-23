module Hydra.Sources.Tier3.Test.Inference.AlgorithmW (algorithmWTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Tier3.Test.Inference.Fundamentals

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


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
--  H.it "#0" $ expectPolytype
--    (lambda "x" $ var "x")
--    [t0] $ tFun t0T t0T
    expectPoly 1 []
      (lambda "x" $ var "x")
      ["t0"] (T.functionN [T.var "t0", T.var "t0"]),

--  --Untyped input:
--  --	letrecs foo = (\x. x)
--  --		in 42
--  --System F type:
--  -- 	Nat
--  H.it "#1" $ expectMonotype
--    (int32 32 `with` [
--      "foo">: lambda "x" $ var "x"])
--    tInt32
    expectMono 2 []
      (lets [
        "foo" >: lambda "x" $ var "x"]
        $ int32 32)
      T.int32,

--  --Untyped input:
--  --	let f = (\x. x) in (f 0)
--  --System F type:
--  -- 	Nat
--  H.it "testLet3" $ expectMonotype
--    ((var "f" @@ int32 0) `with` [
--      "f">: lambda "x" $ var "x"])
--    tInt32
    expectMono 3 []
      (lets [
        "f" >: lambda "x" $ var "x"]
        $ var "f" @@ int32 0)
      T.int32,

--  --Untyped input:
--  --	let f = ((\x. x) 0) in f
--  --System F type:
--  -- 	Nat
--  H.it "testLet4" $ expectMonotype
--    (var "f" `with` [
--      "f">: (lambda "x" $ var "x") @@ int32 0])
--    tInt32
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
--  H.it "testLet5" $ expectPolytype
--    (var "sng" `with` [
--      "sng">: lambda "x" $ list [var "x"]])
--    [t0] $ tFun t0T (tList t0T)
    expectPoly 5 []
      (lets [
        "sng" >: lambda "x" $ list [var "x"]]
        $ var "sng")
      ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0")),

--  --Untyped input:
--  --	let sng = (\x. (cons x nil)) in (pair (sng 0) (sng alice))
--  --System F type:
--  -- 	((List Nat) * (List String))
--  H.it "testLet6" $ expectMonotype
--    (pair (var "sng" @@ int32 0) (var "sng" @@ string "alice") `with` [
--      "sng">: lambda "x" $ list [var "x"]])
--    (Types.pair (tList tInt32) (tList tString))
    expectMono 6 [tag_disabledForDefaultInference]
      (lets [
        "sng" >: lambda "x" $ list [var "x"]]
        $ pair (var "sng" @@ int32 0) (var "sng" @@ string "alice"))
      (T.pair (T.list T.int32) (T.list T.string)),

--  --Untyped input:
--  --	letrecs + = (\x. (\y. (S (+ (P x) y))))
--  --		in (+ (S (S 0)) (S 0))
--  --System F type:
--  -- 	Nat
--  H.it "#6" $ expectMonotype
--    ((var "+" @@ (primSucc @@ (primSucc @@ int32 0)) @@ (primSucc @@ int32 0)) `with` [
--      "+" >: lambda "x" $ lambda "y" (primSucc @@ (var "+" @@ (primPred @@ var "x") @@ var "y"))])
--    tInt32
    expectMono 7 [tag_disabledForDefaultInference, tag_disabledForAltInference]
      (lets [
        "+" >: lambda "x" $ lambda "y" (primSucc @@ (var "+" @@ (primPred @@ var "x") @@ var "y"))]
        $ var "+" @@ (primSucc @@ (primSucc @@ int32 0)) @@ (primSucc @@ int32 0))
      T.int32,

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (f 0 x)))
--  --		in f
--  --System F type:
--  -- 	(Nat -> (Nat -> v5))
--  H.it "testLet9" $ expectPolytype
--    (var "f" `with` [
--      "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")])
--    [t0] $ tFun tInt32 (tFun tInt32 t0T)
    expectPoly 9 [tag_disabledForDefaultInference, tag_disabledForAltInference]
      (lets [
        "f" >: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x")]
        $ var "f")
      ["t0"] (T.functionN [T.int32, T.int32, T.var "t0"]),

----Untyped input:
----	letrec f = (\x. (\y. (f 0 x)))
----		g = (\xx. (\yy. (g 0 xx)))
----		in (pair f g)
----Type inferred by Hindley-Milner:
----	((Int32 -> (Int32 -> v12)) * (Int32 -> (Int32 -> v14)))
--  H.it "testLet10" $ expectPolytype
--    ((pair (var "f") (var "g")) `with` [
--      "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x"),
--      "g">: lambda "xx" $ lambda "yy" (var "g" @@ int32 0 @@ var "xx")])
--    [t0, t1] $ Types.pair
--      (tFunN [tInt32, tInt32, t0T])
--      (tFunN [tInt32, tInt32, t1T])
    expectPoly 10 [tag_disabledForDefaultInference, tag_disabledForAltInference]
      (lets [
        "f">: lambda "x" $ lambda "y" (var "f" @@ int32 0 @@ var "x"),
        "g">: lambda "xx" $ lambda "yy" (var "g" @@ int32 0 @@ var "xx")]
        $ pair (var "f") (var "g"))
      ["t0", "t1"] (T.pair
        (T.functionN [T.int32, T.int32, T.var "t0"])
        (T.functionN [T.int32, T.int32, T.var "t1"])),

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (g 0 x)))
--  --		g = (\u. (\v. (f v 0)))
--  --		in (pair f g)
--  --System F type:
--  -- 	((v12 -> (Nat -> v13)) * (Nat -> (v15 -> v16)))
--  H.it "testLet11" $ expectPolytype
--    ((pair (var "f") (var "g")) `with` [
--      "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
--      "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)])
--    [t0, t1, t2, t3] $ T.pair
--      (tFun t0T (tFun tInt32 t1T))
--      (tFun tInt32 (tFun t2T t3T))
    expectPoly 11 [tag_disabledForDefaultInference, tag_disabledForAltInference]
      (lets [
        "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
        "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
        $ pair (var "f") (var "g"))
      ["t0", "t1", "t2", "t3"] (T.pair
        (T.functionN [T.var "t0", T.int32, T.var "t1"])
        (T.functionN [T.int32, T.var "t2", T.var "t3"])),

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (g 0 0)))
--  --		g = (\u. (\v. (f v 0)))
--  --		in (pair f g)
--  --System F type:
--  -- 	((Nat -> (Nat -> v12)) * (Nat -> (Nat -> v14)))
--  H.it "testLet12" $ expectPolytype
--    ((pair (var "f") (var "g")) `with` [
--      "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ int32 0),
--      "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)])
--    [t0, t1] $ T.pair
--      (tFun tInt32 (tFun tInt32 t0T))
--      (tFun tInt32 (tFun tInt32 t1T))
    expectPoly 12 [tag_disabledForDefaultInference, tag_disabledForAltInference]
      (lets [
        "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ int32 0),
        "g">: lambda "u" $ lambda "v" (var "f" @@ var "v" @@ int32 0)]
        $ pair (var "f") (var "g"))
      ["t0", "t1"] (T.pair
        (T.functionN [T.int32, T.int32, T.var "t0"])
        (T.functionN [T.int32, T.int32, T.var "t1"])),

--  --Untyped input:
--  --	letrecs f = (\x. (\y. (g 0 x)))
--  --		g = (\u. (\v. (f 0 0)))
--  --		in (pair f g)
--  --System F type:
--  -- 	((Nat -> (Nat -> v12)) * (Nat -> (Nat -> v14)))
--  H.it "testLet13" $ expectPolytype
--    ((pair (var "f") (var "g")) `with` [
--      "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
--      "g">: lambda "u" $ lambda "v" (var "f" @@ int32 0 @@ int32 0)])
--    [t0, t1] $ Types.pair
--      (tFun tInt32 (tFun tInt32 t0T))
--      (tFun tInt32 (tFun tInt32 t1T))
    expectPoly 13 [tag_disabledForDefaultInference, tag_disabledForAltInference]
      (lets [
        "f">: lambda "x" $ lambda "y" (var "g" @@ int32 0 @@ var "x"),
        "g">: lambda "u" $ lambda "v" (var "f" @@ int32 0 @@ int32 0)]
        $ pair (var "f") (var "g"))
      ["t0", "t1"] (T.pair
        (T.functionN [T.int32, T.int32, T.var "t0"])
        (T.functionN [T.int32, T.int32, T.var "t1"]))]
  where
    -- Placeholders for the primitives in @wisnesky's test cases; they are not necessarily the same functions,
    -- but they have the same types.
    primPred = primitive _math_neg
    primSucc = primitive _math_neg
