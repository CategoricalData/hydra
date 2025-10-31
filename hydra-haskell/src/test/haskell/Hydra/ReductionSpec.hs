{-# LANGUAGE OverloadedStrings #-}

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ReductionSpec.spec
-}

module Hydra.ReductionSpec where

import Hydra.Kernel
import Hydra.Dsl.Terms as Terms
import Hydra.Lib.Strings
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Show.Core as ShowCore

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Set as S


testAlphaConversion :: H.SpecWith ()
testAlphaConversion = do
  H.describe "Tests for alpha conversion" $ do
    H.it "Variables are substituted at the top level" $
      QC.property $ \v ->
        alphaConvert (Name v) (Name $ v ++ "'") (var v) == (var (v ++ "'") :: Term)
    H.it "Variables are substituted within subexpressions" $
      QC.property $ \v ->
        alphaConvert (Name v) (Name $ v ++ "'") (list [int32 42, var v])
          == (list [int32 42, var (v ++ "'")] :: Term)
    H.it "Lambdas with unrelated variables are transparent to alpha conversion" $
      QC.property $ \v ->
        alphaConvert (Name v) (Name $ v ++ "1") (lambda (v ++ "2") $ list [int32 42, var v, var (v ++ "2")])
          == (lambda (v ++ "2") $ list [int32 42, var (v ++ "1"), var (v ++ "2")] :: Term)
    H.it "Lambdas of the same variable are opaque to alpha conversion" $
      QC.property $ \v ->
        alphaConvert (Name v) (Name $ v ++ "1") (lambda v $ list [int32 42, var v, var (v ++ "2")])
          == (lambda v $ list [int32 42, var v, var (v ++ "2")] :: Term)

testBetaReduceTypeRecursively :: H.SpecWith ()
testBetaReduceTypeRecursively = do
  H.describe "Beta reduce types recursively" $ do

    H.it "Try non-application types" $ do
      H.shouldBe
        (reduce Types.unit)
        Types.unit
      H.shouldBe
        (reduce testTypeLatLon)
        testTypeLatLon

    H.it "Try simple application types" $ do
      H.shouldBe
        (reduce app1)
        (Types.function Types.string Types.string)
      H.shouldBe
        (reduce app2)
        testTypeLatLon
      H.shouldBe
        (reduce app3)
        (TypeRecord $ RowType (Name "Example") [Types.field "foo" Types.unit])

    H.it "Try recursive application types" $ do
      H.shouldBe
        (reduce app4)
        (TypeRecord $ RowType (Name "Example") [Types.field "f1" Types.int32, Types.field "f2" Types.int64])

--    H.it "Distinguish between eager and lazy evaluation" $ do
--      H.shouldBe
--        (reduce False app5)
--        (TypeRecord $ RowType (Name "Example") [Types.field "foo" app1])
--      H.shouldBe
--        (reduce True app5)
--        (TypeRecord $ RowType (Name "Example") [Types.field "foo" $ Types.function Types.string Types.string])
  where
    app1 = Types.apply (Types.forAll "t" $ Types.function (Types.var "t") (Types.var "t")) Types.string :: Type
    app2 = Types.apply (Types.forAll "x" testTypeLatLon) Types.int32 :: Type
    app3 = Types.apply (Types.forAll "a" $ TypeRecord $ RowType (Name "Example") [Types.field "foo" $ Types.var "a"]) Types.unit :: Type
    app4 = Types.apply (Types.apply (Types.forAll "x" $ Types.forAll "y" $ TypeRecord $ RowType (Name "Example") [
      Types.field "f1" $ Types.var "x",
      Types.field "f2" $ Types.var "y"]) Types.int32) Types.int64 :: Type
    app5 = Types.apply (Types.forAll "a" $ TypeRecord $ RowType (Name "Example") [Types.field "foo" $ Types.var "a"]) app1

testEtaExpansion :: H.SpecWith ()
testEtaExpansion = H.describe "etaExpandTypedTerms" $ do
  tx <- H.runIO $ fromTestFlow "test eta expansion" $ graphToTypeContext testGraph

  let expandsTo desc termBefore termAfter = H.it desc $ do
        result <- fromTestFlow "eta expand" $ etaExpandTypedTerm tx termBefore
        H.shouldBe (ShowCore.term result) (ShowCore.term termAfter)

      noChange desc term = expandsTo desc term term

  H.describe "Partial application of primitives" $ do
    H.describe "Bare primitives are not expanded" $ do
      noChange "unary primitive"
        toLower

      noChange "binary primitive"
        splitOn

    H.describe "Partially applied primitives expand with lambdas" $ do
      expandsTo "binary primitive with one argument"
        (splitOn @@ string "foo")
        (lambda "v1" $ splitOn @@ string "foo" @@ var "v1")

      expandsTo "ternary primitive with one argument"
        (foldl @@ var "f")
        (lambda "v1" $ lambda "v2" $ foldl @@ var "f" @@ var "v1" @@ var "v2")

    H.describe "Fully applied primitives are not expanded" $ do
      noChange "unary primitive"
        (toLower @@ string "FOO")

      noChange "binary primitive"
        (splitOn @@ string "," @@ string "a,b,c")

    H.describe "Record projections" $ do
      H.describe "Bare projections expand with a lambda" $ do
        expandsTo "projection without argument"
          (project (Name "Person") (Name "firstName"))
          (lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1")

      H.describe "Applied projections are not expanded" $ do
        noChange "projection with argument"
          (project (Name "Person") (Name "firstName") @@ var "person")

        noChange "projection applied to a record"
          (project (Name "Person") (Name "firstName") @@
            record (Name "Person") ["firstName">: string "John", "lastName">: string "Doe"])

      H.describe "Projections nested in other structures" $ do
        expandsTo "projection in a list"
          (list [project (Name "Person") (Name "firstName"), toLower])
          (list [lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1", toLower])

        expandsTo "projection in a tuple"
          (pair (project (Name "Person") (Name "firstName")) (string "default"))
          (pair (lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1") (string "default"))

        expandsTo "projection in let binding"
          (lets ["getter">: project (Name "Person") (Name "firstName")] (var "getter"))
          (lets ["getter">: lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1"] (var "getter"))

        expandsTo "projection in lambda body"
          (lambda "x" $ project (Name "Person") (Name "firstName"))
          (lambda "x" $ lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1")

      H.describe "Function-valued projections" $ do
        noChange "projection of function-valued field applied to arguments should not be expanded"
          (tyapps
              (project (Name "Triple") (Name "first"))
              [Types.function Types.string Types.string, Types.string, Types.string]
            @@ tyapps
              (record (Name "Triple") ["first">: primitive _strings_toLower, "second">: string "middle", "third">: string "last"])
              [Types.function Types.string Types.string, Types.string, Types.string]
            @@ string "DATA")

    H.describe "Polymorphic terms (System F)" $ do
      H.describe "Type lambdas in let bindings" $ do
        noChange "polymorphic identity function"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (var "id"))

        expandsTo "monomorphic partially applied primitive"
          (letsTyped [("partial", splitOn @@ string "foo", Types.mono (Types.function Types.string (Types.list Types.string)))]
            (var "partial"))
          (letsTyped [("partial", lambda "v1" $ splitOn @@ string "foo" @@ var "v1", Types.mono (Types.function Types.string (Types.list Types.string)))]
            (var "partial"))

        expandsTo "monomorphic projection"
          (letsTyped [("getter", project (Name "Person") (Name "firstName"), Types.mono (Types.function (Types.var "Person") Types.string))]
            (var "getter"))
          (letsTyped [("getter", lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1", Types.mono (Types.function (Types.var "Person") Types.string))]
            (var "getter"))

      H.describe "Type applications of polymorphic bindings" $ do
        noChange "polymorphic variable with type application"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (var "id" `tyapp` Types.string))

        expandsTo "type application of identity applied to binary function with no arguments"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            ((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn))
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (lambda "v1" $ lambda "v2" $ ((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ var "v1" @@ var "v2"))

        expandsTo "type application of identity applied to partially applied binary function"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            ((var "id" `tyapp` (Types.function Types.string (Types.list Types.string))) @@ (splitOn @@ string ",")))
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (lambda "v1" $ ((var "id" `tyapp` (Types.function Types.string (Types.list Types.string))) @@ (lambda "v1" $ splitOn @@ string "," @@ var "v1")) @@ var "v1"))

        noChange "type application of identity applied to fully applied binary function"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            ((var "id" `tyapp` (Types.list Types.string)) @@ (splitOn @@ string "," @@ string "foo,bar")))

        expandsTo "type application of identity applied to binary function, then applied to one argument"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ string ","))
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (lambda "v1" $ (((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ string ",") @@ var "v1"))

        noChange "type application of identity applied to binary function, then fully applied to two arguments"
          (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
            (((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ string "," @@ string "foo,bar"))

    H.describe "Higher-Order Functions" $ do
      H.describe "Functions that return functions" $ do
        noChange "lambda returning bare binary primitive"
          (lambda "x" splitOn)

        noChange "lambda returning bare unary primitive"
          (lambda "x" toLower)

        expandsTo "lambda returning partially applied primitive"
          (lambda "x" $ splitOn @@ ",")
          (lambda "x" $ lambda "v1" $ splitOn @@ "," @@ var "v1")

        noChange "lambda returning fully applied primitive"
          (lambda "x" $ splitOn @@ "," @@ var "x")

        expandsTo "lambda returning bare projection"
          (lambda "person" $ project (Name "Person") (Name "firstName"))
          (lambda "person" $ lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1")

        expandsTo "nested lambdas with partial application in body"
          (lambda "x" $ lambda "y" $ splitOn @@ var "x")
          (lambda "x" $ lambda "y" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")

        expandsTo "lambda returning lambda returning partial application"
          (lambda "x" $ lambda "y" $ lambda "z" $ splitOn @@ var "x")
          (lambda "x" $ lambda "y" $ lambda "z" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")

    H.describe "Let terms" $ do
      H.describe "partial application of a let-bound function" $ do
        expandsTo "simple"
          (letsTyped
            [("helper",
              lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                cat @@ list [var "arg1", var "arg2", var "arg3"],
              Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
            var "helper" @@ "foo")
          (letsTyped
            [("helper",
              lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                cat @@ list [var "arg1", var "arg2", var "arg3"],
              Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
            lambda "v1" $ lambda "v2" $ var "helper" @@ "foo" @@ var "v1" @@ var "v2")

        expandsTo "in a fold"
          (letsTyped
            [("helper",
              lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                cat @@ list [var "arg1", var "arg2", var "arg3"],
              Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
            (tyapps foldl [Types.string, Types.string])
              @@ (var "helper" @@ "foo")
              @@ ""
              @@ list ["bar", "baz"])
          (letsTyped
            [("helper",
              lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                cat @@ list [var "arg1", var "arg2", var "arg3"],
              Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
            (tyapps foldl [Types.string, Types.string])
              @@ (lambda "v1" $ lambda "v2" $ var "helper" @@ "foo" @@ var "v1" @@ var "v2")
              @@ ""
              @@ list ["bar", "baz"])

        expandsTo "within another let binding"
          (letsTyped
            [("tryme",
              (letsTyped
                [("helper",
                  lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                    cat @@ list [var "arg1", var "arg2", var "arg3"],
                  Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
                var "helper" @@ "foo"),
              Types.mono $ Types.functionMany [Types.string, Types.string, Types.string])] $
            unit)
          (letsTyped
            [("tryme",
              (letsTyped
                [("helper",
                  lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                    cat @@ list [var "arg1", var "arg2", var "arg3"],
                  Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
                lambda "v1" $ lambda "v2" $ var "helper" @@ "foo" @@ var "v1" @@ var "v2"),
              Types.mono $ Types.functionMany [Types.string, Types.string, Types.string])] $
            unit)

    H.describe "Case statements" $ do
      H.describe "monomorphic at top level" $ do
        expandsTo "non-applied case statement"
         (match (Name "UnionMonomorphic")
           (Just "other") [
           "string">: lambda "s" $ var "s"])
         (lambda "v1" $ (match (Name "UnionMonomorphic")
             (Just "other") [
             "string">: lambda "s" $ var "s"])
           @@ var "v1")
        noChange "applied case statement"
         ((match (Name "UnionMonomorphic")
             (Just "other") [
             "string">: lambdaTyped "s" Types.string $ var "s"])
           @@ (inject (Name "UnionMonomorphic") $ Field (Name "string") "foo"))
        noChange "applied case statement in lambda"
         (lambdaTyped "x" (Types.var "UnionMonomorphic") $ (match (Name "UnionMonomorphic")
             (Just "other") [
             "string">: lambdaTyped "s" Types.string $ var "s"])
           @@ var "x")

      H.describe "monomorphic in let binding" $ do
        expandsTo "non-applied case statement"
          (letsTyped [
              ("test",
               match (Name "UnionMonomorphic")
                 (Just "other") [
                 "string">: lambda "s" $ var "s"],
               Types.mono $ Types.function (Types.var "UnionMonomorphic") Types.string)]
            "ignored")
          (letsTyped [
              ("test",
               lambda "v1" $ match (Name "UnionMonomorphic")
                 (Just "other") [
                 "string">: lambda "s" $ var "s"] @@ var "v1",
               Types.mono $ Types.function (Types.var "UnionMonomorphic") Types.string)]
            "ignored")
        noChange "applied case statement"
          (letsTyped [
              ("test",
               (match (Name "UnionMonomorphic")
                   (Just "other") [
                   "string">: lambdaTyped "s" Types.string $ var "s"])
                 @@ (inject (Name "UnionMonomorphic") $ Field (Name "string") "foo"),
               Types.mono $ Types.string)]
            "ignored")
        noChange "applied case statement in lambda"
          (letsTyped [
              ("test",
               lambdaTyped "x" (Types.var "UnionMonomorphic") $ (match (Name "UnionMonomorphic")
                   (Just "other") [
                   "string">: lambdaTyped "s" Types.string $ var "s"])
                 @@ var "x",
               Types.mono $ Types.function Types.string Types.string)]
            "ignored")

      H.describe "polymorphic in let binding" $ do
        expandsTo "non-applied UnionPolymorphicRecursive"
          (letsTyped [
              ("test",
               tyapp (match (Name "UnionPolymorphicRecursive")
                 (Just "other") [
                 "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32,
               Types.mono $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) Types.string)] $
            var "test")
          (letsTyped [
              ("test",
               lambda "v1" $ tyapp (match (Name "UnionPolymorphicRecursive")
                   (Just "other") [
                   "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32
                 @@ var "v1",
               Types.mono $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) Types.string)] $
            var "test")
        noChange "applied UnionPolymorphicRecursive with int32"
          (letsTyped [
            ("test",
             tyapp (match (Name "UnionPolymorphicRecursive")
                 (Just "other") [
                 "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32
               @@ tyapp (inject (Name "UnionPolymorphicRecursive") $ Field (Name "value") $ int32 42) Types.int32,
             Types.mono $ Types.string)] $
            var "test")
        noChange "applied UnionPolymorphicRecursive with int32 in lambda"
          (letsTyped [
            ("test",
             lambdaTyped "x" (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) $
               tyapp (match (Name "UnionPolymorphicRecursive")
                   (Just "other") [
                   "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32
                 @@ var "x",
             Types.mono $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) Types.string)] $
            var "test")
        noChange "applied generic UnionPolymorphicRecursive in lambda"
          (tylam "t0" $ letsTyped [
            ("test",
             tylam "t1" $ lambdaTyped "x" (Types.apply (Types.var "UnionPolymorphicRecursive") (Types.var "t1")) $
               tyapp (match (Name "UnionPolymorphicRecursive")
                   (Just "other") [
                   "value">: lambdaTyped "ignored" (Types.var "t1") $ "foo"]) (Types.var "t1")
                 @@ var "x",
             Types.poly ["t1"] $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") (Types.var "t1")) Types.string)] $
            tyapp (var "test") $ Types.var "t0")

      H.describe "polymorphic using Hydra kernel types (regression test)" $ do
        -- This expands because the case statements, after consuming a direction, produces a function.
        -- Eta expansion creates a lambda to make the function argument explicit.
        noChange "case statement on CoderDirection applied to argument, producing a function"
          (tylams ["t0", "t1"] $
            lambdaTyped "dir" (Types.var "hydra.coders.CoderDirection") $
              lambdaTyped "coder" (Types.applys (Types.var "hydra.compute.Coder") (Types.var <$> ["t0", "t0", "t1", "t1"])) $
                match (Name "hydra.coders.CoderDirection")
                  Nothing [
                  "encode">: lambdaTyped "_" Types.unit $
                    lambdaTyped "v12" (Types.var "t1") $
                      tyapps (project (Name "hydra.compute.Coder") (Name "encode")) (Types.var <$> ["t0", "t0", "t1", "t1"])
                        @@ var "coder" @@ var "v12",
                  "decode">: lambdaTyped "_" Types.unit $
                    lambdaTyped "v12" (Types.var "t1") $
                      tyapps (project (Name "hydra.compute.Coder") (Name "decode")) (Types.var <$> ["t0", "t0", "t1", "t1"])
                        @@ var "coder" @@ var "v12"]
                @@ var "dir")

      H.describe "Forced expansion in case statement branches" $ do
        expandsTo "variable reference in case branch is expanded"
          (letsTyped [("handler", toLower, Types.mono (Types.function Types.string Types.string))] $
            match (Name "UnionMonomorphic") Nothing
              ["bool">: lambda "ignored" $ string "boolean value",
               "string">: var "handler",
               "unit">: lambda "ignored" $ string "unit value"])
          (letsTyped [("handler", toLower, Types.mono (Types.function Types.string Types.string))] $
            lambda "v1" $ match (Name "UnionMonomorphic") Nothing
              ["bool">: lambda "ignored" $ string "boolean value",
               "string">: lambda "v1" $ var "handler" @@ var "v1",
               "unit">: lambda "ignored" $ string "unit value"] @@ var "v1")

        expandsTo "bare primitive in case branch is expanded"
          (match (Name "UnionMonomorphic") Nothing
            ["bool">: lambda "ignored" "boolean value",
             "string">: toLower,
             "unit">: lambda "ignored" "unit value"])
          (lambda "v1" $ match (Name "UnionMonomorphic") Nothing
            ["bool">: lambda "ignored" "boolean value",
             "string">: lambda "v1" $ toLower @@ var "v1",
             "unit">: lambda "ignored" "unit value"] @@ var "v1")

        noChange "variable reference outside case branch is not expanded"
          (lets ["handler">: toLower] $ var "handler")

        noChange "bare primitive outside case branch is not expanded"
          toLower
  where
    cat = primitive $ Name "hydra.lib.strings.cat"
    empty = primitive $ Name "hydra.lib.maps.empty"
    foldl = primitive $ Name "hydra.lib.lists.foldl"
    toLower = primitive $ Name "hydra.lib.strings.toLower"
    splitOn = primitive $ Name "hydra.lib.strings.splitOn"

testLiterals :: H.SpecWith ()
testLiterals = do
  H.describe "Tests for literal values" $ do

    H.it "Literal terms have no free variables" $
      QC.property $ \av -> termIsClosed (literal av :: Term)

    H.it "Literal terms are fully reduced; check using a dedicated function" $
      QC.property $ \av -> termIsValue testGraph (literal av :: Term)

    H.it "Literal terms are fully reduced; check by trying to reduce them" $
      QC.property $ \av ->
        shouldSucceedWith
          (eval (literal av))
          (literal av :: Term)

    H.it "Literal terms cannot be applied" $
      QC.property $ \lv -> shouldSucceedWith
        (eval $ apply (literal lv) (literal lv))
        (apply (literal lv) (literal lv))

testMonomorphicPrimitives :: H.SpecWith ()
testMonomorphicPrimitives = do
  H.describe "Tests for monomorphic primitive functions" $ do

    H.it "Example primitives have the expected arity" $ do
      H.shouldBe
        (primitiveArity <$> lookupPrimitive testGraph _strings_toUpper)
        (Just 1)
      H.shouldBe
        (primitiveArity <$> lookupPrimitive testGraph _strings_splitOn)
        (Just 2)

    H.it "Simple applications of a unary function succeed" $
      QC.property $ \s ->
        shouldSucceedWith
          (eval (apply (primitive _strings_toUpper) $ string s))
          (string $ fmap C.toUpper s)

    H.it "Simple applications of a binary function succeed" $
      QC.property $ \i1 i2 ->
        shouldSucceedWith
          (eval (apply (apply (primitive _math_add) $ int32 i1) $ int32 i2))
          (int32 $ i1 + i2)

    H.it "Incomplete application of a primitive function leaves the term unchanged" $
      QC.property $ \s1 ->
        shouldSucceedWith
          (eval (apply (primitive _strings_splitOn) $ string s1))
          (apply (primitive _strings_splitOn) $ string s1)

    H.it "Extra arguments to a primitive function are tolerated" $
      QC.property $ \s1 s2 ->
        shouldSucceedWith
          (eval (apply (apply (primitive _strings_toUpper) $ string s1) $ string s2))
          (apply (string $ toUpper s1) (string s2))

testPolymorphicPrimitives :: H.SpecWith ()
testPolymorphicPrimitives = do
  H.describe "Tests for polymorphic primitive functions" $ do

    H.it "Test polymorphic list length" $ do
      QC.property $ \l ->
        shouldSucceedWith
          (eval (apply (primitive _lists_length) $ list l))
          (int32 $ L.length l)

testNullaryPrimitives :: H.SpecWith ()
testNullaryPrimitives = do
  H.describe "Tests for nullary primitives (constants)" $ do

    H.it "Test empty set constant" $ do
      -- shouldSucceedWith
      --   (eval (apply (primitive _sets_size) (Terms.set S.empty)))
      --   (int32 0)
      shouldSucceedWith
        (eval (apply (primitive _sets_size) (primitive _sets_empty)))
        (int32 0)

reduce :: Type -> Type
reduce typ = fromFlow typ (schemaContext testGraph) (betaReduceType typ)

spec :: H.Spec
spec = do
  testAlphaConversion
  testBetaReduceTypeRecursively
  testEtaExpansion
  testLiterals
  testMonomorphicPrimitives
  testPolymorphicPrimitives
  testNullaryPrimitives
