{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Test.EtaExpansion (etaExpansionTests) where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Tests
import qualified Hydra.Dsl.Types as Types


etaExpansionTests :: TestGroup
etaExpansionTests = TestGroup "eta expansion tests" (Just "Test cases for eta expansion of terms") subgroups []
  where
    subgroups = [
      partialApplicationOfPrimitives,
      polymorphicTerms,
      higherOrderFunctions,
      letTerms,
      caseStatements,
      nonExpansionOfEliminations]

    partialApplicationOfPrimitives = TestGroup "Partial application of primitives" Nothing subgroups []
      where
        subgroups = [barePrimitives, partiallyAppliedPrimitives, fullyAppliedPrimitives, recordProjections]

        barePrimitives = TestGroup "Bare primitives are not expanded" Nothing [] [
          noChange "unary primitive"
            toLower,
          noChange "binary primitive"
            splitOn]

        partiallyAppliedPrimitives = TestGroup "Partially applied primitives expand with lambdas" Nothing [] [
          testCase "binary primitive with one argument"
            (splitOn @@ string "foo")
            (lambda "v1" $ splitOn @@ string "foo" @@ var "v1"),
          testCase "ternary primitive with one argument"
            (foldl @@ var "f")
            (lambda "v1" $ lambda "v2" $ foldl @@ var "f" @@ var "v1" @@ var "v2")]

        fullyAppliedPrimitives = TestGroup "Fully applied primitives are not expanded" Nothing [] [
          noChange "unary primitive"
            (toLower @@ string "FOO"),
          noChange "binary primitive"
            (splitOn @@ string "," @@ string "a,b,c")]

        recordProjections = TestGroup "Record projections" Nothing subgroups []
          where
            subgroups = [bareProjections, appliedProjections, nestedProjections, functionValuedProjections]

            bareProjections = TestGroup "Bare projections expand with a lambda" Nothing [] [
              testCase "projection without argument"
                (project (Name "Person") (Name "firstName"))
                (lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1")]

            appliedProjections = TestGroup "Applied projections are not expanded" Nothing [] [
              noChange "projection with argument"
                (project (Name "Person") (Name "firstName") @@ var "person"),
              noChange "projection applied to a record"
                (project (Name "Person") (Name "firstName") @@
                  record (Name "Person") ["firstName">: string "John", "lastName">: string "Doe"])]

            nestedProjections = TestGroup "Projections nested in other structures" Nothing [] [
              testCase "projection in a list"
                (list [project (Name "Person") (Name "firstName"), toLower])
                (list [lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1", toLower]),
              testCase "projection in a tuple"
                (tuple2 (project (Name "Person") (Name "firstName")) ( string "default"))
                (tuple2 (lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1") ( string "default")),
              testCase "projection in let binding"
                (lets ["getter">: project (Name "Person") (Name "firstName")] (var "getter"))
                (lets ["getter">: lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1"] (var "getter")),
              testCase "projection in lambda body"
                (lambda "x" $ project (Name "Person") (Name "firstName"))
                (lambda "x" $ lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1")]

            functionValuedProjections = TestGroup "Function-valued projections" Nothing [] [
              noChange "projection of function-valued field applied to arguments should not be expanded"
                (tyapps
                    (project (Name "Triple") (Name "first"))
                    [Types.function Types.string Types.string, Types.string, Types.string]
                  @@ tyapps
                    (record (Name "Triple") ["first">: primitive _strings_toLower, "second">: string "middle", "third">: string "last"])
                    [Types.function Types.string Types.string, Types.string, Types.string]
                  @@ string "DATA")]

    polymorphicTerms = TestGroup "Polymorphic terms (System F)" Nothing subgroups []
      where
        subgroups = [typeLambdasInLetBindings, typeApplicationsOfPolymorphicBindings]

        typeLambdasInLetBindings = TestGroup "Type lambdas in let bindings" Nothing [] [
          noChange "polymorphic identity function"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              (var "id")),
          testCase "monomorphic partially applied primitive"
            (letsTyped [("partial", splitOn @@ string "foo", Types.mono (Types.function Types.string (Types.list Types.string)))]
              (var "partial"))
            (letsTyped [("partial", lambda "v1" $ splitOn @@ string "foo" @@ var "v1", Types.mono (Types.function Types.string (Types.list Types.string)))]
              (var "partial")),
          testCase "monomorphic projection"
            (letsTyped [("getter", project (Name "Person") (Name "firstName"), Types.mono (Types.function (Types.var "Person") Types.string))]
              (var "getter"))
            (letsTyped [("getter", lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1", Types.mono (Types.function (Types.var "Person") Types.string))]
              (var "getter"))]

        typeApplicationsOfPolymorphicBindings = TestGroup "Type applications of polymorphic bindings" Nothing [] [
          noChange "polymorphic variable with type application"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              (var "id" `tyapp` Types.string)),
          noChange "type application of identity applied to binary function with no arguments"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              ((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn)),
          testCase "type application of identity applied to partially applied binary function"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              ((var "id" `tyapp` (Types.function Types.string (Types.list Types.string))) @@ (splitOn @@ string ",")))
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))] $
              (var "id" `tyapp` (Types.function Types.string (Types.list Types.string))) @@ (lambda "v1" $ splitOn @@ string "," @@ var "v1")),
          noChange "type application of identity applied to fully applied binary function"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              ((var "id" `tyapp` (Types.list Types.string)) @@ (splitOn @@ string "," @@ string "foo,bar"))),
          testCase "type application of identity applied to binary function, then applied to one argument"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              (((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ string ","))
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              (lambda "v1" $ (((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ string ",") @@ var "v1")),
          noChange "type application of identity applied to binary function, then fully applied to two arguments"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), Types.poly ["a"] (Types.function (Types.var "a") (Types.var "a")))]
              (((var "id" `tyapp` (Types.function Types.string (Types.function Types.string (Types.list Types.string)))) @@ splitOn) @@ string "," @@ string "foo,bar"))]

    higherOrderFunctions = TestGroup "Higher-Order Functions" Nothing subgroups []
      where
        subgroups = [functionsThatReturnFunctions]

        functionsThatReturnFunctions = TestGroup "Functions that return functions" Nothing [] [
          noChange "lambda returning bare binary primitive"
            (lambda "x" splitOn),
          noChange "lambda returning bare unary primitive"
            (lambda "x" toLower),
          testCase "lambda returning partially applied primitive"
            (lambda "x" $ splitOn @@ string ",")
            (lambda "x" $ lambda "v1" $ splitOn @@ string "," @@ var "v1"),
          noChange "lambda returning fully applied primitive"
            (lambda "x" $ splitOn @@ string "," @@ var "x"),
          testCase "lambda returning bare projection"
            (lambda "person" $ project (Name "Person") (Name "firstName"))
            (lambda "person" $ lambda "v1" $ project (Name "Person") (Name "firstName") @@ var "v1"),
          testCase "nested lambdas with partial application in body"
            (lambda "x" $ lambda "y" $ splitOn @@ var "x")
            (lambda "x" $ lambda "y" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1"),
          testCase "lambda returning lambda returning partial application"
            (lambda "x" $ lambda "y" $ lambda "z" $ splitOn @@ var "x")
            (lambda "x" $ lambda "y" $ lambda "z" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")]

    letTerms = TestGroup "Let terms" Nothing subgroups []
      where
        subgroups = [partialApplicationOfLetBoundFunction]

        partialApplicationOfLetBoundFunction = TestGroup "partial application of a let-bound function" Nothing [] [
          testCase "simple"
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
              var "helper" @@ string "foo")
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
              lambda "v1" $ lambda "v2" $ var "helper" @@ string "foo" @@ var "v1" @@ var "v2"),
          testCase "in a fold"
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
              (tyapps foldl [Types.string, Types.string])
                @@ (var "helper" @@ string "foo")
                @@ string ""
                @@ list ["bar", "baz"])
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
              (tyapps foldl [Types.string, Types.string])
                @@ (lambda "v1" $ lambda "v2" $ var "helper" @@ string "foo" @@ var "v1" @@ var "v2")
                @@ string ""
                @@ list ["bar", "baz"]),
          testCase "within another let binding"
            (letsTyped
              [("tryme",
                (letsTyped
                  [("helper",
                    lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                      cat @@ list [var "arg1", var "arg2", var "arg3"],
                    Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
                  var "helper" @@ string "foo"),
                Types.mono $ Types.functionMany [Types.string, Types.string, Types.string])] $
              unit)
            (letsTyped
              [("tryme",
                (letsTyped
                  [("helper",
                    lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                      cat @@ list [var "arg1", var "arg2", var "arg3"],
                    Types.mono $ Types.functionMany [Types.string, Types.string, Types.string, Types.string])] $
                  lambda "v1" $ lambda "v2" $ var "helper" @@ string "foo" @@ var "v1" @@ var "v2"),
                Types.mono $ Types.functionMany [Types.string, Types.string, Types.string])] $
              unit)]

    caseStatements = TestGroup "Case statements" Nothing subgroups []
      where
        subgroups = [monomorphicAtTopLevel, monomorphicInLetBinding, polymorphicInLetBinding, forcedExpansionInCaseBranches]

        monomorphicAtTopLevel = TestGroup "monomorphic at top level" Nothing [] [
          testCase "non-applied case statement"
            (match (Name "UnionMonomorphic")
              (Just $ string "other") [
              "string">: lambda "s" $ var "s"])
            (lambda "v1" $ (match (Name "UnionMonomorphic")
                (Just $ string "other") [
                "string">: lambda "s" $ var "s"])
              @@ var "v1"),
          noChange "applied case statement"
            ((match (Name "UnionMonomorphic")
                (Just $ string "other") [
                "string">: lambdaTyped "s" Types.string $ var "s"])
              @@ (inject (Name "UnionMonomorphic") "string" (string "foo"))),
          noChange "applied case statement in lambda"
            (lambdaTyped "x" (Types.var "UnionMonomorphic") $ (match (Name "UnionMonomorphic")
                (Just $ string "other") [
                "string">: lambdaTyped "s" Types.string $ var "s"])
              @@ var "x")]

        monomorphicInLetBinding = TestGroup "monomorphic in let binding" Nothing [] [
          testCase "non-applied case statement"
            (letsTyped [
                ("test",
                 match (Name "UnionMonomorphic")
                   (Just $ string "other") [
                   "string">: lambda "s" $ var "s"],
                 Types.mono $ Types.function (Types.var "UnionMonomorphic") Types.string)]
              ("ignored"))
            (letsTyped [
                ("test",
                 lambda "v1" $ match (Name "UnionMonomorphic")
                   (Just $ string "other") [
                   "string">: lambda "s" $ var "s"] @@ var "v1",
                 Types.mono $ Types.function (Types.var "UnionMonomorphic") Types.string)]
              ("ignored")),
          noChange "applied case statement"
            (letsTyped [
                ("test",
                 (match (Name "UnionMonomorphic")
                     (Just $ string "other") [
                     "string">: lambdaTyped "s" Types.string $ var "s"])
                   @@ (inject (Name "UnionMonomorphic") "string" (string "foo")),
                 Types.mono $ Types.string)]
              ("ignored")),
          noChange "applied case statement in lambda"
            (letsTyped [
                ("test",
                 lambdaTyped "x" (Types.var "UnionMonomorphic") $ (match (Name "UnionMonomorphic")
                     (Just $ string "other") [
                     "string">: lambdaTyped "s" Types.string $ var "s"])
                   @@ var "x",
                 Types.mono $ Types.function Types.string Types.string)]
              ("ignored"))]

        polymorphicInLetBinding = TestGroup "polymorphic in let binding" Nothing [] [
          testCase "non-applied UnionPolymorphicRecursive"
            (letsTyped [
                ("test",
                 tyapp (match (Name "UnionPolymorphicRecursive")
                   (Just $ string "other") [
                   "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32,
                 Types.mono $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) Types.string)] $
              var "test")
            (letsTyped [
                ("test",
                 lambda "v1" $ tyapp (match (Name "UnionPolymorphicRecursive")
                     (Just $ string "other") [
                     "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32
                   @@ var "v1",
                 Types.mono $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) Types.string)] $
              var "test"),
          noChange "applied UnionPolymorphicRecursive with int32"
            (letsTyped [
              ("test",
               tyapp (match (Name "UnionPolymorphicRecursive")
                   (Just $ string "other") [
                   "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32
                 @@ tyapp (inject (Name "UnionPolymorphicRecursive") "value" (int32 42)) Types.int32,
               Types.mono $ Types.string)] $
              var "test"),
          noChange "applied UnionPolymorphicRecursive with int32 in lambda"
            (letsTyped [
              ("test",
               lambdaTyped "x" (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) $
                 tyapp (match (Name "UnionPolymorphicRecursive")
                     (Just $ string "other") [
                     "value">: lambdaTyped "i" Types.int32 $ primitive _literals_showInt32 @@ var "i"]) Types.int32
                   @@ var "x",
               Types.mono $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") Types.int32) Types.string)] $
              var "test"),
          noChange "applied generic UnionPolymorphicRecursive in lambda"
            (tylam "t0" $ letsTyped [
              ("test",
               tylam "t1" $ lambdaTyped "x" (Types.apply (Types.var "UnionPolymorphicRecursive") (Types.var "t1")) $
                 tyapp (match (Name "UnionPolymorphicRecursive")
                     (Just $ string "other") [
                     "value">: lambdaTyped "ignored" (Types.var "t1") $ "foo"]) (Types.var "t1")
                   @@ var "x",
               Types.poly ["t1"] $ Types.function (Types.apply (Types.var "UnionPolymorphicRecursive") (Types.var "t1")) Types.string)] $
              tyapp (var "test") $ Types.var "t0")]

        forcedExpansionInCaseBranches = TestGroup "Forced expansion in case statement branches" Nothing [] [
          testCase "variable reference in case branch is expanded"
            (letsTyped [("handler", toLower, Types.mono (Types.function Types.string Types.string))] $
              match (Name "UnionMonomorphic") Nothing
                ["bool">: lambda "ignored" $ "boolean value",
                 "string">: var "handler",
                 "unit">: lambda "ignored" $ "unit value"])
            (letsTyped [("handler", toLower, Types.mono (Types.function Types.string Types.string))] $
              lambda "v1" $ match (Name "UnionMonomorphic") Nothing
                ["bool">: lambda "ignored" $ "boolean value",
                 "string">: lambda "v1" $ var "handler" @@ var "v1",
                 "unit">: lambda "ignored" $ "unit value"] @@ var "v1"),
          testCase "bare primitive in case branch is expanded"
            (match (Name "UnionMonomorphic") Nothing
              ["bool">: lambda "ignored" $ "boolean value",
               "string">: toLower,
               "unit">: lambda "ignored" $ "unit value"])
            (lambda "v1" $ match (Name "UnionMonomorphic") Nothing
              ["bool">: lambda "ignored" $ "boolean value",
               "string">: lambda "v1" $ toLower @@ var "v1",
               "unit">: lambda "ignored" $ "unit value"] @@ var "v1"),
          noChange "variable reference outside case branch is not expanded"
            (lets ["handler">: toLower] $ var "handler"),
          noChange "bare primitive outside case branch is not expanded"
            toLower]

    nonExpansionOfEliminations = TestGroup "Non-expansion of eliminations which produce functions" Nothing [] [
      noChange "applied case statement"
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
              @@ var "dir"),
      noChange "applied projection"
        (tyapps (project (Name "Triple") (Name "third")) [Types.int32, Types.int32, Types.function Types.string Types.string]
        @@ (record (Name "Triple") [
          "first">: int32 42,
          "second">: int32 137,
          "third">: lambda "s" $ toLower @@ var "s"]))]

    testCase name input output = TestCaseWithMetadata name tcase Nothing []
      where
        tcase = TestCaseEtaExpansion $ EtaExpansionTestCase input output

    noChange name term = testCase name term term

    cat = primitive $ Name "hydra.lib.strings.cat"
    foldl = primitive $ Name "hydra.lib.lists.foldl"
    splitOn = primitive $ Name "hydra.lib.strings.splitOn"
    toLower = primitive $ Name "hydra.lib.strings.toLower"
