
module Hydra.Sources.Test.EtaExpansion where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Prelude hiding (foldl)


ns :: Namespace
ns = Namespace "hydra.test.etaExpansion"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Test cases for eta expansion of terms")
  where
    elements = [
      Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = Phantoms.definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Test cases for eta expansion of terms" $
  testGroup (Phantoms.string "eta expansion") Phantoms.nothing (Phantoms.list subgroups) (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
  where
    subgroups = [
      partialApplicationOfPrimitives,
      polymorphicTerms,
      higherOrderFunctions,
      letTerms,
      caseStatements,
      nonExpansionOfEliminations]

    partialApplicationOfPrimitives = testGroup (Phantoms.string "Partial application of primitives") Phantoms.nothing (Phantoms.list subgroups') (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
      where
        subgroups' = [
          barePrimitives,
          partiallyAppliedPrimitives,
          fullyAppliedPrimitives,
          recordProjections]

        barePrimitives = testGroup (Phantoms.string "Bare primitives are not expanded") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          noChange "unary primitive"
            toLower,
          noChange "binary primitive"
            splitOn])

        partiallyAppliedPrimitives = testGroup (Phantoms.string "Partially applied primitives expand with lambdas") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          testCase "binary primitive with one argument"
            (splitOn @@ string "foo")
            (lambda "v1" $ splitOn @@ string "foo" @@ var "v1"),
          testCase "ternary primitive with one argument"
            (foldl @@ var "f")
            (lambda "v1" $ lambda "v2" $ foldl @@ var "f" @@ var "v1" @@ var "v2")])

        fullyAppliedPrimitives = testGroup (Phantoms.string "Fully applied primitives are not expanded") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          noChange "unary primitive"
            (toLower @@ string "FOO"),
          noChange "binary primitive"
            (splitOn @@ string "," @@ string "a,b,c")])

        recordProjections = testGroup (Phantoms.string "Record projections") Phantoms.nothing (Phantoms.list subgroups'') (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
          where
            subgroups'' = [
              bareProjections,
              appliedProjections,
              nestedProjections,
              functionValuedProjections]

            bareProjections = testGroup (Phantoms.string "Bare projections expand with a lambda") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
              testCase "projection without argument"
                (project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")))
                (lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1")])

            appliedProjections = testGroup (Phantoms.string "Applied projections are not expanded") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
              noChange "projection with argument"
                (project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "person"),
              noChange "projection applied to a record"
                (project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@
                  record TestTypes.testTypePersonName ["firstName">: string "John", "lastName">: string "Doe"])])

            nestedProjections = testGroup (Phantoms.string "Projections nested in other structures") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
              testCase "projection in a list"
                (list [project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")), toLower])
                (list [lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1", toLower]),
              testCase "projection in a tuple"
                (pair (project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName"))) (string "default"))
                (pair (lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1") (string "default")),
              testCase "projection in let binding"
                (lets ["getter">: project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName"))] (var "getter"))
                (lets ["getter">: lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1"] (var "getter")),
              testCase "projection in lambda body"
                (lambda "x" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")))
                (lambda "x" $ lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1")])

            functionValuedProjections = testGroup (Phantoms.string "Function-valued projections") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
              noChange "projection of function-valued field applied to arguments should not be expanded"
                (tyapps
                    (project TestTypes.testTypeTripleName (Core.name (Phantoms.string "first")))
                    [T.function T.string T.string, T.string, T.string]
                  @@ tyapps
                    (record TestTypes.testTypeTripleName ["first">: primitive _strings_toLower, "second">: string "middle", "third">: string "last"])
                    [T.function T.string T.string, T.string, T.string]
                  @@ string "DATA")])

    polymorphicTerms = testGroup (Phantoms.string "Polymorphic terms (System F)") Phantoms.nothing (Phantoms.list subgroups') (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
      where
        subgroups' = [
          typeLambdasInLetBindings,
          typeApplicationsOfPolymorphicBindings]

        typeLambdasInLetBindings = testGroup (Phantoms.string "Type lambdas in let bindings") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          noChange "polymorphic identity function"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              (var "id")),
          testCase "monomorphic partially applied primitive"
            (letsTyped [("partial", splitOn @@ string "foo", T.mono (T.function T.string (T.list T.string)))]
              (var "partial"))
            (letsTyped [("partial", lambda "v1" $ splitOn @@ string "foo" @@ var "v1", T.mono (T.function T.string (T.list T.string)))]
              (var "partial")),
          testCase "monomorphic projection"
            (letsTyped [("getter", project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")), T.mono (T.function (T.var "Person") T.string))]
              (var "getter"))
            (letsTyped [("getter", lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1", T.mono (T.function (T.var "Person") T.string))]
              (var "getter"))])

        typeApplicationsOfPolymorphicBindings = testGroup (Phantoms.string "Type applications of polymorphic bindings") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          noChange "polymorphic variable with type application"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              (var "id" `tyapp` T.string)),
          noChange "type application of identity applied to binary function with no arguments"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              ((var "id" `tyapp` (T.function T.string (T.function T.string (T.list T.string)))) @@ splitOn)),
          testCase "type application of identity applied to partially applied binary function"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              ((var "id" `tyapp` (T.function T.string (T.list T.string))) @@ (splitOn @@ string ",")))
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))] $
              (var "id" `tyapp` (T.function T.string (T.list T.string))) @@ (lambda "v1" $ splitOn @@ string "," @@ var "v1")),
          noChange "type application of identity applied to fully applied binary function"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              ((var "id" `tyapp` (T.list T.string)) @@ (splitOn @@ string "," @@ string "foo,bar"))),
          testCase "type application of identity applied to binary function, then applied to one argument"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              (((var "id" `tyapp` (T.function T.string (T.function T.string (T.list T.string)))) @@ splitOn) @@ string ","))
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              (lambda "v1" $ (((var "id" `tyapp` (T.function T.string (T.function T.string (T.list T.string)))) @@ splitOn) @@ string ",") @@ var "v1")),
          noChange "type application of identity applied to binary function, then fully applied to two arguments"
            (letsTyped [("id", tylam "a" (lambda "x" $ var "x"), T.poly ["a"] (T.function (T.var "a") (T.var "a")))]
              (((var "id" `tyapp` (T.function T.string (T.function T.string (T.list T.string)))) @@ splitOn) @@ string "," @@ string "foo,bar"))])

    higherOrderFunctions = testGroup (Phantoms.string "Higher-Order Functions") Phantoms.nothing (Phantoms.list subgroups') (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
      where
        subgroups' = [functionsThatReturnFunctions]

        functionsThatReturnFunctions = testGroup (Phantoms.string "Functions that return functions") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
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
            (lambda "person" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")))
            (lambda "person" $ lambda "v1" $ project TestTypes.testTypePersonName (Core.name (Phantoms.string "firstName")) @@ var "v1"),
          testCase "nested lambdas with partial application in body"
            (lambda "x" $ lambda "y" $ splitOn @@ var "x")
            (lambda "x" $ lambda "y" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1"),
          testCase "lambda returning lambda returning partial application"
            (lambda "x" $ lambda "y" $ lambda "z" $ splitOn @@ var "x")
            (lambda "x" $ lambda "y" $ lambda "z" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")])

    letTerms = testGroup (Phantoms.string "Let terms") Phantoms.nothing (Phantoms.list subgroups') (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
      where
        subgroups' = [partialApplicationOfLetBoundFunction]

        partialApplicationOfLetBoundFunction = testGroup (Phantoms.string "partial application of a let-bound function") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          testCase "simple"
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                T.mono $ T.functionMany [T.string, T.string, T.string, T.string])] $
              var "helper" @@ string "foo")
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                T.mono $ T.functionMany [T.string, T.string, T.string, T.string])] $
              lambda "v1" $ lambda "v2" $ var "helper" @@ string "foo" @@ var "v1" @@ var "v2"),
          testCase "in a fold"
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                T.mono $ T.functionMany [T.string, T.string, T.string, T.string])] $
              (tyapps foldl [T.string, T.string])
                @@ (var "helper" @@ string "foo")
                @@ string ""
                @@ list [string "bar", string "baz"])
            (letsTyped
              [("helper",
                lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                  cat @@ list [var "arg1", var "arg2", var "arg3"],
                T.mono $ T.functionMany [T.string, T.string, T.string, T.string])] $
              (tyapps foldl [T.string, T.string])
                @@ (lambda "v1" $ lambda "v2" $ var "helper" @@ string "foo" @@ var "v1" @@ var "v2")
                @@ string ""
                @@ list [string "bar", string "baz"]),
          testCase "within another let binding"
            (letsTyped
              [("tryme",
                (letsTyped
                  [("helper",
                    lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                      cat @@ list [var "arg1", var "arg2", var "arg3"],
                    T.mono $ T.functionMany [T.string, T.string, T.string, T.string])] $
                  var "helper" @@ string "foo"),
                T.mono $ T.functionMany [T.string, T.string, T.string])] $
              unit)
            (letsTyped
              [("tryme",
                (letsTyped
                  [("helper",
                    lambda "arg1" $ lambda "arg2" $ lambda "arg3" $
                      cat @@ list [var "arg1", var "arg2", var "arg3"],
                    T.mono $ T.functionMany [T.string, T.string, T.string, T.string])] $
                  lambda "v1" $ lambda "v2" $ var "helper" @@ string "foo" @@ var "v1" @@ var "v2"),
                T.mono $ T.functionMany [T.string, T.string, T.string])] $
              unit)])

    caseStatements = testGroup (Phantoms.string "Case statements") Phantoms.nothing (Phantoms.list subgroups') (Phantoms.list ([] :: [TTerm TestCaseWithMetadata]))
      where
        subgroups' = [
          monomorphicAtTopLevel,
          monomorphicInLetBinding,
          polymorphicInLetBinding,
          forcedExpansionInCaseBranches]

        monomorphicAtTopLevel = testGroup (Phantoms.string "monomorphic at top level") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          testCase "non-applied case statement"
            (match TestTypes.testTypeUnionMonomorphicName
              (just $ string "other") [
              "string">: lambda "s" $ var "s"])
            (lambda "v1" $ (match TestTypes.testTypeUnionMonomorphicName
                (just $ string "other") [
                "string">: lambda "s" $ var "s"])
              @@ var "v1"),
          noChange "applied case statement"
            ((match TestTypes.testTypeUnionMonomorphicName
                (just $ string "other") [
                "string">: lambdaTyped "s" T.string $ var "s"])
              @@ (inject TestTypes.testTypeUnionMonomorphicName "string" (string "foo"))),
          noChange "applied case statement in lambda"
            (lambdaTyped "x" (Core.typeVariable TestTypes.testTypeUnionMonomorphicName) $ (match TestTypes.testTypeUnionMonomorphicName
                (just $ string "other") [
                "string">: lambdaTyped "s" T.string $ var "s"])
              @@ var "x")])

        monomorphicInLetBinding = testGroup (Phantoms.string "monomorphic in let binding") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          testCase "non-applied case statement"
            (letsTyped [
                ("test",
                 match TestTypes.testTypeUnionMonomorphicName
                   (just $ string "other") [
                   "string">: lambda "s" $ var "s"],
                 T.mono $ T.function (Core.typeVariable TestTypes.testTypeUnionMonomorphicName) T.string)]
              (string "ignored"))
            (letsTyped [
                ("test",
                 lambda "v1" $ match TestTypes.testTypeUnionMonomorphicName
                   (just $ string "other") [
                   "string">: lambda "s" $ var "s"] @@ var "v1",
                 T.mono $ T.function (Core.typeVariable TestTypes.testTypeUnionMonomorphicName) T.string)]
              (string "ignored")),
          noChange "applied case statement"
            (letsTyped [
                ("test",
                 (match TestTypes.testTypeUnionMonomorphicName
                     (just $ string "other") [
                     "string">: lambdaTyped "s" T.string $ var "s"])
                   @@ (inject TestTypes.testTypeUnionMonomorphicName "string" (string "foo")),
                 T.mono $ T.string)]
              (string "ignored")),
          noChange "applied case statement in lambda"
            (letsTyped [
                ("test",
                 lambdaTyped "x" (Core.typeVariable TestTypes.testTypeUnionMonomorphicName) $ (match TestTypes.testTypeUnionMonomorphicName
                     (just $ string "other") [
                     "string">: lambdaTyped "s" T.string $ var "s"])
                   @@ var "x",
                 T.mono $ T.function T.string T.string)]
              (string "ignored"))])

        polymorphicInLetBinding = testGroup (Phantoms.string "polymorphic in let binding") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          testCase "non-applied UnionPolymorphicRecursive"
            (letsTyped [
                ("test",
                 tyapp (match TestTypes.testTypeUnionPolymorphicRecursiveName
                   (just $ string "other") [
                   "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32,
                 T.mono $ T.function (T.apply (Core.typeVariable TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string)] $
              var "test")
            (letsTyped [
                ("test",
                 lambda "v1" $ tyapp (match TestTypes.testTypeUnionPolymorphicRecursiveName
                     (just $ string "other") [
                     "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
                   @@ var "v1",
                 T.mono $ T.function (T.apply (Core.typeVariable TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string)] $
              var "test"),
          noChange "applied UnionPolymorphicRecursive with int32"
            (letsTyped [
              ("test",
               tyapp (match TestTypes.testTypeUnionPolymorphicRecursiveName
                   (just $ string "other") [
                   "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
                 @@ tyapp (inject TestTypes.testTypeUnionPolymorphicRecursiveName "value" (int32 42)) T.int32,
               T.mono $ T.string)] $
              var "test"),
          noChange "applied UnionPolymorphicRecursive with int32 in lambda"
            (letsTyped [
              ("test",
               lambdaTyped "x" (T.apply (Core.typeVariable TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) $
                 tyapp (match TestTypes.testTypeUnionPolymorphicRecursiveName
                     (just $ string "other") [
                     "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
                   @@ var "x",
               T.mono $ T.function (T.apply (Core.typeVariable TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string)] $
              var "test"),
          noChange "applied generic UnionPolymorphicRecursive in lambda"
            (tylam "t0" $ letsTyped [
              ("test",
               tylam "t1" $ lambdaTyped "x" (T.apply (Core.typeVariable TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t1")) $
                 tyapp (match TestTypes.testTypeUnionPolymorphicRecursiveName
                     (just $ string "other") [
                     "value">: lambdaTyped "ignored" (T.var "t1") $ string "foo"]) (T.var "t1")
                   @@ var "x",
               T.poly ["t1"] $ T.function (T.apply (Core.typeVariable TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t1")) T.string)] $
              tyapp (var "test") $ T.var "t0")])

        forcedExpansionInCaseBranches = testGroup (Phantoms.string "Forced expansion in case statement branches") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
          testCase "variable reference in case branch is expanded"
            (letsTyped [("handler", toLower, T.mono (T.function T.string T.string))] $
              match TestTypes.testTypeUnionMonomorphicName nothing
                ["bool">: lambda "ignored" $ string "boolean value",
                 "string">: var "handler",
                 "unit">: lambda "ignored" $ string "unit value"])
            (letsTyped [("handler", toLower, T.mono (T.function T.string T.string))] $
              lambda "v1" $ match TestTypes.testTypeUnionMonomorphicName nothing
                ["bool">: lambda "ignored" $ string "boolean value",
                 "string">: lambda "v1" $ var "handler" @@ var "v1",
                 "unit">: lambda "ignored" $ string "unit value"] @@ var "v1"),
          testCase "bare primitive in case branch is expanded"
            (match TestTypes.testTypeUnionMonomorphicName nothing
              ["bool">: lambda "ignored" $ string "boolean value",
               "string">: toLower,
               "unit">: lambda "ignored" $ string "unit value"])
            (lambda "v1" $ match TestTypes.testTypeUnionMonomorphicName nothing
              ["bool">: lambda "ignored" $ string "boolean value",
               "string">: lambda "v1" $ toLower @@ var "v1",
               "unit">: lambda "ignored" $ string "unit value"] @@ var "v1"),
          noChange "variable reference outside case branch is not expanded"
            (lets ["handler">: toLower] $ var "handler"),
          noChange "bare primitive outside case branch is not expanded"
            toLower])

    nonExpansionOfEliminations = testGroup (Phantoms.string "Non-expansion of eliminations which produce functions") Phantoms.nothing (Phantoms.list ([] :: [TTerm TestGroup])) (Phantoms.list [
      noChange "applied case statement"
        (tylams ["t0", "t1"] $
          lambdaTyped "dir" (T.var "hydra.coders.CoderDirection") $
            lambdaTyped "coder" (T.applys (T.var "hydra.compute.Coder") (T.var <$> ["t0", "t0", "t1", "t1"])) $
              match (Core.nameLift _CoderDirection)
                nothing [
                "encode">: lambdaTyped "_" T.unit $
                  lambdaTyped "v12" (T.var "t1") $
                    tyapps (project (Core.nameLift _Coder) (Core.name (Phantoms.string "encode"))) (T.var <$> ["t0", "t0", "t1", "t1"])
                      @@ var "coder" @@ var "v12",
                "decode">: lambdaTyped "_" T.unit $
                  lambdaTyped "v12" (T.var "t1") $
                    tyapps (project (Core.nameLift _Coder) (Core.name (Phantoms.string "decode"))) (T.var <$> ["t0", "t0", "t1", "t1"])
                      @@ var "coder" @@ var "v12"]
              @@ var "dir"),
      noChange "applied projection"
        (tyapps (project TestTypes.testTypeTripleName (Core.name (Phantoms.string "third"))) [T.int32, T.int32, T.function T.string T.string]
        @@ (record TestTypes.testTypeTripleName [
          "first">: int32 42,
          "second">: int32 137,
          "third">: lambda "s" $ toLower @@ var "s"]))])

-- Helpers

testCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
testCase name input output = testCaseWithMetadata (Phantoms.string name) tcase Phantoms.nothing (Phantoms.list ([] :: [TTerm Tag]))
  where
    tcase = testCaseEtaExpansion $ etaExpansionTestCase input output

noChange :: String -> TTerm Term -> TTerm TestCaseWithMetadata
noChange name term = testCase name term term

cat = primitive $ _strings_cat
foldl = primitive $ _lists_foldl
splitOn = primitive $ _strings_splitOn
toLower = primitive $ _strings_toLower
--toLower = Core.termFunction $ Core.functionPrimitive $ Core.name (Phantoms.string "hydra.lib.strings.toLower")
