-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for core rewrite/fold combinators

module Hydra.Test.Rewriting where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for core rewrite/fold combinators
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "rewriting",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "foldOverTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from single node - pre-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList []))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from tree - pre-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermList [])),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralString "c"), (Core.TermList [
                    Core.TermPair (Core.TermLiteral (Core.LiteralString "d"), (Core.TermList []))])))]))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c")),
                  (Core.TermLiteral (Core.LiteralString "d"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from single node - post-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPost (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList []))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from tree - post-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPost (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermList [])),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralString "c"), (Core.TermList [
                    Core.TermPair (Core.TermLiteral (Core.LiteralString "d"), (Core.TermList []))])))]))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "b"),
                  (Core.TermLiteral (Core.LiteralString "d")),
                  (Core.TermLiteral (Core.LiteralString "c")),
                  (Core.TermLiteral (Core.LiteralString "a"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sum int32 literals",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))])))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 52))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths - pre-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralString "foo"),
                    (Core.TermLiteral (Core.LiteralString "bar"))],
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "quux")])}))]))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths - post-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPost (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralString "foo"),
                    (Core.TermLiteral (Core.LiteralString "bar"))],
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "quux")])}))]))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rewriteType",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in left side of either is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in right side of either is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String types in both sides of either are replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in nested either (left of left) is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in nested either (right of right) is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String types in complex nested either are all replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in list type is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in function domain is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in function codomain is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in optional type is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)))),
                Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rewriteTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string literal foo replaced with bar",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLiteral (Core.LiteralString "foo")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "bar")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in variable not changed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermVariable (Core.Name "x")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "bar"),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple strings in list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "foo")),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "bar"),
                  (Core.TermLiteral (Core.LiteralString "bar")),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in optional (just)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "foo")))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "bar")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in function application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "print")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "print")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in nested applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in record field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Person"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Person"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "strings in multiple record fields",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Data"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "a"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "b"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "c"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Data"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "a"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "b"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "c"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermPair (Core.TermLiteral (Core.LiteralString "foo"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermLiteral (Core.LiteralString "bar"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in let binding value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in let body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in first case branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in second case branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in default branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "foo"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "bar"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string deeply nested in record in list in application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "process")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "Item"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "value"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "process")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "Item"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "value"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in union inject value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = (Core.Name "Result"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "success"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = (Core.Name "Result"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "success"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in wrapped term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "Email"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "Email"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in annotated term body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in first of multiple let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in second of multiple let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "y"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in all let bindings and body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "baz"),
                  (Core.TermLiteral (Core.LiteralString "foo"))])))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "bar"),
                  (Core.TermLiteral (Core.LiteralString "baz"))])))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in type lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in type application body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in nested type lambdas",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in case branch within let binding",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "handler"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "handler"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in annotated wrapped record field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "User"),
                    Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "UserData"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]}))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "User"),
                    Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "UserData"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]}))})),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rewriteAndFoldTermWithPath",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through application - sum literals",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through nested applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "y"))])})))}))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 32)))])}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through record fields",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Point"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through case branches",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "ok"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "err"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through optional",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through wrapped term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "Age"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through type lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through type application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50))),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through set elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "deep nesting - application in lambda in let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths in nested structure",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths in let body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "xs"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
