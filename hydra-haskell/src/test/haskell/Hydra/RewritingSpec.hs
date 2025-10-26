{-# LANGUAGE OverloadedStrings #-}

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.RewritingSpec.spec
-}
module Hydra.RewritingSpec where

import Hydra.Kernel
import Hydra.Monads
import Hydra.Tools.Monads
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Show.Core as ShowCore

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data Quux a = QuuxUnit | QuuxValue a | QuuxPair (Quux a) (Quux a) deriving (Eq, Ord, Show)

fsubQuux :: (a -> b) -> (Quux a -> Quux b) -> Quux a -> Quux b
fsubQuux mf recurse q = case q of
  QuuxUnit -> QuuxUnit
  QuuxValue x -> QuuxValue $ mf x
  QuuxPair left right -> QuuxPair (recurse left) (recurse right)

rewriteQuux :: (a -> b) -> ((Quux a -> Quux b) -> Quux a -> Quux b) -> Quux a -> Quux b
rewriteQuux mf f = rewrite (fsubQuux mf) f

myQuuxRewriter :: Quux String -> Quux Int
myQuuxRewriter = rewriteQuux L.length $ \fsub q -> fsub $ case q of
  QuuxPair left right -> QuuxPair QuuxUnit right
  _ -> q

checkFoldOverTerm :: H.SpecWith ()
checkFoldOverTerm = do
  H.describe "Test foldOverTerm" $ do
    H.describe "Pre-order" $ do
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPre node1)
          ["a"]
      H.it "test #2" $
        H.shouldBe
          (traverse TraversalOrderPre node2)
          ["a", "b", "c", "d"]
    H.describe "Post-order" $ do
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPost node1)
          ["a"]
      H.it "test #1" $
        H.shouldBe
          (traverse TraversalOrderPost node2)
          ["b", "d", "c", "a"]
  where
    node label children = Terms.pair (Terms.string label) (Terms.list children)
    labelOf term = case term of
      TermProduct [TermLiteral (LiteralString label), _] -> Just label
      _ -> Nothing
    traverse :: TraversalOrder -> Term -> [String]
    traverse order = Y.catMaybes . foldOverTerm order (\l t -> l ++ [labelOf t]) []
    node1 = node "a" []
    node2 = node "a" [node "b" [], node "c" [node "d" []]]

checkStripTerm :: H.SpecWith ()
checkStripTerm = do
  H.describe "Tests for stripping annotations from terms" $ do
    H.it "Un-annotated terms are not affected" $
      QC.property $ \term -> case (term :: Term) of
        TermAnnotated _ -> True
        _ -> deannotateTerm term == term
    H.it "Terms are stripped recursively" $
      QC.property $ \term -> case (term :: Term) of
        TermAnnotated _ -> True
        _ -> deannotateTerm (Terms.annot M.empty (Terms.annot M.empty term)) == term

checkStripType :: H.SpecWith ()
checkStripType = do
  H.describe "Tests for stripping annotations from types" $ do
    H.it "Un-annotated types are not affected" $
      QC.property $ \typ -> case (typ :: Type) of
        TypeAnnotated _ -> True
        _ -> deannotateType typ == typ
    H.it "Types are stripped recursively" $
      QC.property $ \typ -> case (typ :: Type) of
        TypeAnnotated _ -> True
        _ -> deannotateType (Types.annot M.empty (Types.annot M.empty typ)) == typ

testEtaExpandTypedTerms :: Graph -> H.SpecWith ()
testEtaExpandTypedTerms g = do
  H.describe "Test eta expansion of typed terms" $ do

    H.describe "Try terms which do not expand" $ do
      noChange "test #1"
        (int32 42)
      noChange "test #2"
        (list ["foo", "bar"])
      noChange "test #3"
        (splitOn @@ "foo" @@ "bar")
      noChange "test #4"
        (lambda "x" $ lambda "y" $ splitOn @@ var "x" @@ var "y")
      noChange "test #5"
        (lambda "x" $ int32 42)

    H.describe "Try bare function terms" $ do
      noChange "test #1"
        toLower
--      expandsTo "test #1"
--        toLower
--        (lambda "v1" $ toLower @@ var "v1")
      noChange "test #2"
         splitOn
--      expandsTo "test #2"
--        splitOn
--        (lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2")
      expandsTo "test #3"
        (splitOn @@ string "foo")
        (lambda "v1" $ splitOn @@ string "foo" @@ var "v1")
      noChange "test #4"
        (splitOn @@ string "foo" @@ string "bar")
      expandsTo "test #5"
        (primitive _optionals_maybe @@ (int32 42) @@ length)
        (lambda "v1" $ primitive _optionals_maybe @@ (int32 42) @@ length @@ var "v1")
--        (lambda "v1" $ (primitive _optionals_maybe @@ (int32 42) @@ (lambda "v1" $ length @@ var "v1")) @@ var "v1")
      expandsTo "test #6"
        (project (Name "Person") (Name "firstName"))
        (lambda "v1" $ ((project (Name "Person") (Name "firstName") @@ var "v1")))
      -- TODO: case statement

    H.describe "Try subterms within applications" $ do
      expandsTo "test #1"
        (splitOn @@ "bar")
        (lambda "v1" $ splitOn @@ "bar" @@ var "v1")
      expandsTo "test #2"
        (lambda "x" $ splitOn @@ var "x")
        (lambda "x" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")
      expandsTo "test #3"
        ((lambda "x" $ var "x") @@ length)
        (lambda "v1" $ ((lambda "x" $ var "x") @@ length) @@ var "v1")
--      noChange "test #3"
--        ((lambda "x" $ var "x") @@ length)
--      expandsTo "test #3"
--        ((lambda "x" $ var "x") @@ length)
--        (lambda "v1" $ length @@ var "v1")

    H.describe "Try let terms" $ do
      noChange "test #1"
        (lets ["foo">: int32 137] $ int32 42)
      noChange "test #2"
        (lets ["foo">: splitOn] $ var "foo")
--      expandsTo "test #2"
--        (lets ["foo">: splitOn] $ var "foo")
--        (lets ["foo">: lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2"] $ var "foo")

    H.describe "Check that complete applications are no-ops" $ do
      noChange "test #1"
        (toLower @@ "FOO")
      noChange "test #2"
        (splitOn @@ "foo" @@ "bar")

    H.describe "Try other subterms" $ do
      expandsTo "test #1"
        (list [lambda "x" $ list ["foo"], splitOn @@ "bar"])
        (list [lambda "x" $ list ["foo"], lambda "v1" $ splitOn @@ "bar" @@ var "v1"])
  where
    length = primitive $ Name "hydra.lib.strings.length"
    splitOn = primitive $ Name "hydra.lib.strings.splitOn"
    toLower = primitive $ Name "hydra.lib.strings.toLower"
    fromList = primitive $ Name "hydra.lib.sets.fromList"
    expandsTo desc termBefore termAfter = expectEtaExpansionResult desc termBefore termAfter
    noChange desc term = expandsTo desc term term

testEtaExpandUntypedTerms :: Graph -> H.SpecWith ()
testEtaExpandUntypedTerms g = do
  H.describe "Test eta expansion of untyped terms" $ do

    H.describe "Try terms which do not expand" $ do
      H.it "test #1" $
        noChange (int32 42)
      H.it "test #2" $
        noChange (list ["foo", "bar"])
      H.it "test #3" $
        noChange (splitOn @@ "foo" @@ "bar")
      H.it "test #4" $
        noChange (lambda "x" $ lambda "y" $ splitOn @@ var "x" @@ var "y")
      H.it "test #5" $
        noChange (lambda "x" $ int32 42)

    H.describe "Try bare function terms" $ do
      H.it "test #1" $
        expandsTo
          toLower
          (lambda "v1" $ toLower @@ var "v1")
      H.it "test #2" $
        expandsTo
          splitOn
          (lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2")
      H.it "test #3" $
        expandsTo
          (splitOn @@ var "foo")
          (lambda "v1" $ splitOn @@ var "foo" @@ var "v1")
      H.it "test #4" $
        expandsTo
          (splitOn @@ var "foo" @@ var "bar")
          (splitOn @@ var "foo" @@ var "bar")
      H.it "test #5" $
        expandsTo
          (splitOn @@ var "foo" @@ var "bar" @@ var "baz")
          (splitOn @@ var "foo" @@ var "bar" @@ var "baz")
      H.it "test #6" $
        expandsTo
          (primitive _optionals_maybe @@ (int32 42) @@ length)
          -- Note two levels of lambda expansion
          (lambda "v1" $ (primitive _optionals_maybe @@ (int32 42) @@ (lambda "v1" $ length @@ var "v1")) @@ var "v1")
      H.it "test #7" $
        expandsTo
          (project (Name "Person") (Name "firstName"))
          (lambda "v1" $ ((project (Name "Person") (Name "firstName") @@ var "v1")))
      -- TODO: case statement

    H.describe "Try subterms within applications" $ do
      H.it "test #1" $
        expandsTo
          (splitOn @@ "bar")
          (lambda "v1" $ splitOn @@ "bar" @@ var "v1")
      H.it "test #2" $
        expandsTo
          (lambda "x" $ splitOn @@ var "x")
          (lambda "x" $ lambda "v1" $ splitOn @@ var "x" @@ var "v1")
      H.it "test #3" $
        expandsTo
          ((lambda "x" $ var "x") @@ length)
          (lambda "v1" $ length @@ var "v1")

    H.describe "Try let terms" $ do
      H.it "test #1" $
        noChange
          (lets ["foo">: int32 137] $ int32 42)
      H.it "test #2" $
        expandsTo
          (lets ["foo">: splitOn] $ var "foo")
          (lets ["foo">: lambda "v1" $ lambda "v2" $ splitOn @@ var "v1" @@ var "v2"] $ var "foo")

    H.describe "Check that complete applications are no-ops" $ do
      H.it "test #1" $
        noChange
          (toLower @@ "FOO")
      H.it "test #2" $
        noChange
          (splitOn @@ "foo" @@ "bar")

    H.describe "Try other subterms" $ do
      H.it "test #1" $
        expandsTo
          (list [lambda "x" $ list ["foo"], splitOn @@ "bar"])
          (list [lambda "x" $ list ["foo"], lambda "v1" $ splitOn @@ "bar" @@ var "v1"])

    H.it "Check that lambda expansion is idempotent" $ do
      QC.property $ \term -> do
        let once = etaExpandTerm g term
        let twice = etaExpandTerm g once
        H.shouldBe once twice
  where
    length = primitive $ Name "hydra.lib.strings.length"
    splitOn = primitive $ Name "hydra.lib.strings.splitOn"
    toLower = primitive $ Name "hydra.lib.strings.toLower"
    fromList = primitive $ Name "hydra.lib.sets.fromList"
    expandsTo termBefore termAfter = do
       let result = etaExpandTerm g termBefore
       H.shouldBe (ShowCore.term result) (ShowCore.term termAfter)
    noChange term = expandsTo term term

testFoldOverTerm :: H.SpecWith ()
testFoldOverTerm = do
  H.describe "Test folding over terms" $ do

    H.it "Try a simple fold" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre adds 0
          (list [int32 42, (lambda "x" $ var "x") @@ int32 10]))
        52

    H.it "Check that traversal order is respected" $ do
      H.shouldBe
        (foldOverTerm TraversalOrderPre listLengths []
          (list [list [string "foo", string "bar"], (lambda "x" $ var "x") @@ (list [string "quux"])]))
        [1, 2, 2]
      H.shouldBe
        (foldOverTerm TraversalOrderPost listLengths []
          (list [list [string "foo", string "bar"], (lambda "x" $ var "x") @@ (list [string "quux"])]))
        [2, 1, 2]
  where
    adds sum term = case term of
      TermLiteral (LiteralInteger (IntegerValueInt32 i)) -> sum + i
      _ -> sum
    listLengths l term = case term of
      TermList els -> L.length els:l
      _ -> l

testFlattenLetTerms :: H.SpecWith ()
testFlattenLetTerms = do
  H.describe "Test flattening of 'let' terms" $ do

    H.it "Non-let terms are unaffected" $ do
      H.shouldBe
        (flattenLetTerms $ Terms.int32 42)
        (Terms.int32 42)
      H.shouldBe
        (flattenLetTerms $ Terms.list [Terms.string "foo"])
        (Terms.list [Terms.string "foo"])

    H.it "Non-nested let terms are unaffected" $
      H.shouldBe
        (flattenLetTerms letTerm1)
        (letTerm1)

    H.it "Nonrecursive, nested bindings are flattened" $
      H.shouldBe
        (flattenLetTerms letTerm2)
        (letTerm2_flattened)

    H.it "Multiple levels of nesting are flattened appropriately" $
      H.shouldBe
        (flattenLetTerms letTerm3)
        (letTerm3_flattened)
  where
    makeLet body pairs = TermLet $ Let (makeBinding <$> pairs) body
      where
        makeBinding (k, v) = Binding (Name k) v Nothing
    letTerm1 = makeLet (TermList [Terms.var "x", Terms.var "y"]) [
      ("x", Terms.int32 1),
      ("y", Terms.int32 2)]
    letTerm2 = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", letTerm1)]
    letTerm2_flattened = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", TermList [Terms.var "b_x", Terms.var "b_y"]),
      ("b_x", Terms.int32 1),
      ("b_y", Terms.int32 2)]
    letTerm3 = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", makeLet (TermList [Terms.var "x", Terms.var "y"]) [
        ("x", Terms.int32 1),
        ("y", makeLet (TermList [Terms.var "a", Terms.var "q"]) [
          ("p", Terms.int32 137),
          ("q", TermList [Terms.var "x", Terms.int32 5])])])]
    letTerm3_flattened = makeLet (TermList [Terms.var "a", Terms.var "b"]) [
      ("a", Terms.int32 1),
      ("b", TermList [Terms.var "b_x", Terms.var "b_y"]),
      ("b_x", Terms.int32 1),
      ("b_y", TermList [Terms.var "a", Terms.var "b_y_q"]),
      ("b_y_p", Terms.int32 137),
      ("b_y_q", TermList [Terms.var "b_x", Terms.int32 5])]

testFreeVariablesInTerm :: H.SpecWith ()
testFreeVariablesInTerm = do
  H.describe "Test free variables" $ do

--    H.it "Generated terms never have free variables" $ do
--      QC.property $ \(TypeApplicationTerm term _) -> do
--        H.shouldBe
--          (freeVariablesInTerm (term))
--          S.empty

    H.it "Free variables in individual terms" $ do
      H.shouldBe
        (freeVariablesInTerm (string "foo"))
        S.empty
      H.shouldBe
        (freeVariablesInTerm (var "x"))
        (S.fromList [Name "x"])
      H.shouldBe
        (freeVariablesInTerm (list [var "x", (lambda "y" $ var "y") @@ int32 42]))
        (S.fromList [Name "x"])
      H.shouldBe
        (freeVariablesInTerm (list [var "x", (lambda "y" $ var "y") @@ var "y"]))
        (S.fromList [Name "x", Name "y"])

testLiftLambdaAboveLet :: H.SpecWith ()
testLiftLambdaAboveLet = H.describe "liftLambdaAboveLet" $ do
    H.describe "Basic lambda lifting" $ do
      liftsTo "simple let with lambda in body"
        (lets ["x">: int32 42] (lambda "y" $ var "x"))
        (lambda "y" $ lets ["x">: int32 42] (var "x"))

      liftsTo "let with lambda using both let-bound and lambda-bound variables"
        (lets ["x">: int32 42] $ lambda "y" $ add @@ var "x" @@ var "y")
        (lambda "y" $ lets ["x">: int32 42] $ add @@ var "x" @@ var "y")

    H.describe "No transformation needed" $ do
      noChange "bare lambda"
        (lambda "x" $ var "x")

      noChange "bare let"
        (lets ["x">: int32 42] (var "x"))

      noChange "let without lambda in body"
        (lets ["x">: int32 42, "y">: string "hello"] (pair (var "x") (var "y")))

      noChange "lambda with let in body"
        (lambda "y" $ lets ["x">: int32 42] (var "x"))

    H.describe "Multiple lambdas and multiple lets" $ do
        liftsTo "let with two nested lambdas"
          (lets ["x">: int32 42] $ lambda "y" $ lambda "z" $ add @@ var "x" @@ var "y")
          (lambda "y" $ lambda "z" $ lets ["x">: int32 42] $ add @@ var "x" @@ var "y")

        liftsTo "multiple let bindings with lambda"
          (lets ["x">: int32 42, "y">: string "hello"] $ lambda "z" $ var "x")
          (lambda "z" $ lets ["x">: int32 42, "y">: string "hello"] $ var "x")

        liftsTo "nested lets with lambda at innermost level"
          (lets ["x">: int32 42] $ lets ["y">: string "hello"] $ lambda "z" $ var "x")
          (lambda "z" $ lets ["x">: int32 42] $ lets ["y">: string "hello"] $ var "x")

        liftsTo "lambda between two lets"
          (lets ["x">: int32 42] $ lambda "y" $ lets ["z">: string "hello"] $ var "x")
          (lambda "y" $ lets ["x">: int32 42] $ lets ["z">: string "hello"] $ var "x")

        liftsTo "multiple lambdas between nested lets"
          (lets ["a">: int32 1] $ lambda "x" $ lambda "y" $ lets ["b">: int32 2] $ var "a")
          (lambda "x" $ lambda "y" $ lets ["a">: int32 1] $ lets ["b">: int32 2] $ var "a")

        noChange "multiple lambdas already above let"
          (lambda "x" $ lambda "y" $ lets ["z">: int32 42] $ var "z")

    H.describe "Annotations" $ do
        liftsTo "annotation above let containing lambda"
          (annot (M.fromList [(Name "comment", string "outer")]) $
            lets ["x">: int32 42] $ lambda "y" $ var "x")
          (annot (M.fromList [(Name "comment", string "outer")]) $
            lambda "y" $ lets ["x">: int32 42] $ var "x")

        liftsTo "annotation above lambda in let body"
          (lets ["x">: int32 42] $
            annot (M.fromList [(Name "comment", string "inner")]) $
            lambda "y" $ var "x")
          (annot (M.fromList [(Name "comment", string "inner")]) $
            lambda "y" $ lets ["x">: int32 42] $ var "x")

        liftsTo "annotation between two lambdas"
          (lets ["x">: int32 42] $ lambda "y" $
            annot (M.fromList [(Name "comment", string "between")]) $
            lambda "z" $ var "x")
          (lambda "y" $
            annot (M.fromList [(Name "comment", string "between")]) $
            lambda "z" $ lets ["x">: int32 42] $ var "x")

        liftsTo "multiple annotations with lambda and let"
          (annot (M.fromList [(Name "outer", string "1")]) $
            lets ["x">: int32 42] $
            annot (M.fromList [(Name "inner", string "2")]) $
            lambda "y" $ var "x")
          (annot (M.fromList [(Name "outer", string "1")]) $
            annot (M.fromList [(Name "inner", string "2")]) $
            lambda "y" $ lets ["x">: int32 42] $ var "x")

        liftsTo "annotation on the body of lambda in let"
          (lets ["x">: int32 42] $ lambda "y" $
            annot (M.fromList [(Name "comment", string "body")]) $
            var "x")
          (lambda "y" $ lets ["x">: int32 42] $
            annot (M.fromList [(Name "comment", string "body")]) $
            var "x")

        liftsTo "annotation between nested lets"
          (lets ["x">: int32 42] $
            annot (M.fromList [(Name "middle", string "annotation")]) $
            lets ["y">: string "hello"] $
            lambda "z" $ var "x")
          (lambda "z" $ lets ["x">: int32 42] $
            annot (M.fromList [(Name "middle", string "annotation")]) $
            lets ["y">: string "hello"] $ var "x")

        noChange "annotation on lambda already above let"
          (annot (M.fromList [(Name "comment", string "correct")]) $
            lambda "y" $ lets ["x">: int32 42] $ var "x")

        liftsTo "nested annotations around lambda in let"
          (lets ["x">: int32 42] $
            annot (M.fromList [(Name "outer", string "1")]) $
            annot (M.fromList [(Name "inner", string "2")]) $
            lambda "y" $ var "x")
          (annot (M.fromList [(Name "outer", string "1")]) $
            annot (M.fromList [(Name "inner", string "2")]) $
            lambda "y" $ lets ["x">: int32 42] $ var "x")

    H.describe "Recursive lifting in nested structures" $ do
        liftsTo "let-lambda inside a list"
          (list [
            int32 1,
            lets ["x">: int32 42] $ lambda "y" $ var "x",
            int32 2])
          (list [
            int32 1,
            lambda "y" $ lets ["x">: int32 42] $ var "x",
            int32 2])

        liftsTo "let-lambda in multiple list elements"
          (list [
            lets ["x">: int32 1] $ lambda "y" $ var "x",
            lets ["z">: int32 2] $ lambda "w" $ var "z"])
          (list [
            lambda "y" $ lets ["x">: int32 1] $ var "x",
            lambda "w" $ lets ["z">: int32 2] $ var "z"])

        liftsTo "let-lambda in a let binding value"
          (lets ["f">: (lets ["x">: int32 42] $ lambda "y" $ var "x")] $
            var "f")
          (lets ["f">: (lambda "y" $ lets ["x">: int32 42] $ var "x")] $
            var "f")

        liftsTo "let-lambda in multiple let binding values"
          (lets [
            "f">: (lets ["x">: int32 1] $ lambda "y" $ var "x"),
            "g">: (lets ["z">: int32 2] $ lambda "w" $ var "w")] $
            var "f")
          (lets [
            "f">: (lambda "y" $ lets ["x">: int32 1] $ var "x"),
            "g">: (lambda "w" $ lets ["z">: int32 2] $ var "w")] $
            var "f")

        liftsTo "let-lambda inside a pair"
          (pair
            (lets ["x">: int32 42] $ lambda "y" $ var "x")
            (string "test"))
          (pair
            (lambda "y" $ lets ["x">: int32 42] $ var "x")
            (string "test"))

        liftsTo "let-lambda in both elements of a pair"
          (pair
            (lets ["x">: int32 1] $ lambda "y" $ var "x")
            (lets ["z">: int32 2] $ lambda "w" $ var "z"))
          (pair
            (lambda "y" $ lets ["x">: int32 1] $ var "x")
            (lambda "w" $ lets ["z">: int32 2] $ var "z"))

        liftsTo "let-lambda inside a record field"
          (record (Name "MyRecord") [
            "field1">: int32 42,
            "field2">: (lets ["x">: string "hello"] $ lambda "y" $ var "x")])
          (record (Name "MyRecord") [
            "field1">: int32 42,
            "field2">: (lambda "y" $ lets ["x">: string "hello"] $ var "x")])

        liftsTo "let-lambda inside lambda body"
          (lambda "outer" $
            lets ["x">: int32 42] $ lambda "inner" $ var "x")
          (lambda "outer" $ lambda "inner" $
            lets ["x">: int32 42] $ var "x")

        liftsTo "deeply nested let-lambda in let binding in list"
          (lets ["items">: list [
            lets ["x">: int32 1] $ lambda "y" $ var "x"]] $
            var "items")
          (lets ["items">: list [
            lambda "y" $ lets ["x">: int32 1] $ var "x"]] $
            var "items")
  where
    liftsTo desc termBefore termAfter = H.it desc $ do
      let result = liftLambdaAboveLet termBefore
      H.shouldBe (ShowCore.term result) (ShowCore.term termAfter)
    noChange desc term = liftsTo desc term term
    add = primitive _math_add





testNormalizeTypeVariablesInTerm :: H.SpecWith ()
testNormalizeTypeVariablesInTerm = do
    H.describe "No type variables" $ do
      H.it "test #1" $ noChange
        (int32 42)
      H.it "test #2" $ noChange
        (tlet (int32 42) [
          ("foo", Nothing, string "foo")])
      H.it "test #3" $ noChange
        (tlet (int32 42) [
          ("foo", Just tsString, string "foo")])
      H.it "test #4" $ noChange
        (withMonoFoo $ int32 42)

    H.describe "Only free type variables" $ do
      H.it "test #1" $ noChange
        (withPolyFoo $ int32 42)
      H.it "test #2" $ noChange
        (withMonoFoo const42)
      H.it "test #3" $ noChange
        (withPolyFoo const42)

    H.describe "Simple polymorphic let bindings" $ do
      H.it "test #1" $ changesTo
        (withIdBefore id42)
        (withIdAfter id42)

    H.describe "Rewriting of bindings does not affect body" $ do
      H.it "test #1" $ changesTo
        (withIdBefore const42) -- Free variable "a" coincides with bound variable "a", but in a different branch.
        (withIdAfter const42)
      H.it "test #2" $ changesTo -- Same substitution in bindings and environment
        (withIdBefore (withIdBefore id42))
        (withIdAfter (withIdAfter id42))

    H.describe "Nested polymorphic let bindings" $ do
      H.it "Parent variable shadows child variable" $ changesTo
        (tlet id42 [
          ("id", Just faa, tlet (lambdaTyped "y" tA $ var "id2" @@ var "y") [
            ("id2", Just faa, lambdaTyped "x" tA $ var "x")])])
        (tlet id42 [
          ("id", Just ft0t0, tlet (lambdaTyped "y" t0 $ var "id2" @@ var "y") [
            ("id2", Just ft1t1, lambdaTyped "x" t1 $ var "x")])])
      H.it "No shadowing" $ changesTo
        (tlet id42 [
          ("id", Just faa, tlet (lambdaTyped "y" tA $ var "id2" @@ var "y") [
            ("id2", Just fbb, lambdaTyped "x" tB $ var "x")])])
        (tlet id42 [
          ("id", Just ft0t0, tlet (lambdaTyped "y" t0 $ var "id2" @@ var "y") [
            ("id2", Just ft1t1, lambdaTyped "x" t1 $ var "x")])])
      H.it "No shadowing, locally free type variable" $ changesTo
        (tlet (var "fun1" @@ string "foo" @@ int32 42) [
          ("fun1", Just (Types.poly ["a", "b"] $ Types.functionMany [tA, tB, tPair tA tB]), lambdaTyped "x" tA $ lambdaTyped "y" tB $
            tlet (var "fun2" @@ var "x") [
              ("fun2", Just (Types.poly ["c"] $ tFun tC $ tPair tC tB), lambdaTyped "z" tC $ pair (var "z") (var "y"))])])
        (tlet (var "fun1" @@ string "foo" @@ int32 42) [
          ("fun1", Just (Types.poly ["t0", "t1"] $ Types.functionMany [t0, t1, tPair t0 t1]), lambdaTyped "x" t0 $ lambdaTyped "y" t1 $
            tlet (var "fun2" @@ var "x") [
              ("fun2", Just (Types.poly ["t2"] $ tFun t2 $ tPair t2 t1), lambdaTyped "z" t2 $ pair (var "z") (var "y"))])])
  where
    normalize = normalizeTypeVariablesInTerm
    changesTo term1 term2 = H.shouldBe (normalize term1) term2
    noChange term = H.shouldBe (normalize term) term
    tlet env triples = TermLet $ Let (toBinding <$> triples) env
      where
        toBinding (key, mts, value) = Binding (Name key) value mts
    t0 = Types.var "t0"
    t1 = Types.var "t1"
    t2 = Types.var "t2"
    const42 = lambdaTyped "x" (Types.function tA tInt32) $ int32 42
    faa = Types.poly ["a"] $ tFun tA tA
    fbb = Types.poly ["b"] $ tFun tB tB
    ft0t0 = Types.poly ["t0"] $ tFun t0 t0
    ft1t1 = Types.poly ["t1"] $ tFun t1 t1
    id42 = var "id" @@ int32 42
    tsString = Types.mono Types.string
    tsA = Types.mono $ Types.var "a"
    withIdBefore term = tlet term [
      ("id", Just faa, lambda "x" $ var "x")]
    withIdAfter term = tlet term [
      ("id", Just ft0t0, lambda "x" $ var "x")]
    withMonoFoo term = tlet term [
      ("foo", Just tsString, string "foo")]
    withPolyFoo term = tlet term [
      ("foo", Just tsA, var "bar")]

testReplaceTerm :: H.SpecWith ()
testReplaceTerm = do
    H.describe "Test term replacement" $ do

      H.it "Check that the correct subterms are replaced" $ do
        H.shouldBe
          (rewriteTerm replaceInts
            (int32 42))
          (int64 42)
        H.shouldBe
          (rewriteTerm replaceInts
            (list [int32 42, (lambda "x" $ var "x") @@ int32 137]))
          (list [int64 42, (lambda "x" $ var "x") @@ int64 137])

      H.it "Check that traversal order is respected" $ do
        H.shouldBe
          (rewriteTerm replaceListsPre
            (list [list [list []]]))
          (list [list []])
        H.shouldBe
          (rewriteTerm replaceListsPost
            (list [list [list []]]))
          (list [])

--      H.it "Check that metadata is replace recursively" $ do
--        H.shouldBe
--          (rewriteTerm keepTerm replaceKv (list [annot 42 (string "foo")] Int))
--          (list [annot "42" (string "foo")])
  where
    keepTerm recurse term = recurse term

    replaceInts recurse term = case term2 of
        TermLiteral (LiteralInteger (IntegerValueInt32 v)) -> int64 $ fromIntegral v
        _ -> term2
      where
        term2 = recurse term

    replaceLists term = case term of
      TermList (h:_) -> case h of
        TermList [] -> list []
        _ -> term
      _ -> term

    replaceListsPre recurse = recurse . replaceLists

    replaceListsPost recurse = replaceLists . recurse

    replaceKv i = show i

testRewriteExampleType :: H.SpecWith ()
testRewriteExampleType = do
    H.describe "Test rewriting of a made-up recursive type" $ do

      H.it "Rewrite a hand-picked expression" $ do
        H.shouldBe
          quux2
          (myQuuxRewriter quux1)
  where
    quux1 = QuuxPair QuuxUnit (QuuxPair (QuuxValue "abc") (QuuxValue "12345"))
    quux2 = QuuxPair QuuxUnit (QuuxPair QuuxUnit (QuuxValue 5))

testRewriteFunctionReachesSubterms :: String -> (((Term -> Term) -> Term -> Term) -> Term -> Term) -> H.SpecWith ()
testRewriteFunctionReachesSubterms functionName function = H.describe ("Test that " ++ functionName ++ " reaches all subterms") $ do
  H.describe "Simple terms" $ do
    checkRewrite "string literal"
      foo
      bar

    checkRewrite "string in variable (should not change)"
      (var "x")
      (var "x")

  H.describe "Collections" $ do
    checkRewrite "string in list"
      (list [foo, baz])
      (list [bar, baz])

    checkRewrite "multiple strings in list"
      (list [foo, foo, baz])
      (list [bar, bar, baz])

    checkRewrite "string in optional (Just)"
      (just foo)
      (just bar)

    checkRewrite "string in set"
      (set $ S.fromList [foo, baz])
      (set $ S.fromList [bar, baz])

  H.describe "Applications and functions" $ do
    checkRewrite "string in function application"
      (var "print" @@ foo)
      (var "print" @@ bar)

    checkRewrite "string in lambda body"
      (lambda "x" foo)
      (lambda "x" bar)

    checkRewrite "string in nested applications"
      (var "f" @@ (var "g" @@ foo))
      (var "f" @@ (var "g" @@ bar))

  H.describe "Records and products" $ do
    checkRewrite "string in record field"
      (record (Name "Person") ["name">: foo])
      (record (Name "Person") ["name">: bar])

    checkRewrite "strings in multiple record fields"
      (record (Name "Data") ["a">: foo, "b">: baz, "c">: foo])
      (record (Name "Data") ["a">: bar, "b">: baz, "c">: bar])

    checkRewrite "string in tuple"
      (pair foo (int32 42))
      (pair bar (int32 42))

  H.describe "Let bindings" $ do
    checkRewrite "string in let binding value"
      (lets ["x">: foo] (var "x"))
      (lets ["x">: bar] (var "x"))

    checkRewrite "string in let body"
      (lets ["x">: int32 1] foo)
      (lets ["x">: int32 1] bar)

  H.describe "Case statements" $ do
    checkRewrite "string in first case branch"
      (match (Name "Result") Nothing
        ["success">: foo,
         "error">: baz])
      (match (Name "Result") Nothing
        ["success">: bar,
         "error">: baz])

    checkRewrite "string in second case branch"
      (match (Name "Result") Nothing
        ["success">: baz,
         "error">: foo])
      (match (Name "Result") Nothing
        ["success">: baz,
         "error">: bar])

    checkRewrite "string in multiple case branches"
      (match (Name "Result") Nothing
        ["success">: foo,
         "error">: foo,
         "pending">: baz])
      (match (Name "Result") Nothing
        ["success">: bar,
         "error">: bar,
         "pending">: baz])

    checkRewrite "string in default branch"
      (match (Name "Result") (Just foo)
        ["success">: baz,
         "error">: baz])
      (match (Name "Result") (Just bar)
        ["success">: baz,
         "error">: baz])

    checkRewrite "string in both case branch and default"
      (match (Name "Result") (Just foo)
        ["success">: foo,
         "error">: baz])
      (match (Name "Result") (Just bar)
        ["success">: bar,
         "error">: baz])

    checkRewrite "string in nested lambda in case branch"
      (match (Name "Result") Nothing
        ["success">: lambda "x" foo,
         "error">: lambda "y" baz])
      (match (Name "Result") Nothing
        ["success">: lambda "x" bar,
         "error">: lambda "y" baz])

  H.describe "Deeply nested terms" $ do
    checkRewrite "string deeply nested in record in list in application"
      (var "process" @@ list [record (Name "Item") ["value">: foo]])
      (var "process" @@ list [record (Name "Item") ["value">: bar]])

  H.describe "Maps" $ do
    checkRewrite "string in map keys"
      (Terms.map $ M.fromList [(foo, baz), (baz, baz)])
      (Terms.map $ M.fromList [(bar, baz), (baz, baz)])

    checkRewrite "string in map values"
      (Terms.map $ M.fromList [(baz, foo), (baz, baz)])
      (Terms.map $ M.fromList [(baz, bar), (baz, baz)])

    checkRewrite "string in both map keys and values"
      (Terms.map $ M.fromList [(foo, foo), (baz, baz)])
      (Terms.map $ M.fromList [(bar, bar), (baz, baz)])

  H.describe "Unions and injections" $ do
    checkRewrite "string in union variant value"
      (variant (Name "Result") (Name "success") foo)
      (variant (Name "Result") (Name "success") bar)

  H.describe "Sums" $ do
    checkRewrite "string in sum term"
      (Terms.sum 0 3 foo)
      (Terms.sum 0 3 bar)

  H.describe "Wrapped terms" $ do
    checkRewrite "string in wrapped term"
      (wrap (Name "Email") foo)
      (wrap (Name "Email") bar)

  H.describe "Annotated terms" $ do
    checkRewrite "string in annotated term body"
      (annotated foo (M.fromList [(Name "comment", baz)]))
      (annotated bar (M.fromList [(Name "comment", baz)]))

    checkRewrite "string in annotation value is NOT replaced"
      (annotated baz (M.fromList [(Name "comment", foo)]))
      (annotated baz (M.fromList [(Name "comment", foo)]))

  H.describe "System F polymorphism" $ do
    checkRewrite "string in type lambda body"
      (tylam "a" foo)
      (tylam "a" bar)

    checkRewrite "string in type application body"
      (tyapp foo (Types.string))
      (tyapp bar (Types.string))

    checkRewrite "string in nested type lambdas"
      (tylams ["a", "b"] foo)
      (tylams ["a", "b"] bar)

  H.describe "Multiple bindings in let" $ do
    checkRewrite "string in first of multiple let bindings"
      (lets ["x">: foo, "y">: baz] (var "x"))
      (lets ["x">: bar, "y">: baz] (var "x"))

    checkRewrite "string in second of multiple let bindings"
      (lets ["x">: baz, "y">: foo] (var "y"))
      (lets ["x">: baz, "y">: bar] (var "y"))

    checkRewrite "string in all let bindings and body"
      (lets ["x">: foo, "y">: foo] foo)
      (lets ["x">: bar, "y">: bar] bar)

  H.describe "Complex nested structures" $ do
    checkRewrite "string in case branch within let binding"
      (lets ["handler">: match (Name "Result") Nothing ["ok">: foo, "err">: baz]] (var "handler"))
      (lets ["handler">: match (Name "Result") Nothing ["ok">: bar, "err">: baz]] (var "handler"))

    checkRewrite "string in annotated wrapped record field"
      (annotated (wrap (Name "User") (record (Name "UserData") ["name">: foo])) M.empty)
      (annotated (wrap (Name "User") (record (Name "UserData") ["name">: bar])) M.empty)
  where
    foo = string "foo"
    bar = string "bar"
    baz = string "baz"

    checkRewrite :: String -> Term -> Term -> H.SpecWith ()
    checkRewrite desc start expected = H.it desc $ do
      let rewritten = function replaceFooWithBar start
      rewritten `H.shouldBe` expected

    replaceFooWithBar :: (Term -> Term) -> Term -> Term
    replaceFooWithBar recurse term = case term of
      TermLiteral (LiteralString "foo") -> bar
      _ -> recurse term

testRewriteTerm :: H.SpecWith ()
testRewriteTerm = do
  testRewriteFunctionReachesSubterms "rewriteTerm" rewriteTerm

testRewriteTermM :: H.SpecWith ()
testRewriteTermM = do
  testRewriteFunctionReachesSubterms "rewriteTermM" rewrite
  where
    rewrite recurse term = fromFlow Terms.unit () $ rewriteTermM rewriteM term
      where
        rewriteM :: (Term -> Flow () Term) -> Term -> Flow () Term
        rewriteM recurseM t = return $ recurse (fromFlow Terms.unit () . recurseM) t

testRewriteTermWithContext :: H.SpecWith ()
testRewriteTermWithContext = do
  testRewriteFunctionReachesSubterms "rewriteTermWithContext" rewrite
  where
    rewrite recurse term = rewriteTermWithContext rewriteWithCtx () term
      where
        rewriteWithCtx :: (a -> Term -> Term) -> a -> Term -> Term
        rewriteWithCtx recurseWithCtx ctx t = recurse (recurseWithCtx ctx) t

testRewriteTermWithContextM :: H.SpecWith ()
testRewriteTermWithContextM = do
  testRewriteFunctionReachesSubterms "rewriteTermWithContextM" rewrite
  where
    rewrite recurse term = fromFlow Terms.unit () $ rewriteTermWithContextM rewriteWithCtxM () term
      where
        rewriteWithCtxM :: (a -> Term -> Flow () Term) -> a -> Term -> Flow () Term
        rewriteWithCtxM recurseWithCtxM ctx t = return $ recurse (fromFlow Terms.unit () . recurseWithCtxM ctx) t

testSimplifyTerm :: H.SpecWith ()
testSimplifyTerm = do
  H.describe "Test term simplification (optimization)" $ do

    H.it "Check that 'const' applications are simplified" $ do
      H.shouldBe
        (simplifyTerm $ (lambda "x" $ string "foo") @@ int32 42)
        (string "foo")
      H.shouldBe
        (simplifyTerm ((lambda "x" $ list [var "x", var "x"]) @@ var "y"))
        (list [var "y", var "y"])
      H.shouldBe
        (simplifyTerm ((lambda "x" $ string "foo") @@ var "y"))
        (string "foo")
      H.shouldBe
        (simplifyTerm ((lambda "x"
          ((lambda "a" (list [string "foo", var "a"])) @@ var "x")) @@ var "y"))
        (list [string "foo", var "y"])

--testStripAnnotations :: H.SpecWith ()
--testStripAnnotations = do
--  H.describe "Test stripping metadata from terms" $ do
--
--    H.it "Strip type annotations" $ do
--      QC.property $ \(TypeApplicationTerm term typ) -> do
--        shouldSucceedWith
--          (getTermType term)
--          Nothing
--        shouldSucceedWith
--          (getTermType $ withType typ term)
--          (Just typ)
--        shouldSucceedWith
--          (getTermType $ strip $ withType typ term)
--          Nothing

testTopologicalSortBindings :: H.SpecWith ()
testTopologicalSortBindings = do
    H.describe "Test topological sort of bindings" $ do

      H.it "Isolated bindings" $ do
        checkBindings
          [("a", string "foo"), ("b", string "bar")]
          [["a"], ["b"]]

      H.it "Single recursive binding" $ do
        checkBindings
          [("a", list [var "a"])]
          [["a"]]

      H.it "Mutually recursive bindings" $ do
        checkBindings
          [("a", list [var "b"]), ("b", list [var "a"])]
          [["a", "b"]]

      H.it "Mixed bindings" $ do
        checkBindings
          [("a", var "b"), ("b", list [var "a", var "c"]), ("c", string "foo"), ("d", string "bar")]
          [["c"], ["a", "b"], ["d"]]
  where
    checkBindings bindings expectedVars = H.shouldBe
        (topologicalSortBindingMap bindingMap)
        expected
      where
        bindingMap = M.mapKeys (\k -> Name k) $ M.fromList bindings
        expected = fmap (fmap (\k -> (Name k, Y.fromMaybe unit $ M.lookup (Name k) bindingMap))) expectedVars

spec :: H.Spec
spec = do
  checkFoldOverTerm
  checkStripTerm
  checkStripType

  testFoldOverTerm

  testEtaExpandTypedTerms testGraph
  testEtaExpandUntypedTerms testGraph
  testFlattenLetTerms
  testFreeVariablesInTerm
  testLiftLambdaAboveLet
  testNormalizeTypeVariablesInTerm
  testReplaceTerm
  testRewriteExampleType
  testRewriteTerm
  testRewriteTermM
  testRewriteTermWithContext
  testRewriteTermWithContextM
  testSimplifyTerm
--  testStripAnnotations -- TODO: restore me
  testTopologicalSortBindings
