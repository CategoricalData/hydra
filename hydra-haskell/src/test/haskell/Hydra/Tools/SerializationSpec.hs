module Hydra.Tools.SerializationSpec where

import qualified Test.Hspec as H

import Hydra.Ast
import Hydra.Tools.Serialization
import Hydra.Langs.Haskell.Operators


check :: Expr -> String -> H.Expectation
check expr printed = printExpr (parenthesize expr) `H.shouldBe` printed

caseStatement :: Expr -> [(Expr, Expr)] -> Expr
caseStatement cond cases = ifx ofOp lhs rhs
  where
    lhs = spaceSep [cst "case", cond]
    rhs = newlineSep (uncurry (ifx caseOp) <$> cases)
    ofOp = Op (Symbol "of") (Padding WsSpace $ WsBreakAndIndent "  ") (Precedence 0) AssociativityNone

lam :: [String] -> Expr -> Expr
lam vars = ifx lambdaOp $ cst $ "\\" ++ unwords vars

checkAssociativity :: H.SpecWith ()
checkAssociativity = do
  H.describe "Unit tests to verify that associativity is respected" $ do

    H.it "Right-associative operator" $ do
      check
        (ifx arrowOp (ifx arrowOp (cst "a") (cst "b")) (ifx arrowOp (cst "c") (cst "d")))
        "(a -> b) -> c -> d"

checkCaseStatements :: H.SpecWith ()
checkCaseStatements = do
  H.describe "Unit tests for case statements" $ do

    H.it "Simple case statement" $ do
      check
        (caseStatement (ifx gtOp (cst "x") (num 42)) [(cst "False", cst "Big"), (cst "True", cst "Small")])
        (    "case x > 42 of\n"
          ++ "  False -> Big\n"
          ++ "  True -> Small")

    H.it "Nested case statement" $ do
      check
        (caseStatement (ifx gtOp (cst "x") (num 42)) [
          (cst "True", caseStatement (ifx gtOp (cst "x") (num 100)) [(cst "True", cst "ReallyBig"), (cst "False", cst "Big")]),
          (cst "False", cst "Small")])
        (    "case x > 42 of\n"
          ++ "  True -> case x > 100 of\n"
          ++ "    True -> ReallyBig\n"
          ++ "    False -> Big\n"
          ++ "  False -> Small")

checkLambdas :: H.SpecWith ()
checkLambdas = do
  H.describe "Unit tests for lambda expressions" $ do

    H.it "Simple lambda" $ do
      check
        (lam ["x", "y"] (ifx plusOp (cst "x") (cst "y")))
        "\\x y -> x + y"

checkLists :: H.SpecWith ()
checkLists = do
  H.describe "Unit tests for list expressions" $ do

    H.it "Empty list" $ do
      check
        (bracketList inlineStyle [])
        "[]"

    H.it "Simple non-empty list" $ do
      check
        (bracketList inlineStyle [num 1, num 2, num 3])
        "[1, 2, 3]"

    H.it "Nested list" $ do
      check
        (bracketList inlineStyle [bracketList inlineStyle [num 1, num 3], num 2])
        "[[1, 3], 2]"

    H.it "List with parenthesized expression inside" $ do
      check
        (bracketList inlineStyle [bracketList inlineStyle [num 1, ifx multOp (ifx plusOp (num 2) (num 3)) (ifx plusOp (num 1) (num 10))], num 2])
        "[[1, (2 + 3) * (1 + 10)], 2]"

checkPrecedence :: H.SpecWith ()
checkPrecedence = do
  H.describe "Unit tests to verify that operator precedence is respected" $ do

    H.it "Check expressions with operators of different precedence" $ do
      check
        (ifx plusOp (ifx multOp (num 2) (num 3)) (ifx multOp (num 1) (num 10)))
        "2 * 3 + 1 * 10"
      check
        (ifx multOp (ifx plusOp (num 2) (num 3)) (ifx plusOp (num 1) (num 10)))
        "(2 + 3) * (1 + 10)"

    H.it "Check an operator which is both left- and right-associative" $ do
      check
        (ifx multOp (cst "x") (ifx multOp (cst "y") (cst "z")))
        "x * y * z"
      check
        (ifx multOp (ifx multOp (cst "x") (cst "y")) (cst "z"))
        "x * y * z"

spec :: H.Spec
spec = do
  checkAssociativity
  checkCaseStatements
  checkLambdas
  checkLists
  checkPrecedence
