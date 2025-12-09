{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for AST serialization (printExpr and parenthesize)
--
-- Note: This module supersedes the Haskell-specific Hydra.SerializationSpec tests.
module Hydra.Sources.Test.Serialization where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Ast as Ast
import Hydra.Dsl.Meta.Phantoms as Phantoms hiding ((++))
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Serialization as Serialization
import qualified Hydra.Sources.Haskell.Operators as Operators

import Hydra.Ast (Expr, Op, BlockStyle)


module_ :: Module
module_ = Module (Namespace "hydra.test.serialization") elements
    [Serialization.module_, Operators.module_]
    KernelTypes.kernelTypesModules
    (Just "Test cases for AST serialization")
  where
    elements = [Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
    doc "Test cases for AST serialization" $
    supergroup "serialization" [
      associativityGroup,
      caseStatementGroup,
      lambdaGroup,
      listGroup,
      precedenceGroup]

-- Helper for building an infix expression: ifx op lhs rhs
infixExpr :: AsTerm t Op => t -> TTerm Expr -> TTerm Expr -> TTerm Expr
infixExpr opExpr lhs rhs = Serialization.ifx @@ asTerm opExpr @@ lhs @@ rhs

-- Helper for building a constant expression
cstExpr :: TTerm String -> TTerm Expr
cstExpr s = Serialization.cst @@ s

-- Helper for building a numeric expression
numExpr :: Int -> TTerm Expr
numExpr n = Serialization.num @@ int32 n

-- Helper for building a space-separated expression
spaceSepExpr :: [TTerm Expr] -> TTerm Expr
spaceSepExpr exprs = Serialization.spaceSep @@ list exprs

-- Helper for building a newline-separated expression
newlineSepExpr :: [TTerm Expr] -> TTerm Expr
newlineSepExpr exprs = Serialization.newlineSep @@ list exprs

-- Helper for building a bracket list
bracketListExpr :: AsTerm t BlockStyle => t -> [TTerm Expr] -> TTerm Expr
bracketListExpr style exprs = Serialization.bracketList @@ asTerm style @@ list exprs

-- Inline style (reference to kernel)
inlineBlockStyle :: TBinding BlockStyle
inlineBlockStyle = Serialization.inlineStyle

-- Reference to Haskell operators
arrowOp, gtOp, plusOp, multOp, lambdaOp, caseOp :: TBinding Op
arrowOp = Operators.arrowOp
gtOp = Operators.gtOp
plusOp = Operators.plusOp
multOp = Operators.multOp
lambdaOp = Operators.lambdaOp
caseOp = Operators.caseOp

-- Helper for lambda expressions: \vars -> body
-- lambdaExpr ["x", "y"] body = ifx lambdaOp (cst "\x y") body
lambdaExpr :: [String] -> TTerm Expr -> TTerm Expr
lambdaExpr vars body = infixExpr lambdaOp (cstExpr $ string ("\\" ++ unwords vars)) body

-- Helper for case statements
-- This creates: case cond of { pattern1 -> expr1; pattern2 -> expr2; ... }
casesExpr :: TTerm Expr -> [(TTerm Expr, TTerm Expr)] -> TTerm Expr
casesExpr cond branches = infixExpr ofOp lhs rhs
  where
    lhs = spaceSepExpr [cstExpr $ string "case", cond]
    rhs = newlineSepExpr [infixExpr caseOp pat expr | (pat, expr) <- branches]
    ofOp = Ast.op
      (Ast.symbol $ string "of")
      (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ string "  "))
      (Ast.precedence $ int32 0)
      Ast.associativityNone

-- | Test cases for associativity
associativityGroup :: TTerm TestGroup
associativityGroup = subgroup "associativity" [
    serCase "right-associative operator"
      -- (a -> b) -> c -> d
      (infixExpr arrowOp (infixExpr arrowOp (cstExpr $ string "a") (cstExpr $ string "b"))
                         (infixExpr arrowOp (cstExpr $ string "c") (cstExpr $ string "d")))
      (string "(a -> b) -> c -> d")]

-- | Test cases for case statements
caseStatementGroup :: TTerm TestGroup
caseStatementGroup = subgroup "case statements" [
    serCase "simple case statement"
      (casesExpr (infixExpr gtOp (cstExpr $ string "x") (numExpr 42))
                 [(cstExpr $ string "False", cstExpr $ string "Big"),
                  (cstExpr $ string "True", cstExpr $ string "Small")])
      (string "case x > 42 of\n  False -> Big\n  True -> Small"),

    serCase "nested case statement"
      (casesExpr (infixExpr gtOp (cstExpr $ string "x") (numExpr 42))
                 [(cstExpr $ string "True",
                   casesExpr (infixExpr gtOp (cstExpr $ string "x") (numExpr 100))
                             [(cstExpr $ string "True", cstExpr $ string "ReallyBig"),
                              (cstExpr $ string "False", cstExpr $ string "Big")]),
                  (cstExpr $ string "False", cstExpr $ string "Small")])
      (string "case x > 42 of\n  True -> case x > 100 of\n    True -> ReallyBig\n    False -> Big\n  False -> Small")]

-- | Test cases for lambda expressions
lambdaGroup :: TTerm TestGroup
lambdaGroup = subgroup "lambdas" [
    serCase "simple lambda"
      (lambdaExpr ["x", "y"] (infixExpr plusOp (cstExpr $ string "x") (cstExpr $ string "y")))
      (string "\\x y -> x + y")]

-- | Test cases for list expressions
listGroup :: TTerm TestGroup
listGroup = subgroup "lists" [
    serCase "empty list"
      (bracketListExpr inlineBlockStyle [])
      (string "[]"),

    serCase "simple non-empty list"
      (bracketListExpr inlineBlockStyle [numExpr 1, numExpr 2, numExpr 3])
      (string "[1, 2, 3]"),

    serCase "nested list"
      (bracketListExpr inlineBlockStyle
        [bracketListExpr inlineBlockStyle [numExpr 1, numExpr 3], numExpr 2])
      (string "[[1, 3], 2]"),

    serCase "list with parenthesized expression inside"
      (bracketListExpr inlineBlockStyle
        [bracketListExpr inlineBlockStyle
          [numExpr 1,
           infixExpr multOp
             (infixExpr plusOp (numExpr 2) (numExpr 3))
             (infixExpr plusOp (numExpr 1) (numExpr 10))],
         numExpr 2])
      (string "[[1, (2 + 3) * (1 + 10)], 2]")]

-- | Test cases for operator precedence
precedenceGroup :: TTerm TestGroup
precedenceGroup = subgroup "precedence" [
    serCase "operators with different precedence - no parens needed"
      (infixExpr plusOp
        (infixExpr multOp (numExpr 2) (numExpr 3))
        (infixExpr multOp (numExpr 1) (numExpr 10)))
      (string "2 * 3 + 1 * 10"),

    serCase "operators with different precedence - parens needed"
      (infixExpr multOp
        (infixExpr plusOp (numExpr 2) (numExpr 3))
        (infixExpr plusOp (numExpr 1) (numExpr 10)))
      (string "(2 + 3) * (1 + 10)"),

    serCase "associative operator left nesting"
      (infixExpr multOp (cstExpr $ string "x")
        (infixExpr multOp (cstExpr $ string "y") (cstExpr $ string "z")))
      (string "x * y * z"),

    serCase "associative operator right nesting"
      (infixExpr multOp
        (infixExpr multOp (cstExpr $ string "x") (cstExpr $ string "y"))
        (cstExpr $ string "z"))
      (string "x * y * z")]
