{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for AST serialization (printExpr and parenthesize)
--
-- Note: This module supersedes the Haskell-specific Hydra.SerializationSpec tests.
module Hydra.Sources.Test.Serialization where

-- Standard imports for deep DSL tests (produces TypedTerm a with specific types)
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Phantoms                as Phantoms hiding ((++))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
import Hydra.Dsl.AsTerm
import qualified Hydra.Dsl.Ast as Ast
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Sources.Kernel.Terms.Serialization as Serialization

import Hydra.Ast (Expr, Op, BlockStyle)


ns :: ModuleName
ns = ModuleName "hydra.test.serialization"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Serialization.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for AST serialization"))}
  where
    definitions = [
      Phantoms.toDefinition arrowOp,
      Phantoms.toDefinition gtOp,
      Phantoms.toDefinition plusOp,
      Phantoms.toDefinition multOp,
      Phantoms.toDefinition lambdaOp,
      Phantoms.toDefinition caseOp,
      Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for AST serialization" $
    supergroup "serialization" [
      associativityGroup,
      caseStatementGroup,
      lambdaGroup,
      listGroup,
      precedenceGroup]

-- Test operators defined locally to avoid a dependency on hydra.haskell.operators
arrowOp :: TypedTermDefinition Op
arrowOp = define "arrowOp" $
  Serialization.op @@ string "->" @@ (Math.negate $ int32 1) @@ Ast.associativityRight

-- | Test cases for associativity
associativityGroup :: TypedTerm TestGroup
associativityGroup = subgroup "associativity" [
    serCase "right-associative operator"
      -- (a -> b) -> c -> d
      (infixExpr arrowOp (infixExpr arrowOp (cstExpr $ string "a") (cstExpr $ string "b"))
                         (infixExpr arrowOp (cstExpr $ string "c") (cstExpr $ string "d")))
      (string "(a -> b) -> c -> d")]

-- Helper for building a bracket list
bracketListExpr :: AsTerm t BlockStyle => t -> [TypedTerm Expr] -> TypedTerm Expr
bracketListExpr style exprs = Serialization.bracketList @@ asTerm style @@ list exprs

caseOp :: TypedTermDefinition Op
caseOp = define "caseOp" $
  Serialization.op @@ string "->" @@ int32 0 @@ Ast.associativityNone

-- | Test cases for case statements
caseStatementGroup :: TypedTerm TestGroup
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

-- Helper for case statements
-- This creates: case cond of { pattern1 -> expr1; pattern2 -> expr2; ... }
casesExpr :: TypedTerm Expr -> [(TypedTerm Expr, TypedTerm Expr)] -> TypedTerm Expr
casesExpr cond branches = infixExpr ofOp lhs rhs
  where
    lhs = spaceSepExpr [cstExpr $ string "case", cond]
    rhs = newlineSepExpr [infixExpr caseOp pat expr | (pat, expr) <- branches]
    ofOp = Ast.op
      (Ast.symbol $ string "of")
      (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ string "  "))
      (Ast.precedence $ int32 0)
      Ast.associativityNone

-- Helper for building a constant expression
cstExpr :: TypedTerm String -> TypedTerm Expr
cstExpr s = Serialization.cst @@ s

gtOp :: TypedTermDefinition Op
gtOp = define "gtOp" $
  Serialization.op @@ string ">" @@ int32 4 @@ Ast.associativityNone

-- Helper for building an infix expression: ifx op lhs rhs
infixExpr :: AsTerm t Op => t -> TypedTerm Expr -> TypedTerm Expr -> TypedTerm Expr
infixExpr opExpr lhs rhs = Serialization.ifx @@ asTerm opExpr @@ lhs @@ rhs

-- Inline style (reference to kernel)
inlineBlockStyle :: TypedTermDefinition BlockStyle
inlineBlockStyle = Serialization.inlineStyle

-- Helper for lambda expressions: \vars -> body
-- lambdaExpr ["x", "y"] body = ifx lambdaOp (cst "\x y") body
lambdaExpr :: [String] -> TypedTerm Expr -> TypedTerm Expr
lambdaExpr vars body = infixExpr lambdaOp (cstExpr $ string ("\\" ++ unwords vars)) body

-- | Test cases for lambda expressions
lambdaGroup :: TypedTerm TestGroup
lambdaGroup = subgroup "lambdas" [
    serCase "simple lambda"
      (lambdaExpr ["x", "y"] (infixExpr plusOp (cstExpr $ string "x") (cstExpr $ string "y")))
      (string "\\x y -> x + y")]

lambdaOp :: TypedTermDefinition Op
lambdaOp = define "lambdaOp" $
  Serialization.op @@ string "->" @@ (Math.negate $ int32 1) @@ Ast.associativityRight

-- | Test cases for list expressions
listGroup :: TypedTerm TestGroup
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

multOp :: TypedTermDefinition Op
multOp = define "multOp" $
  Serialization.op @@ string "*" @@ int32 7 @@ Ast.associativityBoth

-- Helper for building a newline-separated expression
newlineSepExpr :: [TypedTerm Expr] -> TypedTerm Expr
newlineSepExpr exprs = Serialization.newlineSep @@ list exprs

-- Helper for building a numeric expression
numExpr :: Int -> TypedTerm Expr
numExpr n = Serialization.num @@ int32 n

plusOp :: TypedTermDefinition Op
plusOp = define "plusOp" $
  Serialization.op @@ string "+" @@ int32 6 @@ Ast.associativityBoth

-- | Test cases for operator precedence
precedenceGroup :: TypedTerm TestGroup
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

-- Universal serialization test case: printExpr (parenthesize expr) == expected
serCase :: String -> TypedTerm Expr -> TypedTerm String -> TypedTerm TestCaseWithMetadata
serCase cname expr expected = universalCase cname (Serialization.printExpr @@ (Serialization.parenthesize @@ expr)) expected

-- Helper for building a space-separated expression
spaceSepExpr :: [TypedTerm Expr] -> TypedTerm Expr
spaceSepExpr exprs = Serialization.spaceSep @@ list exprs
