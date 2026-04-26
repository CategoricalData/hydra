-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for AST serialization

module Hydra.Test.Serialization where
import qualified Hydra.Ast as Ast
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Test cases for AST serialization
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "serialization",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "associativity",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "right-associative operator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx arrowOp (Serialization.ifx arrowOp (Serialization.cst "a") (Serialization.cst "b")) (Serialization.ifx arrowOp (Serialization.cst "c") (Serialization.cst "d"))))),
                Testing.universalTestCaseExpected = "(a -> b) -> c -> d"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "case statements",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple case statement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx (Ast.Op {
                  Ast.opSymbol = (Ast.Symbol "of"),
                  Ast.opPadding = Ast.Padding {
                    Ast.paddingLeft = Ast.WsSpace,
                    Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
                  Ast.opPrecedence = (Ast.Precedence 0),
                  Ast.opAssociativity = Ast.AssociativityNone}) (Serialization.spaceSep [
                  Serialization.cst "case",
                  (Serialization.ifx gtOp (Serialization.cst "x") (Serialization.num 42))]) (Serialization.newlineSep [
                  Serialization.ifx caseOp (Serialization.cst "False") (Serialization.cst "Big"),
                  (Serialization.ifx caseOp (Serialization.cst "True") (Serialization.cst "Small"))])))),
                Testing.universalTestCaseExpected = "case x > 42 of\n  False -> Big\n  True -> Small"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested case statement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx (Ast.Op {
                  Ast.opSymbol = (Ast.Symbol "of"),
                  Ast.opPadding = Ast.Padding {
                    Ast.paddingLeft = Ast.WsSpace,
                    Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
                  Ast.opPrecedence = (Ast.Precedence 0),
                  Ast.opAssociativity = Ast.AssociativityNone}) (Serialization.spaceSep [
                  Serialization.cst "case",
                  (Serialization.ifx gtOp (Serialization.cst "x") (Serialization.num 42))]) (Serialization.newlineSep [
                  Serialization.ifx caseOp (Serialization.cst "True") (Serialization.ifx (Ast.Op {
                    Ast.opSymbol = (Ast.Symbol "of"),
                    Ast.opPadding = Ast.Padding {
                      Ast.paddingLeft = Ast.WsSpace,
                      Ast.paddingRight = (Ast.WsBreakAndIndent "  ")},
                    Ast.opPrecedence = (Ast.Precedence 0),
                    Ast.opAssociativity = Ast.AssociativityNone}) (Serialization.spaceSep [
                    Serialization.cst "case",
                    (Serialization.ifx gtOp (Serialization.cst "x") (Serialization.num 100))]) (Serialization.newlineSep [
                    Serialization.ifx caseOp (Serialization.cst "True") (Serialization.cst "ReallyBig"),
                    (Serialization.ifx caseOp (Serialization.cst "False") (Serialization.cst "Big"))])),
                  (Serialization.ifx caseOp (Serialization.cst "False") (Serialization.cst "Small"))])))),
                Testing.universalTestCaseExpected = "case x > 42 of\n  True -> case x > 100 of\n    True -> ReallyBig\n    False -> Big\n  False -> Small"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "lambdas",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx lambdaOp (Serialization.cst "\\x y") (Serialization.ifx plusOp (Serialization.cst "x") (Serialization.cst "y"))))),
                Testing.universalTestCaseExpected = "\\x y -> x + y"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "lists",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.bracketList Serialization.inlineStyle []))),
                Testing.universalTestCaseExpected = "[]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple non-empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.bracketList Serialization.inlineStyle [
                  Serialization.num 1,
                  (Serialization.num 2),
                  (Serialization.num 3)]))),
                Testing.universalTestCaseExpected = "[1, 2, 3]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.bracketList Serialization.inlineStyle [
                  Serialization.bracketList Serialization.inlineStyle [
                    Serialization.num 1,
                    (Serialization.num 3)],
                  (Serialization.num 2)]))),
                Testing.universalTestCaseExpected = "[[1, 3], 2]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list with parenthesized expression inside",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.bracketList Serialization.inlineStyle [
                  Serialization.bracketList Serialization.inlineStyle [
                    Serialization.num 1,
                    (Serialization.ifx multOp (Serialization.ifx plusOp (Serialization.num 2) (Serialization.num 3)) (Serialization.ifx plusOp (Serialization.num 1) (Serialization.num 10)))],
                  (Serialization.num 2)]))),
                Testing.universalTestCaseExpected = "[[1, (2 + 3) * (1 + 10)], 2]"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "precedence",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "operators with different precedence - no parens needed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx plusOp (Serialization.ifx multOp (Serialization.num 2) (Serialization.num 3)) (Serialization.ifx multOp (Serialization.num 1) (Serialization.num 10))))),
                Testing.universalTestCaseExpected = "2 * 3 + 1 * 10"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "operators with different precedence - parens needed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx multOp (Serialization.ifx plusOp (Serialization.num 2) (Serialization.num 3)) (Serialization.ifx plusOp (Serialization.num 1) (Serialization.num 10))))),
                Testing.universalTestCaseExpected = "(2 + 3) * (1 + 10)"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "associative operator left nesting",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx multOp (Serialization.cst "x") (Serialization.ifx multOp (Serialization.cst "y") (Serialization.cst "z"))))),
                Testing.universalTestCaseExpected = "x * y * z"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "associative operator right nesting",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Serialization.printExpr (Serialization.parenthesize (Serialization.ifx multOp (Serialization.ifx multOp (Serialization.cst "x") (Serialization.cst "y")) (Serialization.cst "z")))),
                Testing.universalTestCaseExpected = "x * y * z"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
arrowOp :: Ast.Op
arrowOp = Serialization.op "->" (Math.negate 1) Ast.AssociativityRight
caseOp :: Ast.Op
caseOp = Serialization.op "->" 0 Ast.AssociativityNone
gtOp :: Ast.Op
gtOp = Serialization.op ">" 4 Ast.AssociativityNone
lambdaOp :: Ast.Op
lambdaOp = Serialization.op "->" (Math.negate 1) Ast.AssociativityRight
multOp :: Ast.Op
multOp = Serialization.op "*" 7 Ast.AssociativityBoth
plusOp :: Ast.Op
plusOp = Serialization.op "+" 6 Ast.AssociativityBoth
