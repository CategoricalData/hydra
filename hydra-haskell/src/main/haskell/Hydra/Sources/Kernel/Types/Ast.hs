module Hydra.Sources.Kernel.Types.Ast where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (op, brackets)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ast"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "A model which provides a common syntax tree for Hydra serializers"
  where
    elements = [
      associativity,
      blockStyle,
      bracketExpr,
      brackets,
      expr,
      indentedExpression,
      indentStyle,
      op,
      opExpr,
      padding,
      precedence,
      symbol,
      ws]

associativity :: Binding
associativity = define "Associativity" $
  doc "Operator associativity" $
  T.enum ["none", "left", "right", "both"]

blockStyle :: Binding
blockStyle = define "BlockStyle" $
  doc "Formatting option for code blocks" $
  T.record [
    "indent">:
      doc "An optional indentation string" $
      T.maybe T.string,
    "newlineBeforeContent">:
      doc "Whether to place a newline before the content" $
      T.boolean,
    "newlineAfterContent">:
      doc "Whether to place a newline after the content" $
      T.boolean]

bracketExpr :: Binding
bracketExpr = define "BracketExpr" $
  doc "An expression enclosed by brackets" $
  T.record [
    "brackets">:
      doc "The bracket pair enclosing the expression"
      brackets,
    "enclosed">:
      doc "The expression within the brackets"
      expr,
    "style">:
      doc "The formatting style for the bracketed block"
      blockStyle]

brackets :: Binding
brackets = define "Brackets" $
  doc "Matching open and close bracket symbols" $
  T.record [
    "open">:
      doc "The opening bracket symbol"
      symbol,
    "close">:
      doc "The closing bracket symbol"
      symbol]

expr :: Binding
expr = define "Expr" $
  doc "An abstract expression" $
  T.union [
    "const">:
      doc "A constant symbol"
      symbol,
    "indent">:
      doc "An indented expression"
      indentedExpression,
    "op">:
      doc "An operator expression"
      opExpr,
    "brackets">:
      doc "A bracketed expression"
      bracketExpr]

indentedExpression :: Binding
indentedExpression = define "IndentedExpression" $
  doc "An expression indented in a certain style" $
  T.record [
    "style">:
      doc "The indentation style"
      indentStyle,
    "expr">:
      doc "The expression to be indented"
      expr]

indentStyle :: Binding
indentStyle = define "IndentStyle" $
  doc "Any of several indentation styles" $
  T.union [
    "allLines">:
      doc "Indent all lines with the given string" $
      T.string,
    "subsequentLines">:
      doc "Indent only lines after the first with the given string" $
      T.string]

op :: Binding
op = define "Op" $
  doc "An operator symbol" $
  T.record [
    "symbol">:
      doc "The operator symbol"
      symbol,
    "padding">:
      doc "The padding around the operator"
      padding,
    "precedence">:
      doc "The precedence of the operator"
      precedence,
    "associativity">:
      doc "The associativity of the operator"
      associativity]

opExpr :: Binding
opExpr = define "OpExpr" $
  doc "An operator expression" $
  T.record [
    "op">:
      doc "The operator"
      op,
    "lhs">:
      doc "The left-hand side operand"
      expr,
    "rhs">:
      doc "The right-hand side operand"
      expr]

padding :: Binding
padding = define "Padding" $
  doc "Left and right padding for an operator" $
  T.record [
    "left">:
      doc "Padding to the left of the operator"
      ws,
    "right">:
      doc "Padding to the right of the operator"
      ws]

precedence :: Binding
precedence = define "Precedence" $
  doc "Operator precedence" $
  T.wrap T.int32

symbol :: Binding
symbol = define "Symbol" $
  doc "Any symbol" $
  T.wrap T.string

ws :: Binding
ws = define "Ws" $
  doc "One of several classes of whitespace" $
  T.union [
    "none">:
      doc "No whitespace" $
      T.unit,
    "space">:
      doc "A single space" $
      T.unit,
    "break">:
      doc "A line break" $
      T.unit,
    "breakAndIndent">:
      doc "A line break followed by indentation" $
      T.string,
    "doubleBreak">:
      doc "Two line breaks" $
      T.unit]
