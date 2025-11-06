module Hydra.Sources.Kernel.Types.Ast where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "A model which provides a common syntax tree for Hydra serializers"
  where
    ns = Namespace "hydra.ast"
    def = datatype ns
    ast = typeref ns

    elements = [

      def "Associativity" $
        doc "Operator associativity" $
        enum ["none", "left", "right", "both"],

      def "BlockStyle" $
        doc "Formatting option for code blocks" $
        record [
          "indent">:
            doc "An optional indentation string" $
            optional string,
          "newlineBeforeContent">:
            doc "Whether to place a newline before the content" $
            boolean,
          "newlineAfterContent">:
            doc "Whether to place a newline after the content" $
            boolean],

      def "BracketExpr" $
        doc "An expression enclosed by brackets" $
        record [
          "brackets">:
            doc "The bracket pair enclosing the expression" $
            ast "Brackets",
          "enclosed">:
            doc "The expression within the brackets" $
            ast "Expr",
          "style">:
            doc "The formatting style for the bracketed block" $
            ast "BlockStyle"],

      def "Brackets" $
        doc "Matching open and close bracket symbols" $
        record [
          "open">:
            doc "The opening bracket symbol" $
            ast "Symbol",
          "close">:
            doc "The closing bracket symbol" $
            ast "Symbol"],

      def "Expr" $
        doc "An abstract expression" $
        union [
          "const">:
            doc "A constant symbol" $
            ast "Symbol",
          "indent">:
            doc "An indented expression" $
            ast "IndentedExpression",
          "op">:
            doc "An operator expression" $
            ast "OpExpr",
          "brackets">:
            doc "A bracketed expression" $
            ast "BracketExpr"],

      def "IndentedExpression" $
        doc "An expression indented in a certain style" $
        record [
          "style">:
            doc "The indentation style" $
            ast "IndentStyle",
          "expr">:
            doc "The expression to be indented" $
            ast "Expr"],

      def "IndentStyle" $
        doc "Any of several indentation styles" $
        union [
          "allLines">:
            doc "Indent all lines with the given string" $
            string,
          "subsequentLines">:
            doc "Indent only lines after the first with the given string" $
            string],

      def "Op" $
        doc "An operator symbol" $
        record [
          "symbol">:
            doc "The operator symbol" $
            ast "Symbol",
          "padding">:
            doc "The padding around the operator" $
            ast "Padding",
          "precedence">:
            doc "The precedence of the operator" $
            ast "Precedence",
          "associativity">:
            doc "The associativity of the operator" $
            ast "Associativity"],

      def "OpExpr" $
        doc "An operator expression" $
        record [
          "op">:
            doc "The operator" $
            ast "Op",
          "lhs">:
            doc "The left-hand side operand" $
            ast "Expr",
          "rhs">:
            doc "The right-hand side operand" $
            ast "Expr"],

      def "Padding" $
        doc "Left and right padding for an operator" $
        record [
          "left">:
            doc "Padding to the left of the operator" $
            ast "Ws",
          "right">:
            doc "Padding to the right of the operator" $
            ast "Ws"],

      def "Precedence" $
        doc "Operator precedence" $
        wrap int32,

      def "Symbol" $
        doc "Any symbol" $
        wrap string,

      def "Ws" $
        doc "One of several classes of whitespace" $
        union [
          "none">:
            doc "No whitespace" $
            unit,
          "space">:
            doc "A single space" $
            unit,
          "break">:
            doc "A line break" $
            unit,
          "breakAndIndent">:
            doc "A line break followed by indentation" $
            string,
          "doubleBreak">:
            doc "Two line breaks" $
            unit]]
