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
          "indent">: optional string,
          "newlineBeforeContent">: boolean,
          "newlineAfterContent">: boolean],

      def "BracketExpr" $
        doc "An expression enclosed by brackets" $
        record [
          "brackets">: ast "Brackets",
          "enclosed">: ast "Expr",
          "style">: ast "BlockStyle"],

      def "Brackets" $
        doc "Matching open and close bracket symbols" $
        record [
          "open">: ast "Symbol",
          "close">: ast "Symbol"],

      def "Expr" $
        doc "An abstract expression" $
        union [
          "const">: ast "Symbol",
          "indent">: ast "IndentedExpression",
          "op">: ast "OpExpr",
          "brackets">: ast "BracketExpr"],

      def "IndentedExpression" $
        doc "An expression indented in a certain style" $
        record [
          "style">: ast "IndentStyle",
          "expr">: ast "Expr"],

      def "IndentStyle" $
        doc "Any of several indentation styles" $
        union [
          "allLines">: string,
          "subsequentLines">: string],

      def "Op" $
        doc "An operator symbol" $
        record [
          "symbol">: ast "Symbol",
          "padding">: ast "Padding",
          "precedence">: ast "Precedence",
          "associativity">: ast "Associativity"],

      def "OpExpr" $
        doc "An operator expression" $
        record [
          "op">: ast "Op",
          "lhs">: ast "Expr",
          "rhs">: ast "Expr"],

      def "Padding" $
        doc "Left and right padding for an operator" $
        record [
          "left">: ast "Ws",
          "right">: ast "Ws"],

      def "Precedence" $
        doc "Operator precedence" $
        wrap int32,

      def "Symbol" $
        doc "Any symbol" $
        wrap string,

      def "Ws" $
        doc "One of several classes of whitespace" $
        union [
          "none">: unit,
          "space">: unit,
          "break">: unit,
          "breakAndIndent">: string,
          "doubleBreak">: unit]]
