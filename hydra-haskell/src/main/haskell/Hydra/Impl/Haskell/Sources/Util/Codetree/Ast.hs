module Hydra.Impl.Haskell.Sources.Util.Codetree.Ast where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


codetreeAstModule = Module codetreeAst []

-- Note: here, the element namespace doubles as a graph name
codetreeAstName = "hydra/util/codetree/ast"

codetreeAst :: Graph Meta
codetreeAst = Graph codetreeAstName elements (const True) hydraCoreName
  where
    def = datatype codetreeAstName
    ast = nominal . qualify codetreeAstName

    elements = [

      def "Associativity" $
        doc "Operator associativity" $
        enum ["none", "left", "right", "both"],

      def "BracketExpr" $
        doc "An expression enclosed by brackets" $
        record [
          field "brackets" $ ast "Brackets",
          field "enclosed" $ ast "Expr"],

      def "Brackets" $
        doc "Matching open and close bracket symbols" $
        record [
          field "open" $ ast "Symbol",
          field "close" $ ast "Symbol"],

      def "Expr" $
        doc "An abstract expression" $
        union [
          field "const" $ ast "Symbol",
          field "op" $ ast "OpExpr",
          field "brackets" $ ast "BracketExpr"],

      def "Op" $
        doc "An operator symbol" $
        record [
          field "symbol" $ ast "Symbol",
          field "padding" $ ast "Padding",
          field "precedence" $ ast "Precedence",
          field "associativity" $ ast "Associativity"],

      def "OpExpr" $
        doc "An operator expression" $
        record [
          field "op" $ ast "Op",
          field "lhs" $ ast "Expr",
          field "rhs" $ ast "Expr"],

      def "Padding" $
        doc "Left and right padding for an operator" $
        record [
          field "left" $ ast "Ws",
          field "right" $ ast "Ws"],

      def "Precedence" $
        doc "Operator precedence" $
        int32,

      def "Symbol" $
        doc "Any symbol"
        string,

      def "Ws" $
        doc "One of several classes of whitespace" $
        enum ["none", "space", "break", "breakAndIndent"]]
