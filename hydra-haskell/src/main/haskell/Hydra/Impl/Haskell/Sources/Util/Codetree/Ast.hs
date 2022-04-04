module Hydra.Impl.Haskell.Sources.Util.Codetree.Ast where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard

-- Note: here, the element namespace doubles as a graph name
codetreeAstName = "hydra/util/codetree/ast"

codetreeAst :: Graph Meta
codetreeAst = Graph codetreeAstName elements (const True) hydraCoreName
  where
    astDef = datatype codetreeAstName
    ast = nominal . qualify codetreeAstName

    elements = [

      astDef "Associativity"
        "Operator associativity" $
        enum ["none", "left", "right", "both"],

      astDef "BracketExpr"
        "An expression enclosed by brackets" $
        record [
          field "brackets" $ ast "Brackets",
          field "enclosed" $ ast "Expr"],

      astDef "Brackets"
        "Matching open and close bracket symbols" $
        record [
          field "open" $ ast "Symbol",
          field "close" $ ast "Symbol"],

      astDef "Expr"
        "An abstract expression" $
        union [
          field "const" $ ast "Symbol",
          field "op" $ ast "OpExpr",
          field "brackets" $ ast "BracketExpr"],

      astDef "Op"
        "An operator symbol" $
        record [
          field "symbol" $ ast "Symbol",
          field "padding" $ ast "Padding",
          field "precedence" $ ast "Precedence",
          field "associativity" $ ast "Associativity"],

      astDef "OpExpr"
        "An operator expression" $
        record [
          field "op" $ ast "Op",
          field "lhs" $ ast "Expr",
          field "rhs" $ ast "Expr"],

      astDef "Padding"
        "Left and right padding for an operator" $
        record [
          field "left" $ ast "Ws",
          field "right" $ ast "Ws"],

      astDef "Precedence"
        "Operator precedence" $
        int32,

      astDef "Symbol"
        "Any symbol" $
        string,

      astDef "Ws"
        "One of several classes of whitespace" $
        enum ["none", "space", "break", "breakAndIndent"]]
