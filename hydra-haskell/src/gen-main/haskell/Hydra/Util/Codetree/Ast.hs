module Hydra.Util.Codetree.Ast where

import Hydra.Core as Core
import Data.Map
import Data.Set

-- Operator associativity
data Associativity 
  = AssociativityNone 
  | AssociativityLeft 
  | AssociativityRight 
  | AssociativityBoth 
  deriving (Eq, Ord, Read, Show)

_Associativity = "hydra/util/codetree/ast.Associativity"

_Associativity_none = "none"

_Associativity_left = "left"

_Associativity_right = "right"

_Associativity_both = "both"

-- An expression enclosed by brackets
data BracketExpr 
  = BracketExpr {
    bracketExprBrackets :: Brackets,
    bracketExprEnclosed :: Expr}
  deriving (Eq, Ord, Read, Show)

_BracketExpr = "hydra/util/codetree/ast.BracketExpr"

_BracketExpr_brackets = "brackets"

_BracketExpr_enclosed = "enclosed"

-- Matching open and close bracket symbols
data Brackets 
  = Brackets {
    bracketsOpen :: Symbol,
    bracketsClose :: Symbol}
  deriving (Eq, Ord, Read, Show)

_Brackets = "hydra/util/codetree/ast.Brackets"

_Brackets_open = "open"

_Brackets_close = "close"

-- An abstract expression
data Expr 
  = ExprConst Symbol
  | ExprOp OpExpr
  | ExprBrackets BracketExpr
  deriving (Eq, Ord, Read, Show)

_Expr = "hydra/util/codetree/ast.Expr"

_Expr_const = "const"

_Expr_op = "op"

_Expr_brackets = "brackets"

-- An operator symbol
data Op 
  = Op {
    opSymbol :: Symbol,
    opPadding :: Padding,
    opPrecedence :: Precedence,
    opAssociativity :: Associativity}
  deriving (Eq, Ord, Read, Show)

_Op = "hydra/util/codetree/ast.Op"

_Op_symbol = "symbol"

_Op_padding = "padding"

_Op_precedence = "precedence"

_Op_associativity = "associativity"

-- An operator expression
data OpExpr 
  = OpExpr {
    opExprOp :: Op,
    opExprLhs :: Expr,
    opExprRhs :: Expr}
  deriving (Eq, Ord, Read, Show)

_OpExpr = "hydra/util/codetree/ast.OpExpr"

_OpExpr_op = "op"

_OpExpr_lhs = "lhs"

_OpExpr_rhs = "rhs"

-- Left and right padding for an operator
data Padding 
  = Padding {
    paddingLeft :: Ws,
    paddingRight :: Ws}
  deriving (Eq, Ord, Read, Show)

_Padding = "hydra/util/codetree/ast.Padding"

_Padding_left = "left"

_Padding_right = "right"

-- Operator precedence
type Precedence = Int

_Precedence = "hydra/util/codetree/ast.Precedence"

-- Any symbol
type Symbol = String

_Symbol = "hydra/util/codetree/ast.Symbol"

-- One of several classes of whitespace
data Ws 
  = WsNone 
  | WsSpace 
  | WsBreak 
  | WsBreakAndIndent 
  deriving (Eq, Ord, Read, Show)

_Ws = "hydra/util/codetree/ast.Ws"

_Ws_none = "none"

_Ws_space = "space"

_Ws_break = "break"

_Ws_breakAndIndent = "breakAndIndent"