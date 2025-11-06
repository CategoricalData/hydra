-- | A model which provides a common syntax tree for Hydra serializers

module Hydra.Ast where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Operator associativity
data Associativity = 
  AssociativityNone  |
  AssociativityLeft  |
  AssociativityRight  |
  AssociativityBoth 
  deriving (Eq, Ord, Read, Show)

_Associativity = (Core.Name "hydra.ast.Associativity")

_Associativity_none = (Core.Name "none")

_Associativity_left = (Core.Name "left")

_Associativity_right = (Core.Name "right")

_Associativity_both = (Core.Name "both")

-- | Formatting option for code blocks
data BlockStyle = 
  BlockStyle {
    -- | An optional indentation string
    blockStyleIndent :: (Maybe String),
    -- | Whether to place a newline before the content
    blockStyleNewlineBeforeContent :: Bool,
    -- | Whether to place a newline after the content
    blockStyleNewlineAfterContent :: Bool}
  deriving (Eq, Ord, Read, Show)

_BlockStyle = (Core.Name "hydra.ast.BlockStyle")

_BlockStyle_indent = (Core.Name "indent")

_BlockStyle_newlineBeforeContent = (Core.Name "newlineBeforeContent")

_BlockStyle_newlineAfterContent = (Core.Name "newlineAfterContent")

-- | An expression enclosed by brackets
data BracketExpr = 
  BracketExpr {
    -- | The bracket pair enclosing the expression
    bracketExprBrackets :: Brackets,
    -- | The expression within the brackets
    bracketExprEnclosed :: Expr,
    -- | The formatting style for the bracketed block
    bracketExprStyle :: BlockStyle}
  deriving (Eq, Ord, Read, Show)

_BracketExpr = (Core.Name "hydra.ast.BracketExpr")

_BracketExpr_brackets = (Core.Name "brackets")

_BracketExpr_enclosed = (Core.Name "enclosed")

_BracketExpr_style = (Core.Name "style")

-- | Matching open and close bracket symbols
data Brackets = 
  Brackets {
    -- | The opening bracket symbol
    bracketsOpen :: Symbol,
    -- | The closing bracket symbol
    bracketsClose :: Symbol}
  deriving (Eq, Ord, Read, Show)

_Brackets = (Core.Name "hydra.ast.Brackets")

_Brackets_open = (Core.Name "open")

_Brackets_close = (Core.Name "close")

-- | An abstract expression
data Expr = 
  -- | A constant symbol
  ExprConst Symbol |
  -- | An indented expression
  ExprIndent IndentedExpression |
  -- | An operator expression
  ExprOp OpExpr |
  -- | A bracketed expression
  ExprBrackets BracketExpr
  deriving (Eq, Ord, Read, Show)

_Expr = (Core.Name "hydra.ast.Expr")

_Expr_const = (Core.Name "const")

_Expr_indent = (Core.Name "indent")

_Expr_op = (Core.Name "op")

_Expr_brackets = (Core.Name "brackets")

-- | An expression indented in a certain style
data IndentedExpression = 
  IndentedExpression {
    -- | The indentation style
    indentedExpressionStyle :: IndentStyle,
    -- | The expression to be indented
    indentedExpressionExpr :: Expr}
  deriving (Eq, Ord, Read, Show)

_IndentedExpression = (Core.Name "hydra.ast.IndentedExpression")

_IndentedExpression_style = (Core.Name "style")

_IndentedExpression_expr = (Core.Name "expr")

-- | Any of several indentation styles
data IndentStyle = 
  -- | Indent all lines with the given string
  IndentStyleAllLines String |
  -- | Indent only lines after the first with the given string
  IndentStyleSubsequentLines String
  deriving (Eq, Ord, Read, Show)

_IndentStyle = (Core.Name "hydra.ast.IndentStyle")

_IndentStyle_allLines = (Core.Name "allLines")

_IndentStyle_subsequentLines = (Core.Name "subsequentLines")

-- | An operator symbol
data Op = 
  Op {
    -- | The operator symbol
    opSymbol :: Symbol,
    -- | The padding around the operator
    opPadding :: Padding,
    -- | The precedence of the operator
    opPrecedence :: Precedence,
    -- | The associativity of the operator
    opAssociativity :: Associativity}
  deriving (Eq, Ord, Read, Show)

_Op = (Core.Name "hydra.ast.Op")

_Op_symbol = (Core.Name "symbol")

_Op_padding = (Core.Name "padding")

_Op_precedence = (Core.Name "precedence")

_Op_associativity = (Core.Name "associativity")

-- | An operator expression
data OpExpr = 
  OpExpr {
    -- | The operator
    opExprOp :: Op,
    -- | The left-hand side operand
    opExprLhs :: Expr,
    -- | The right-hand side operand
    opExprRhs :: Expr}
  deriving (Eq, Ord, Read, Show)

_OpExpr = (Core.Name "hydra.ast.OpExpr")

_OpExpr_op = (Core.Name "op")

_OpExpr_lhs = (Core.Name "lhs")

_OpExpr_rhs = (Core.Name "rhs")

-- | Left and right padding for an operator
data Padding = 
  Padding {
    -- | Padding to the left of the operator
    paddingLeft :: Ws,
    -- | Padding to the right of the operator
    paddingRight :: Ws}
  deriving (Eq, Ord, Read, Show)

_Padding = (Core.Name "hydra.ast.Padding")

_Padding_left = (Core.Name "left")

_Padding_right = (Core.Name "right")

-- | Operator precedence
newtype Precedence = 
  Precedence {
    unPrecedence :: Int}
  deriving (Eq, Ord, Read, Show)

_Precedence = (Core.Name "hydra.ast.Precedence")

-- | Any symbol
newtype Symbol = 
  Symbol {
    unSymbol :: String}
  deriving (Eq, Ord, Read, Show)

_Symbol = (Core.Name "hydra.ast.Symbol")

-- | One of several classes of whitespace
data Ws = 
  -- | No whitespace
  WsNone  |
  -- | A single space
  WsSpace  |
  -- | A line break
  WsBreak  |
  -- | A line break followed by indentation
  WsBreakAndIndent String |
  -- | Two line breaks
  WsDoubleBreak 
  deriving (Eq, Ord, Read, Show)

_Ws = (Core.Name "hydra.ast.Ws")

_Ws_none = (Core.Name "none")

_Ws_space = (Core.Name "space")

_Ws_break = (Core.Name "break")

_Ws_breakAndIndent = (Core.Name "breakAndIndent")

_Ws_doubleBreak = (Core.Name "doubleBreak")
