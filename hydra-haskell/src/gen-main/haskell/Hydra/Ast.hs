-- | A model which provides a common syntax tree for Hydra serializers

module Hydra.Ast where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Operator associativity
data Associativity = 
  AssociativityNone  |
  AssociativityLeft  |
  AssociativityRight  |
  AssociativityBoth 
  deriving (Eq, Ord, Read, Show)

_Associativity = (Core.Name "hydra/ast.Associativity")

_Associativity_none = (Core.Name "none")

_Associativity_left = (Core.Name "left")

_Associativity_right = (Core.Name "right")

_Associativity_both = (Core.Name "both")

_Associativity_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.Associativity"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "both"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | Formatting option for code blocks
data BlockStyle = 
  BlockStyle {
    blockStyleIndent :: (Maybe String),
    blockStyleNewlineBeforeContent :: Bool,
    blockStyleNewlineAfterContent :: Bool}
  deriving (Eq, Ord, Read, Show)

_BlockStyle = (Core.Name "hydra/ast.BlockStyle")

_BlockStyle_indent = (Core.Name "indent")

_BlockStyle_newlineBeforeContent = (Core.Name "newlineBeforeContent")

_BlockStyle_newlineAfterContent = (Core.Name "newlineAfterContent")

_BlockStyle_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.BlockStyle"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "indent"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "newlineBeforeContent"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "newlineAfterContent"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

-- | An expression enclosed by brackets
data BracketExpr = 
  BracketExpr {
    bracketExprBrackets :: Brackets,
    bracketExprEnclosed :: Expr,
    bracketExprStyle :: BlockStyle}
  deriving (Eq, Ord, Read, Show)

_BracketExpr = (Core.Name "hydra/ast.BracketExpr")

_BracketExpr_brackets = (Core.Name "brackets")

_BracketExpr_enclosed = (Core.Name "enclosed")

_BracketExpr_style = (Core.Name "style")

_BracketExpr_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.BracketExpr"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "brackets"),
      Core.fieldTypeType = _Brackets_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enclosed"),
      Core.fieldTypeType = _Expr_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "style"),
      Core.fieldTypeType = _BlockStyle_type_}]}))

-- | Matching open and close bracket symbols
data Brackets = 
  Brackets {
    bracketsOpen :: Symbol,
    bracketsClose :: Symbol}
  deriving (Eq, Ord, Read, Show)

_Brackets = (Core.Name "hydra/ast.Brackets")

_Brackets_open = (Core.Name "open")

_Brackets_close = (Core.Name "close")

_Brackets_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.Brackets"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "open"),
      Core.fieldTypeType = _Symbol_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "close"),
      Core.fieldTypeType = _Symbol_type_}]}))

-- | An abstract expression
data Expr = 
  ExprConst Symbol |
  ExprIndent IndentedExpression |
  ExprOp OpExpr |
  ExprBrackets BracketExpr
  deriving (Eq, Ord, Read, Show)

_Expr = (Core.Name "hydra/ast.Expr")

_Expr_const = (Core.Name "const")

_Expr_indent = (Core.Name "indent")

_Expr_op = (Core.Name "op")

_Expr_brackets = (Core.Name "brackets")

_Expr_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.Expr"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "const"),
      Core.fieldTypeType = _Symbol_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "indent"),
      Core.fieldTypeType = _IndentedExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "op"),
      Core.fieldTypeType = _OpExpr_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "brackets"),
      Core.fieldTypeType = _BracketExpr_type_}]}))

-- | An expression indented in a certain style
data IndentedExpression = 
  IndentedExpression {
    indentedExpressionStyle :: IndentStyle,
    indentedExpressionExpr :: Expr}
  deriving (Eq, Ord, Read, Show)

_IndentedExpression = (Core.Name "hydra/ast.IndentedExpression")

_IndentedExpression_style = (Core.Name "style")

_IndentedExpression_expr = (Core.Name "expr")

_IndentedExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.IndentedExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "style"),
      Core.fieldTypeType = _IndentStyle_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expr"),
      Core.fieldTypeType = _Expr_type_}]}))

-- | Any of several indentation styles
data IndentStyle = 
  IndentStyleAllLines String |
  IndentStyleSubsequentLines String
  deriving (Eq, Ord, Read, Show)

_IndentStyle = (Core.Name "hydra/ast.IndentStyle")

_IndentStyle_allLines = (Core.Name "allLines")

_IndentStyle_subsequentLines = (Core.Name "subsequentLines")

_IndentStyle_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.IndentStyle"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "allLines"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subsequentLines"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

-- | An operator symbol
data Op = 
  Op {
    opSymbol :: Symbol,
    opPadding :: Padding,
    opPrecedence :: Precedence,
    opAssociativity :: Associativity}
  deriving (Eq, Ord, Read, Show)

_Op = (Core.Name "hydra/ast.Op")

_Op_symbol = (Core.Name "symbol")

_Op_padding = (Core.Name "padding")

_Op_precedence = (Core.Name "precedence")

_Op_associativity = (Core.Name "associativity")

_Op_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.Op"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "symbol"),
      Core.fieldTypeType = _Symbol_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "padding"),
      Core.fieldTypeType = _Padding_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "precedence"),
      Core.fieldTypeType = _Precedence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "associativity"),
      Core.fieldTypeType = _Associativity_type_}]}))

-- | An operator expression
data OpExpr = 
  OpExpr {
    opExprOp :: Op,
    opExprLhs :: Expr,
    opExprRhs :: Expr}
  deriving (Eq, Ord, Read, Show)

_OpExpr = (Core.Name "hydra/ast.OpExpr")

_OpExpr_op = (Core.Name "op")

_OpExpr_lhs = (Core.Name "lhs")

_OpExpr_rhs = (Core.Name "rhs")

_OpExpr_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.OpExpr"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "op"),
      Core.fieldTypeType = _Op_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _Expr_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Expr_type_}]}))

-- | Left and right padding for an operator
data Padding = 
  Padding {
    paddingLeft :: Ws,
    paddingRight :: Ws}
  deriving (Eq, Ord, Read, Show)

_Padding = (Core.Name "hydra/ast.Padding")

_Padding_left = (Core.Name "left")

_Padding_right = (Core.Name "right")

_Padding_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.Padding"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _Ws_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _Ws_type_}]}))

-- | Operator precedence
newtype Precedence = 
  Precedence {
    unPrecedence :: Int}
  deriving (Eq, Ord, Read, Show)

_Precedence = (Core.Name "hydra/ast.Precedence")

_Precedence_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))

-- | Any symbol
newtype Symbol = 
  Symbol {
    unSymbol :: String}
  deriving (Eq, Ord, Read, Show)

_Symbol = (Core.Name "hydra/ast.Symbol")

_Symbol_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | One of several classes of whitespace
data Ws = 
  WsNone  |
  WsSpace  |
  WsBreak  |
  WsBreakAndIndent String |
  WsDoubleBreak 
  deriving (Eq, Ord, Read, Show)

_Ws = (Core.Name "hydra/ast.Ws")

_Ws_none = (Core.Name "none")

_Ws_space = (Core.Name "space")

_Ws_break = (Core.Name "break")

_Ws_breakAndIndent = (Core.Name "breakAndIndent")

_Ws_doubleBreak = (Core.Name "doubleBreak")

_Ws_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/ast.Ws"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "space"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "break"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "breakAndIndent"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "doubleBreak"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))