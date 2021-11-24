{-# LANGUAGE DeriveGeneric #-}
module Hydra.Util.Codetree.Ast
  ( Associativity(..)
  , BracketExpr(..)
  , Brackets(..)
  , Expr(..)
  , Op(..)
  , OpExpr(..)
  , Padding(..)
  , Precedence
  , Symbol
  , Ws(..)
  , _Associativity
  , _Associativity_both
  , _Associativity_left
  , _Associativity_none
  , _Associativity_right
  , _BracketExpr
  , _BracketExpr_brackets
  , _BracketExpr_enclosed
  , _Brackets
  , _Brackets_close
  , _Brackets_open
  , _Expr
  , _Expr_brackets
  , _Expr_const
  , _Expr_op
  , _Op
  , _OpExpr
  , _OpExpr_lhs
  , _OpExpr_op
  , _OpExpr_rhs
  , _Op_associativity
  , _Op_padding
  , _Op_precedence
  , _Op_symbol
  , _Padding
  , _Padding_left
  , _Padding_right
  , _Precedence
  , _Symbol
  , _Ws
  , _Ws_break
  , _Ws_breakAndIndent
  , _Ws_none
  , _Ws_space
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

data Associativity
  = AssociativityNone
  | AssociativityLeft
  | AssociativityRight
  | AssociativityBoth deriving (Eq, Generic, Ord, Read, Show)

data BracketExpr
  = BracketExpr
    -- | @type hydra/util/codetree/ast.Brackets
    { bracketExprBrackets :: Brackets
    -- | @type hydra/util/codetree/ast.Expr
    , bracketExprEnclosed :: Expr } deriving (Eq, Generic, Ord, Read, Show)

data Brackets
  = Brackets
    -- | @type hydra/util/codetree/ast.Symbol
    { bracketsOpen :: Symbol
    -- | @type hydra/util/codetree/ast.Symbol
    , bracketsClose :: Symbol } deriving (Eq, Generic, Ord, Read, Show)

data Expr
  -- | @type hydra/util/codetree/ast.Symbol
  = ExprConst Symbol
  -- | @type hydra/util/codetree/ast.OpExpr
  | ExprOp OpExpr
  -- | @type hydra/util/codetree/ast.BracketExpr
  | ExprBrackets BracketExpr deriving (Eq, Generic, Ord, Read, Show)

data Op
  = Op
    -- | @type hydra/util/codetree/ast.Symbol
    { opSymbol :: Symbol
    -- | @type hydra/util/codetree/ast.Padding
    , opPadding :: Padding
    -- | @type hydra/util/codetree/ast.Precedence
    , opPrecedence :: Precedence
    -- | @type hydra/util/codetree/ast.Associativity
    , opAssociativity :: Associativity } deriving (Eq, Generic, Ord, Read, Show)

data OpExpr
  = OpExpr
    -- | @type hydra/util/codetree/ast.Op
    { opExprOp :: Op
    -- | @type hydra/util/codetree/ast.Expr
    , opExprLhs :: Expr
    -- | @type hydra/util/codetree/ast.Expr
    , opExprRhs :: Expr } deriving (Eq, Generic, Ord, Read, Show)

data Padding
  = Padding
    -- | @type hydra/util/codetree/ast.Ws
    { paddingLeft :: Ws
    -- | @type hydra/util/codetree/ast.Ws
    , paddingRight :: Ws } deriving (Eq, Generic, Ord, Read, Show)

-- | @type integer
type Precedence = Int

-- | @type string
type Symbol = String

data Ws
  = WsNone
  | WsSpace
  | WsBreak
  | WsBreakAndIndent deriving (Eq, Generic, Ord, Read, Show)

_Associativity = "hydra/util/codetree/ast.Associativity" :: String
_Associativity_both = "both" :: String
_Associativity_left = "left" :: String
_Associativity_none = "none" :: String
_Associativity_right = "right" :: String
_BracketExpr = "hydra/util/codetree/ast.BracketExpr" :: String
_BracketExpr_brackets = "brackets" :: String
_BracketExpr_enclosed = "enclosed" :: String
_Brackets = "hydra/util/codetree/ast.Brackets" :: String
_Brackets_close = "close" :: String
_Brackets_open = "open" :: String
_Expr = "hydra/util/codetree/ast.Expr" :: String
_Expr_brackets = "brackets" :: String
_Expr_const = "const" :: String
_Expr_op = "op" :: String
_Op = "hydra/util/codetree/ast.Op" :: String
_OpExpr = "hydra/util/codetree/ast.OpExpr" :: String
_OpExpr_lhs = "lhs" :: String
_OpExpr_op = "op" :: String
_OpExpr_rhs = "rhs" :: String
_Op_associativity = "associativity" :: String
_Op_padding = "padding" :: String
_Op_precedence = "precedence" :: String
_Op_symbol = "symbol" :: String
_Padding = "hydra/util/codetree/ast.Padding" :: String
_Padding_left = "left" :: String
_Padding_right = "right" :: String
_Precedence = "hydra/util/codetree/ast.Precedence" :: String
_Symbol = "hydra/util/codetree/ast.Symbol" :: String
_Ws = "hydra/util/codetree/ast.Ws" :: String
_Ws_break = "break" :: String
_Ws_breakAndIndent = "breakAndIndent" :: String
_Ws_none = "none" :: String
_Ws_space = "space" :: String
