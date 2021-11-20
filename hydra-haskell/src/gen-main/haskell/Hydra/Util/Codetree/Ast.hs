{-# LANGUAGE DeriveGeneric #-}
module Hydra.Util.Codetree.Ast
  ( Associativity(..)
  , BlockExpression(..)
  , Delimiters(..)
  , Expression(..)
  , ListExpression(..)
  , Operator(..)
  , OperatorExpression(..)
  , Precedence
  , _Associativity
  , _Associativity_left
  , _Associativity_right
  , _BlockExpression
  , _BlockExpression_delimiters
  , _BlockExpression_head
  , _BlockExpression_subexpressions
  , _Delimiters
  , _Delimiters_close
  , _Delimiters_open
  , _Expression
  , _Expression_atomic
  , _Expression_block
  , _Expression_infix
  , _Expression_list
  , _Expression_prefix
  , _ListExpression
  , _ListExpression_elements
  , _ListExpression_separator
  , _Operator
  , _OperatorExpression
  , _OperatorExpression_operands
  , _OperatorExpression_operator
  , _Operator_associativity
  , _Operator_precedence
  , _Operator_symbol
  , _Precedence
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

data Associativity
  = AssociativityLeft
  | AssociativityRight deriving (Eq, Generic, Ord, Read, Show)

data BlockExpression
  = BlockExpression
    -- | @type hydra/util/codetree/ast.Delimiters
    { blockExpressionDelimiters :: Delimiters
    -- | @type optional: hydra/util/codetree/ast.Expression
    , blockExpressionHead :: Maybe Expression
    -- | @type list: hydra/util/codetree/ast.Expression
    , blockExpressionSubexpressions :: [Expression] } deriving (Eq, Generic, Ord, Read, Show)

data Delimiters
  = Delimiters
    -- | @type optional: string
    { delimitersOpen :: Maybe String
    -- | @type optional: string
    , delimitersClose :: Maybe String } deriving (Eq, Generic, Ord, Read, Show)

data Expression
  -- | @type string
  = ExpressionAtomic String
  -- | @type hydra/util/codetree/ast.BlockExpression
  | ExpressionBlock BlockExpression
  -- | @type hydra/util/codetree/ast.OperatorExpression
  | ExpressionInfix OperatorExpression
  -- | @type hydra/util/codetree/ast.ListExpression
  | ExpressionList ListExpression
  -- | @type hydra/util/codetree/ast.OperatorExpression
  | ExpressionPrefix OperatorExpression deriving (Eq, Generic, Ord, Read, Show)

data ListExpression
  = ListExpression
    -- | @type string
    { listExpressionSeparator :: String
    -- | @type list: hydra/util/codetree/ast.Expression
    , listExpressionElements :: [Expression] } deriving (Eq, Generic, Ord, Read, Show)

data Operator
  = Operator
    -- | @type string
    { operatorSymbol :: String
    -- | @type hydra/util/codetree/ast.Precedence
    , operatorPrecedence :: Precedence
    -- | @type hydra/util/codetree/ast.Associativity
    , operatorAssociativity :: Associativity } deriving (Eq, Generic, Ord, Read, Show)

data OperatorExpression
  = OperatorExpression
    -- | @type hydra/util/codetree/ast.Operator
    { operatorExpressionOperator :: Operator
    -- | @type list: hydra/util/codetree/ast.Expression
    , operatorExpressionOperands :: [Expression] } deriving (Eq, Generic, Ord, Read, Show)

-- | @type integer
type Precedence = Int

_Associativity = "hydra/util/codetree/ast.Associativity" :: String
_Associativity_left = "left" :: String
_Associativity_right = "right" :: String
_BlockExpression = "hydra/util/codetree/ast.BlockExpression" :: String
_BlockExpression_delimiters = "delimiters" :: String
_BlockExpression_head = "head" :: String
_BlockExpression_subexpressions = "subexpressions" :: String
_Delimiters = "hydra/util/codetree/ast.Delimiters" :: String
_Delimiters_close = "close" :: String
_Delimiters_open = "open" :: String
_Expression = "hydra/util/codetree/ast.Expression" :: String
_Expression_atomic = "atomic" :: String
_Expression_block = "block" :: String
_Expression_infix = "infix" :: String
_Expression_list = "list" :: String
_Expression_prefix = "prefix" :: String
_ListExpression = "hydra/util/codetree/ast.ListExpression" :: String
_ListExpression_elements = "elements" :: String
_ListExpression_separator = "separator" :: String
_Operator = "hydra/util/codetree/ast.Operator" :: String
_OperatorExpression = "hydra/util/codetree/ast.OperatorExpression" :: String
_OperatorExpression_operands = "operands" :: String
_OperatorExpression_operator = "operator" :: String
_Operator_associativity = "associativity" :: String
_Operator_precedence = "precedence" :: String
_Operator_symbol = "symbol" :: String
_Precedence = "hydra/util/codetree/ast.Precedence" :: String
