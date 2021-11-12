{-# LANGUAGE DeriveGeneric #-}
module Hydra.Util.Codetree.Ast
  ( Associativity(..)
  , Delimiters(..)
  , Expression_Block(..)
  , Expression_List(..)
  , Expression_Operator(..)
  , Expression(..)
  , Operator(..)
  , Precedence
  , _Associativity
  , _Associativity_left
  , _Associativity_right
  , _Delimiters
  , _Delimiters_close
  , _Delimiters_open
  , _Expression
  , _Expression_Block
  , _Expression_Block_delimiters
  , _Expression_Block_head
  , _Expression_Block_subexpressions
  , _Expression_List
  , _Expression_List_elements
  , _Expression_List_separator
  , _Expression_Operator
  , _Expression_Operator_operands
  , _Expression_Operator_operator
  , _Expression_atomic
  , _Expression_block
  , _Expression_infix
  , _Expression_list
  , _Expression_prefix
  , _Operator
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

data Delimiters
  = Delimiters
    -- | @type optional: string
    { delimitersOpen :: Maybe String
    -- | @type optional: string
    , delimitersClose :: Maybe String } deriving (Eq, Generic, Ord, Read, Show)

data Expression_Block
  = Expression_Block
    -- | @type hydra/util/codetree/ast.Delimiters
    { expressionBlockDelimiters :: Delimiters
    -- | @type optional: hydra/util/codetree/ast.Expression
    , expressionBlockHead :: Maybe Expression
    -- | @type list: hydra/util/codetree/ast.Expression
    , expressionBlockSubexpressions :: [Expression] } deriving (Eq, Generic, Ord, Read, Show)

data Expression_List
  = Expression_List
    -- | @type string
    { expressionListSeparator :: String
    -- | @type list: hydra/util/codetree/ast.Expression
    , expressionListElements :: [Expression] } deriving (Eq, Generic, Ord, Read, Show)

data Expression_Operator
  = Expression_Operator
    -- | @type hydra/util/codetree/ast.Operator
    { expressionOperatorOperator :: Operator
    -- | @type list: hydra/util/codetree/ast.Expression
    , expressionOperatorOperands :: [Expression] } deriving (Eq, Generic, Ord, Read, Show)

data Expression
  -- | @type string
  = ExpressionAtomic String
  -- | @type hydra/util/codetree/ast.Expression.Block
  | ExpressionBlock Expression_Block
  -- | @type hydra/util/codetree/ast.Expression.Operator
  | ExpressionInfix Expression_Operator
  -- | @type hydra/util/codetree/ast.Expression.List
  | ExpressionList Expression_List
  -- | @type hydra/util/codetree/ast.Expression.Operator
  | ExpressionPrefix Expression_Operator deriving (Eq, Generic, Ord, Read, Show)

data Operator
  = Operator
    -- | @type string
    { operatorSymbol :: String
    -- | @type hydra/util/codetree/ast.Precedence
    , operatorPrecedence :: Precedence
    -- | @type hydra/util/codetree/ast.Associativity
    , operatorAssociativity :: Associativity } deriving (Eq, Generic, Ord, Read, Show)

-- | @type integer
type Precedence = Int

_Associativity = "hydra/util/codetree/ast.Associativity" :: String
_Associativity_left = "left" :: String
_Associativity_right = "right" :: String
_Delimiters = "hydra/util/codetree/ast.Delimiters" :: String
_Delimiters_close = "close" :: String
_Delimiters_open = "open" :: String
_Expression = "hydra/util/codetree/ast.Expression" :: String
_Expression_Block = "hydra/util/codetree/ast.Expression_Block" :: String
_Expression_Block_delimiters = "delimiters" :: String
_Expression_Block_head = "head" :: String
_Expression_Block_subexpressions = "subexpressions" :: String
_Expression_List = "hydra/util/codetree/ast.Expression_List" :: String
_Expression_List_elements = "elements" :: String
_Expression_List_separator = "separator" :: String
_Expression_Operator = "hydra/util/codetree/ast.Expression_Operator" :: String
_Expression_Operator_operands = "operands" :: String
_Expression_Operator_operator = "operator" :: String
_Expression_atomic = "atomic" :: String
_Expression_block = "block" :: String
_Expression_infix = "infix" :: String
_Expression_list = "list" :: String
_Expression_prefix = "prefix" :: String
_Operator = "hydra/util/codetree/ast.Operator" :: String
_Operator_associativity = "associativity" :: String
_Operator_precedence = "precedence" :: String
_Operator_symbol = "symbol" :: String
_Precedence = "hydra/util/codetree/ast.Precedence" :: String
