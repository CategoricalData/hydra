-- Note: this is an automatically generated file. Do not edit.

-- | AST operators for Rust serialization

module Hydra.Rust.Operators where

import qualified Hydra.Ast as Ast
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Add-assign operator (+=)
addAssignOp :: Ast.Op
addAssignOp = Serialization.op "+=" 1 Ast.AssociativityRight

-- | Addition operator (+)
addOp :: Ast.Op
addOp = Serialization.op "+" 10 Ast.AssociativityLeft

-- | Logical AND operator (&&)
andOp :: Ast.Op
andOp = Serialization.op "&&" 4 Ast.AssociativityLeft

-- | Function application operator (whitespace)
appOp :: Ast.Op
appOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol ""),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsSpace},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityLeft}

-- | Return type arrow (->)
arrowOp :: Ast.Op
arrowOp = Serialization.op "->" 0 Ast.AssociativityRight

-- | Type cast operator (as)
asOp :: Ast.Op
asOp = Serialization.op "as" 12 Ast.AssociativityLeft

-- | Assignment operator (=)
assignOp :: Ast.Op
assignOp = Serialization.op "=" 1 Ast.AssociativityRight

-- | Bitwise and-assign operator (&=)
bitAndAssignOp :: Ast.Op
bitAndAssignOp = Serialization.op "&=" 1 Ast.AssociativityRight

-- | Bitwise AND operator (&)
bitAndOp :: Ast.Op
bitAndOp = Serialization.op "&" 8 Ast.AssociativityLeft

-- | Bitwise or-assign operator (|=)
bitOrAssignOp :: Ast.Op
bitOrAssignOp = Serialization.op "|=" 1 Ast.AssociativityRight

-- | Bitwise OR operator (|)
bitOrOp :: Ast.Op
bitOrOp = Serialization.op "|" 6 Ast.AssociativityLeft

-- | Bitwise xor-assign operator (^=)
bitXorAssignOp :: Ast.Op
bitXorAssignOp = Serialization.op "^=" 1 Ast.AssociativityRight

-- | Bitwise XOR operator (^)
bitXorOp :: Ast.Op
bitXorOp = Serialization.op "^" 7 Ast.AssociativityLeft

-- | Type annotation (::) for let statements
colonColonOp :: Ast.Op
colonColonOp = Serialization.op ":" 0 Ast.AssociativityNone

-- | Type ascription operator (:)
colonOp :: Ast.Op
colonOp = Serialization.op ":" 12 Ast.AssociativityLeft

-- | Dereference operator (*)
derefOp :: Ast.Op
derefOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "*"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 13),
      Ast.opAssociativity = Ast.AssociativityNone}

-- | Div-assign operator (/=)
divAssignOp :: Ast.Op
divAssignOp = Serialization.op "/=" 1 Ast.AssociativityRight

-- | Division operator (/)
divOp :: Ast.Op
divOp = Serialization.op "/" 11 Ast.AssociativityLeft

-- | Path separator (::)
doubleColonOp :: Ast.Op
doubleColonOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "::"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 15),
      Ast.opAssociativity = Ast.AssociativityLeft}

-- | Equality operator (==)
eqOp :: Ast.Op
eqOp = Serialization.op "==" 5 Ast.AssociativityNone

-- | Match arm arrow (=>)
fatArrowOp :: Ast.Op
fatArrowOp = Serialization.op "=>" 0 Ast.AssociativityNone

-- | Field access operator (.)
fieldOp :: Ast.Op
fieldOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "."),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 14),
      Ast.opAssociativity = Ast.AssociativityLeft}

-- | Greater-than-or-equal operator (>=)
geOp :: Ast.Op
geOp = Serialization.op ">=" 5 Ast.AssociativityNone

-- | Greater-than operator (>)
gtOp :: Ast.Op
gtOp = Serialization.op ">" 5 Ast.AssociativityNone

-- | Less-than-or-equal operator (<=)
leOp :: Ast.Op
leOp = Serialization.op "<=" 5 Ast.AssociativityNone

-- | Less-than operator (<)
ltOp :: Ast.Op
ltOp = Serialization.op "<" 5 Ast.AssociativityNone

-- | Method call operator (.)
methodOp :: Ast.Op
methodOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "."),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 14),
      Ast.opAssociativity = Ast.AssociativityLeft}

-- | Mul-assign operator (*=)
mulAssignOp :: Ast.Op
mulAssignOp = Serialization.op "*=" 1 Ast.AssociativityRight

-- | Multiplication operator (*)
mulOp :: Ast.Op
mulOp = Serialization.op "*" 11 Ast.AssociativityLeft

-- | Not-equal operator (!=)
neOp :: Ast.Op
neOp = Serialization.op "!=" 5 Ast.AssociativityNone

-- | Unary negation operator (-)
negOp :: Ast.Op
negOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "-"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 13),
      Ast.opAssociativity = Ast.AssociativityNone}

-- | Unary logical not operator (!)
notOp :: Ast.Op
notOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "!"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 13),
      Ast.opAssociativity = Ast.AssociativityNone}

-- | Logical OR operator (||)
orOp :: Ast.Op
orOp = Serialization.op "||" 3 Ast.AssociativityLeft

-- | Inclusive range operator (..=)
rangeInclusiveOp :: Ast.Op
rangeInclusiveOp = Serialization.op "..=" 2 Ast.AssociativityNone

-- | Range operator (..)
rangeOp :: Ast.Op
rangeOp = Serialization.op ".." 2 Ast.AssociativityNone

-- | Reference operator (&)
refOp :: Ast.Op
refOp =
    Ast.Op {
      Ast.opSymbol = (Ast.Symbol "&"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 13),
      Ast.opAssociativity = Ast.AssociativityNone}

-- | Rem-assign operator (%=)
remAssignOp :: Ast.Op
remAssignOp = Serialization.op "%=" 1 Ast.AssociativityRight

-- | Remainder operator (%)
remOp :: Ast.Op
remOp = Serialization.op "%" 11 Ast.AssociativityLeft

-- | Shift-left assign operator (<<=)
shlAssignOp :: Ast.Op
shlAssignOp = Serialization.op "<<=" 1 Ast.AssociativityRight

-- | Shift-left operator (<<)
shlOp :: Ast.Op
shlOp = Serialization.op "<<" 9 Ast.AssociativityLeft

-- | Shift-right assign operator (>>=)
shrAssignOp :: Ast.Op
shrAssignOp = Serialization.op ">>=" 1 Ast.AssociativityRight

-- | Shift-right operator (>>)
shrOp :: Ast.Op
shrOp = Serialization.op ">>" 9 Ast.AssociativityLeft

-- | Sub-assign operator (-=)
subAssignOp :: Ast.Op
subAssignOp = Serialization.op "-=" 1 Ast.AssociativityRight

-- | Subtraction operator (-)
subOp :: Ast.Op
subOp = Serialization.op "-" 10 Ast.AssociativityLeft
