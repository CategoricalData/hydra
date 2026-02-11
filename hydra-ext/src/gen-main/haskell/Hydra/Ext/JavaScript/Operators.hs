-- Note: this is an automatically generated file. Do not edit.

-- | AST operators for JavaScript with precedence and associativity

module Hydra.Ext.JavaScript.Operators where

import qualified Hydra.Ast as Ast
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Function application (whitespace)
appOp :: Ast.Op
appOp = Ast.Op {
  Ast.opSymbol = (Ast.Symbol ""),
  Ast.opPadding = Ast.Padding {
    Ast.paddingLeft = Ast.WsNone,
    Ast.paddingRight = Ast.WsNone},
  Ast.opPrecedence = (Ast.Precedence 20),
  Ast.opAssociativity = Ast.AssociativityLeft}

-- | Member access operator (.)
memberOp :: Ast.Op
memberOp = Ast.Op {
  Ast.opSymbol = (Ast.Symbol "."),
  Ast.opPadding = Ast.Padding {
    Ast.paddingLeft = Ast.WsNone,
    Ast.paddingRight = Ast.WsNone},
  Ast.opPrecedence = (Ast.Precedence 20),
  Ast.opAssociativity = Ast.AssociativityLeft}

-- | Optional chaining operator (?.)
optionalChainOp :: Ast.Op
optionalChainOp = Ast.Op {
  Ast.opSymbol = (Ast.Symbol "?."),
  Ast.opPadding = Ast.Padding {
    Ast.paddingLeft = Ast.WsNone,
    Ast.paddingRight = Ast.WsNone},
  Ast.opPrecedence = (Ast.Precedence 20),
  Ast.opAssociativity = Ast.AssociativityLeft}

-- | Exponentiation operator (**)
exponentiateOp :: Ast.Op
exponentiateOp = (Serialization.op "**" 16 Ast.AssociativityRight)

-- | Multiplication operator (*)
multiplyOp :: Ast.Op
multiplyOp = (Serialization.op "*" 15 Ast.AssociativityLeft)

-- | Division operator (/)
divideOp :: Ast.Op
divideOp = (Serialization.op "/" 15 Ast.AssociativityLeft)

-- | Modulo operator (%)
moduloOp :: Ast.Op
moduloOp = (Serialization.op "%" 15 Ast.AssociativityLeft)

-- | Addition operator (+)
addOp :: Ast.Op
addOp = (Serialization.op "+" 14 Ast.AssociativityLeft)

-- | Subtraction operator (-)
subtractOp :: Ast.Op
subtractOp = (Serialization.op "-" 14 Ast.AssociativityLeft)

-- | Less than operator (<)
lessThanOp :: Ast.Op
lessThanOp = (Serialization.op "<" 12 Ast.AssociativityLeft)

-- | Less than or equal operator (<=)
lessThanOrEqualOp :: Ast.Op
lessThanOrEqualOp = (Serialization.op "<=" 12 Ast.AssociativityLeft)

-- | Greater than operator (>)
greaterThanOp :: Ast.Op
greaterThanOp = (Serialization.op ">" 12 Ast.AssociativityLeft)

-- | Greater than or equal operator (>=)
greaterThanOrEqualOp :: Ast.Op
greaterThanOrEqualOp = (Serialization.op ">=" 12 Ast.AssociativityLeft)

-- | In operator (in)
inOp :: Ast.Op
inOp = (Serialization.op "in" 12 Ast.AssociativityLeft)

-- | Instance of operator (instanceof)
instanceOfOp :: Ast.Op
instanceOfOp = (Serialization.op "instanceof" 12 Ast.AssociativityLeft)

-- | Equality operator (==)
equalOp :: Ast.Op
equalOp = (Serialization.op "==" 11 Ast.AssociativityLeft)

-- | Inequality operator (!=)
notEqualOp :: Ast.Op
notEqualOp = (Serialization.op "!=" 11 Ast.AssociativityLeft)

-- | Strict equality operator (===)
strictEqualOp :: Ast.Op
strictEqualOp = (Serialization.op "===" 11 Ast.AssociativityLeft)

-- | Strict inequality operator (!==)
strictNotEqualOp :: Ast.Op
strictNotEqualOp = (Serialization.op "!==" 11 Ast.AssociativityLeft)

-- | Bitwise AND operator (&)
bitwiseAndOp :: Ast.Op
bitwiseAndOp = (Serialization.op "&" 10 Ast.AssociativityLeft)

-- | Bitwise XOR operator (^)
bitwiseXorOp :: Ast.Op
bitwiseXorOp = (Serialization.op "^" 9 Ast.AssociativityLeft)

-- | Bitwise OR operator (|)
bitwiseOrOp :: Ast.Op
bitwiseOrOp = (Serialization.op "|" 8 Ast.AssociativityLeft)

-- | Left shift operator (<<)
leftShiftOp :: Ast.Op
leftShiftOp = (Serialization.op "<<" 13 Ast.AssociativityLeft)

-- | Right shift operator (>>)
rightShiftOp :: Ast.Op
rightShiftOp = (Serialization.op ">>" 13 Ast.AssociativityLeft)

-- | Unsigned right shift operator (>>>)
unsignedRightShiftOp :: Ast.Op
unsignedRightShiftOp = (Serialization.op ">>>" 13 Ast.AssociativityLeft)

-- | Logical AND operator (&&)
logicalAndOp :: Ast.Op
logicalAndOp = (Serialization.op "&&" 6 Ast.AssociativityLeft)

-- | Logical OR operator (||)
logicalOrOp :: Ast.Op
logicalOrOp = (Serialization.op "||" 5 Ast.AssociativityLeft)

-- | Nullish coalescing operator (??)
nullishCoalescingOp :: Ast.Op
nullishCoalescingOp = (Serialization.op "??" 4 Ast.AssociativityLeft)

-- | Assignment operator (=)
assignOp :: Ast.Op
assignOp = (Serialization.op "=" 2 Ast.AssociativityRight)

-- | Arrow function operator (=>)
arrowOp :: Ast.Op
arrowOp = (Serialization.op "=>" 2 Ast.AssociativityRight)

-- | Ternary operator (?:) - represents the ? part
ternaryOp :: Ast.Op
ternaryOp = (Serialization.op "?" 3 Ast.AssociativityRight)

-- | Comma operator (,)
commaOp :: Ast.Op
commaOp = (Serialization.op "," 1 Ast.AssociativityLeft)

-- | Definition operator (= in const x = ...)
defineOp :: Ast.Op
defineOp = (Serialization.op "=" 0 Ast.AssociativityNone)

-- | Type annotation colon (:)
colonOp :: Ast.Op
colonOp = (Serialization.op ":" 0 Ast.AssociativityNone)
