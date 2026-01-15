-- Note: this is an automatically generated file. Do not edit.

-- | AST operators for Haskell

module Hydra.Ext.Haskell.Operators where

import qualified Hydra.Ast as Ast
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Logical AND operator (&&)
andOp :: Ast.Op
andOp = (Serialization.op "&&" 3 Ast.AssociativityRight)

-- | Applicative apply operator (<*>)
apOp :: Ast.Op
apOp = (Serialization.op "<*>" 4 Ast.AssociativityLeft)

-- | Function application operator (whitespace)
appOp :: Ast.Op
appOp = Ast.Op {
  Ast.opSymbol = (Ast.Symbol ""),
  Ast.opPadding = Ast.Padding {
    Ast.paddingLeft = Ast.WsNone,
    Ast.paddingRight = Ast.WsSpace},
  Ast.opPrecedence = (Ast.Precedence 0),
  Ast.opAssociativity = Ast.AssociativityLeft}

-- | Low-precedence function application ($)
applyOp :: Ast.Op
applyOp = (Serialization.op "$" 0 Ast.AssociativityRight)

-- | Function type arrow (->)
arrowOp :: Ast.Op
arrowOp = (Serialization.op "->" (Math.negate 1) Ast.AssociativityRight)

-- | Type class constraint arrow (=>)
assertOp :: Ast.Op
assertOp = (Serialization.op "=>" 0 Ast.AssociativityNone)

-- | Monadic bind operator (>>=)
bindOp :: Ast.Op
bindOp = (Serialization.op ">>=" 1 Ast.AssociativityLeft)

-- | Case alternative arrow (->)
caseOp :: Ast.Op
caseOp = (Serialization.op "->" 0 Ast.AssociativityNone)

-- | Function composition (.)
composeOp :: Ast.Op
composeOp = (Serialization.op "." 9 Ast.AssociativityLeft)

-- | List concatenation (++)
concatOp :: Ast.Op
concatOp = (Serialization.op "++" 5 Ast.AssociativityRight)

-- | List cons (:)
consOp :: Ast.Op
consOp = (Serialization.op ":" 5 Ast.AssociativityRight)

-- | Definition operator (=)
defineOp :: Ast.Op
defineOp = (Serialization.op "=" 0 Ast.AssociativityNone)

-- | Semigroup append (<>)
diamondOp :: Ast.Op
diamondOp = (Serialization.op "<>" 6 Ast.AssociativityRight)

-- | Integer division (`div`)
divOp :: Ast.Op
divOp = (Serialization.op "`div`" 7 Ast.AssociativityLeft)

-- | Fractional division (/)
divideOp :: Ast.Op
divideOp = (Serialization.op "/" 7 Ast.AssociativityLeft)

-- | List membership (`elem`)
elemOp :: Ast.Op
elemOp = (Serialization.op "`elem`" 4 Ast.AssociativityNone)

-- | Equality comparison (==)
equalOp :: Ast.Op
equalOp = (Serialization.op "==" 4 Ast.AssociativityNone)

-- | Functor map (<$>)
fmapOp :: Ast.Op
fmapOp = (Serialization.op "<$>" 4 Ast.AssociativityLeft)

-- | Greater than (>)
gtOp :: Ast.Op
gtOp = (Serialization.op ">" 4 Ast.AssociativityNone)

-- | Greater than or equal (>=)
gteOp :: Ast.Op
gteOp = (Serialization.op ">=" 4 Ast.AssociativityNone)

-- | List indexing (!!)
indexOp :: Ast.Op
indexOp = (Serialization.op "!!" 9 Ast.AssociativityLeft)

-- | Lambda body arrow (->)
lambdaOp :: Ast.Op
lambdaOp = (Serialization.op "->" (Math.negate 1) Ast.AssociativityRight)

-- | Less than (<)
ltOp :: Ast.Op
ltOp = (Serialization.op "<" 4 Ast.AssociativityNone)

-- | Less than or equal (<=)
lteOp :: Ast.Op
lteOp = (Serialization.op ">=" 4 Ast.AssociativityNone)

-- | Subtraction (-). Originally: associativityLeft
minusOp :: Ast.Op
minusOp = (Serialization.op "-" 6 Ast.AssociativityBoth)

-- | Modulo (`mod`)
modOp :: Ast.Op
modOp = (Serialization.op "`mod`" 7 Ast.AssociativityLeft)

-- | Multiplication (*). Originally: associativityLeft
multOp :: Ast.Op
multOp = (Serialization.op "*" 7 Ast.AssociativityBoth)

-- | Not equal (/=)
neqOp :: Ast.Op
neqOp = (Serialization.op "/=" 4 Ast.AssociativityNone)

-- | List non-membership (`notElem`)
notElemOp :: Ast.Op
notElemOp = (Serialization.op "`notElem`" 4 Ast.AssociativityNone)

-- | Logical OR (||)
orOp :: Ast.Op
orOp = (Serialization.op "||" 2 Ast.AssociativityRight)

-- | Addition (+). Originally: associativityLeft
plusOp :: Ast.Op
plusOp = (Serialization.op "+" 6 Ast.AssociativityBoth)

-- | Integer quotient (`quot`)
quotOp :: Ast.Op
quotOp = (Serialization.op "`quot`" 7 Ast.AssociativityLeft)

-- | Integer remainder (`rem`)
remOp :: Ast.Op
remOp = (Serialization.op "`rem`" 7 Ast.AssociativityLeft)

-- | Type annotation (::)
typeOp :: Ast.Op
typeOp = (Serialization.op "::" 0 Ast.AssociativityNone)
