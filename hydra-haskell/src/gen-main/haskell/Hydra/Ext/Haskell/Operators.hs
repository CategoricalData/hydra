-- | AST operators for Haskell

module Hydra.Ext.Haskell.Operators where

import qualified Hydra.Ast as Ast
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

andOp :: Ast.Op
andOp = (Serialization.op "&&" 3 Ast.AssociativityRight)

apOp :: Ast.Op
apOp = (Serialization.op "<*>" 4 Ast.AssociativityLeft)

-- | No source
appOp :: Ast.Op
appOp = Ast.Op {
  Ast.opSymbol = (Ast.Symbol ""),
  Ast.opPadding = Ast.Padding {
    Ast.paddingLeft = Ast.WsNone,
    Ast.paddingRight = Ast.WsSpace},
  Ast.opPrecedence = (Ast.Precedence 0),
  Ast.opAssociativity = Ast.AssociativityLeft}

applyOp :: Ast.Op
applyOp = (Serialization.op "$" 0 Ast.AssociativityRight)

arrowOp :: Ast.Op
arrowOp = (Serialization.op "->" (Math.negate 1) Ast.AssociativityRight)

-- | No source
assertOp :: Ast.Op
assertOp = (Serialization.op "=>" 0 Ast.AssociativityNone)

bindOp :: Ast.Op
bindOp = (Serialization.op ">>=" 1 Ast.AssociativityLeft)

-- | No source
caseOp :: Ast.Op
caseOp = (Serialization.op "->" 0 Ast.AssociativityNone)

composeOp :: Ast.Op
composeOp = (Serialization.op "." 9 Ast.AssociativityLeft)

concatOp :: Ast.Op
concatOp = (Serialization.op "++" 5 Ast.AssociativityRight)

consOp :: Ast.Op
consOp = (Serialization.op ":" 5 Ast.AssociativityRight)

-- | No source
defineOp :: Ast.Op
defineOp = (Serialization.op "=" 0 Ast.AssociativityNone)

diamondOp :: Ast.Op
diamondOp = (Serialization.op "<>" 6 Ast.AssociativityRight)

divOp :: Ast.Op
divOp = (Serialization.op "`div`" 7 Ast.AssociativityLeft)

divideOp :: Ast.Op
divideOp = (Serialization.op "/" 7 Ast.AssociativityLeft)

elemOp :: Ast.Op
elemOp = (Serialization.op "`elem`" 4 Ast.AssociativityNone)

equalOp :: Ast.Op
equalOp = (Serialization.op "==" 4 Ast.AssociativityNone)

fmapOp :: Ast.Op
fmapOp = (Serialization.op "<$>" 4 Ast.AssociativityLeft)

gtOp :: Ast.Op
gtOp = (Serialization.op ">" 4 Ast.AssociativityNone)

gteOp :: Ast.Op
gteOp = (Serialization.op ">=" 4 Ast.AssociativityNone)

indexOp :: Ast.Op
indexOp = (Serialization.op "!!" 9 Ast.AssociativityLeft)

-- | No source
lambdaOp :: Ast.Op
lambdaOp = (Serialization.op "->" (Math.negate 1) Ast.AssociativityRight)

ltOp :: Ast.Op
ltOp = (Serialization.op "<" 4 Ast.AssociativityNone)

lteOp :: Ast.Op
lteOp = (Serialization.op ">=" 4 Ast.AssociativityNone)

-- | Originally: associativityLeft
minusOp :: Ast.Op
minusOp = (Serialization.op "-" 6 Ast.AssociativityBoth)

modOp :: Ast.Op
modOp = (Serialization.op "`mod`" 7 Ast.AssociativityLeft)

-- | Originally: associativityLeft
multOp :: Ast.Op
multOp = (Serialization.op "*" 7 Ast.AssociativityBoth)

neqOp :: Ast.Op
neqOp = (Serialization.op "/=" 4 Ast.AssociativityNone)

notElemOp :: Ast.Op
notElemOp = (Serialization.op "`notElem`" 4 Ast.AssociativityNone)

orOp :: Ast.Op
orOp = (Serialization.op "||" 2 Ast.AssociativityRight)

-- | Originally: associativityLeft
plusOp :: Ast.Op
plusOp = (Serialization.op "+" 6 Ast.AssociativityBoth)

quotOp :: Ast.Op
quotOp = (Serialization.op "`quot`" 7 Ast.AssociativityLeft)

remOp :: Ast.Op
remOp = (Serialization.op "`rem`" 7 Ast.AssociativityLeft)

-- | No source
typeOp :: Ast.Op
typeOp = (Serialization.op "::" 0 Ast.AssociativityNone)
