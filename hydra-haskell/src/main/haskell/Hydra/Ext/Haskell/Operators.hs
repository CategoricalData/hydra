module Hydra.Ext.Haskell.Operators where

import qualified Hydra.Ast as Ast
import qualified Hydra.Serialization as Serialization


andOp = Serialization.op "&&" 3 Ast.AssociativityRight :: Ast.Op
apOp = Serialization.op "<*>" 4 Ast.AssociativityLeft :: Ast.Op
appOp = Ast.Op (Ast.Symbol "") (Ast.Padding Ast.WsNone Ast.WsSpace) (Ast.Precedence 0) Ast.AssociativityLeft :: Ast.Op -- No source
applyOp = Serialization.op "$" 0 Ast.AssociativityRight :: Ast.Op
arrowOp = Serialization.op "->" (negate 1) Ast.AssociativityRight :: Ast.Op
assertOp = Serialization.op "=>" 0 Ast.AssociativityNone :: Ast.Op -- No source
--assignOp = Serialization.op "<-"
bindOp = Serialization.op ">>=" 1 Ast.AssociativityLeft :: Ast.Op
caseOp = Serialization.op "->" 0 Ast.AssociativityNone :: Ast.Op -- No source
composeOp = Serialization.op "." 9 Ast.AssociativityLeft :: Ast.Op
concatOp = Serialization.op "++" 5 Ast.AssociativityRight :: Ast.Op
consOp = Serialization.op ":" 5 Ast.AssociativityRight :: Ast.Op
defineOp = Serialization.op "=" 0 Ast.AssociativityNone :: Ast.Op -- No source
diamondOp = Serialization.op "<>" 6 Ast.AssociativityRight :: Ast.Op
divOp = Serialization.op "`div`" 7 Ast.AssociativityLeft :: Ast.Op
divideOp = Serialization.op "/" 7 Ast.AssociativityLeft :: Ast.Op
elemOp = Serialization.op "`elem`" 4 Ast.AssociativityNone :: Ast.Op
equalOp = Serialization.op "==" 4 Ast.AssociativityNone :: Ast.Op
fmapOp = Serialization.op "<$>" 4 Ast.AssociativityLeft :: Ast.Op
gtOp = Serialization.op ">" 4 Ast.AssociativityNone :: Ast.Op
gteOp = Serialization.op ">=" 4 Ast.AssociativityNone :: Ast.Op
indexOp = Serialization.op "!!" 9 Ast.AssociativityLeft :: Ast.Op
lambdaOp = Serialization.op "->" (negate 1) Ast.AssociativityRight :: Ast.Op -- No source
ltOp = Serialization.op "<" 4 Ast.AssociativityNone :: Ast.Op
lteOp = Serialization.op ">=" 4 Ast.AssociativityNone :: Ast.Op
minusOp = Serialization.op "-" 6 Ast.AssociativityBoth :: Ast.Op -- Originally: AssociativityLeft
modOp = Serialization.op "`mod`" 7 Ast.AssociativityLeft :: Ast.Op
multOp = Serialization.op "*" 7 Ast.AssociativityBoth :: Ast.Op -- Originally: AssociativityLeft
neqOp = Serialization.op "/=" 4 Ast.AssociativityNone :: Ast.Op
notElemOp = Serialization.op "`notElem`" 4 Ast.AssociativityNone :: Ast.Op
orOp = Serialization.op "||" 2 Ast.AssociativityRight :: Ast.Op
plusOp = Serialization.op "+" 6 Ast.AssociativityBoth :: Ast.Op -- Originally: AssociativityLeft
quotOp = Serialization.op "`quot`" 7 Ast.AssociativityLeft :: Ast.Op
remOp = Serialization.op "`rem`" 7 Ast.AssociativityLeft :: Ast.Op
--suchThatOp = Serialization.op "|"
typeOp = Serialization.op "::" 0 Ast.AssociativityNone :: Ast.Op -- No source
