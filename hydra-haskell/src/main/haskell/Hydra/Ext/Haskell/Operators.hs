module Hydra.Ext.Haskell.Operators where

import Hydra.Ast
import Hydra.Tools.Serialization


andOp = op "&&" 3 AssociativityRight :: Op
apOp = op "<*>" 4 AssociativityLeft :: Op
appOp = Op (Symbol "") (Padding WsNone WsSpace) (Precedence 0) AssociativityLeft :: Op -- No source
applyOp = op "$" 0 AssociativityRight :: Op
arrowOp = op "->" (negate 1) AssociativityRight :: Op
assertOp = op "=>" 0 AssociativityNone :: Op -- No source
--assignOp = op "<-"
bindOp = op ">>=" 1 AssociativityLeft :: Op
caseOp = op "->" 0 AssociativityNone :: Op -- No source
composeOp = op "." 9 AssociativityLeft :: Op
concatOp = op "++" 5 AssociativityRight :: Op
consOp = op ":" 5 AssociativityRight :: Op
defineOp = op "=" 0 AssociativityNone :: Op -- No source
diamondOp = op "<>" 6 AssociativityRight :: Op
divOp = op "`div`" 7 AssociativityLeft :: Op
divideOp = op "/" 7 AssociativityLeft :: Op
elemOp = op "`elem`" 4 AssociativityNone :: Op
equalOp = op "==" 4 AssociativityNone :: Op
fmapOp = op "<$>" 4 AssociativityLeft :: Op
gtOp = op ">" 4 AssociativityNone :: Op
gteOp = op ">=" 4 AssociativityNone :: Op
indexOp = op "!!" 9 AssociativityLeft :: Op
lambdaOp = op "->" (negate 1) AssociativityRight :: Op -- No source
ltOp = op "<" 4 AssociativityNone :: Op
lteOp = op ">=" 4 AssociativityNone :: Op
minusOp = op "-" 6 AssociativityBoth :: Op -- Originally: AssociativityLeft
modOp = op "`mod`" 7 AssociativityLeft :: Op
multOp = op "*" 7 AssociativityBoth :: Op -- Originally: AssociativityLeft
neqOp = op "/=" 4 AssociativityNone :: Op
notElemOp = op "`notElem`" 4 AssociativityNone :: Op
orOp = op "||" 2 AssociativityRight :: Op
plusOp = op "+" 6 AssociativityBoth :: Op -- Originally: AssociativityLeft
quotOp = op "`quot`" 7 AssociativityLeft :: Op
remOp = op "`rem`" 7 AssociativityLeft :: Op
--suchThatOp = op "|"
typeOp = op "::" 0 AssociativityNone :: Op -- No source
