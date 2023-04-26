module Hydra.Langs.Haskell.Operators where

import Hydra.Ast
import Hydra.Tools.Serialization


andOp :: Op
andOp = op "&&" 3 AssociativityRight

apOp :: Op
apOp = op "<*>" 4 AssociativityLeft

appOp :: Op
appOp = Op (Symbol "") (Padding WsNone WsSpace) (Precedence 0) AssociativityLeft -- No source

applyOp :: Op
applyOp = op "$" 0 AssociativityRight

arrowOp :: Op
arrowOp = op "->" (negate 1) AssociativityRight

--assignOp = op "<-"

bindOp :: Op
bindOp = op ">>=" 1 AssociativityLeft

caseOp :: Op
caseOp = op "->" 0 AssociativityNone -- No source

composeOp :: Op
composeOp = op "." 9 AssociativityLeft

concatOp :: Op
concatOp = op "++" 5 AssociativityRight

consOp :: Op
consOp = op ":" 5 AssociativityRight

defineOp :: Op
defineOp = op "=" 0 AssociativityNone -- No source

diamondOp :: Op
diamondOp = op "<>" 6 AssociativityRight

divOp :: Op
divOp = op "`div`" 7 AssociativityLeft

divideOp :: Op
divideOp = op "/" 7 AssociativityLeft

elemOp :: Op
elemOp = op "`elem`" 4 AssociativityNone

equalOp :: Op
equalOp = op "==" 4 AssociativityNone

fmapOp :: Op
fmapOp = op "<$>" 4 AssociativityLeft

gtOp :: Op
gtOp = op ">" 4 AssociativityNone

gteOp :: Op
gteOp = op ">=" 4 AssociativityNone

indexOp :: Op
indexOp = op "!!" 9 AssociativityLeft

lambdaOp :: Op
lambdaOp = op "->" (negate 1) AssociativityRight -- No source

ltOp :: Op
ltOp = op "<" 4 AssociativityNone

lteOp :: Op
lteOp = op ">=" 4 AssociativityNone

minusOp :: Op
minusOp = op "-" 6 AssociativityBoth -- Originally: AssociativityLeft

modOp :: Op
modOp = op "`mod`" 7 AssociativityLeft

multOp :: Op
multOp = op "*" 7 AssociativityBoth -- Originally: AssociativityLeft

neqOp :: Op
neqOp = op "/=" 4 AssociativityNone

notElemOp :: Op
notElemOp = op "`notElem`" 4 AssociativityNone

orOp :: Op
orOp = op "||" 2 AssociativityRight

plusOp :: Op
plusOp = op "+" 6 AssociativityBoth -- Originally: AssociativityLeft

quotOp :: Op
quotOp = op "`quot`" 7 AssociativityLeft

remOp :: Op
remOp = op "`rem`" 7 AssociativityLeft

--suchThatOp = op "|"

--thenOp = op "=>"

typeOp :: Op
typeOp = op "::" 0 AssociativityNone -- No source
