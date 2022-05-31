module Hydra.Ext.Java.Operators where

import Hydra.Util.Codetree.Ast
import Hydra.Util.Codetree.Script


-- andOp > orOp
andOp :: Op
andOp = op "&&" 2 AssociativityLeft

arrowOp :: Op
arrowOp = op "->" (negate 1) AssociativityRight

-- bitAndOp > bitXorOp 
bitAndOp :: Op
bitAndOp = op "^" 5 AssociativityLeft

-- bitOrOp > andOp 
bitOrOp :: Op
bitOrOp = op "|" 3 AssociativityLeft

-- bitXorOp > bitOrOp 
bitXorOp :: Op
bitXorOp = op "^" 4 AssociativityLeft

defineOp :: Op
defineOp = op "=" 0 AssociativityNone -- No source

-- timesOp == divideOp == modOp > plusOp
divideOp :: Op
divideOp = op "/" 9 AssociativityLeft

-- equalOp == notEqualOp > bitAndOp
equalOp :: Op
equalOp = op "==" 6 AssociativityLeft

-- minusOp > shiftLeftOp
minusOp :: Op
minusOp = op "-" 8 AssociativityLeft

-- timesOp == divideOp == modOp > plusOp
modOp :: Op
modOp = op "%" 9 AssociativityLeft

-- equalOp == notEqualOp > bitAndOp
notEqualOp :: Op
notEqualOp = op "==" 6 AssociativityLeft

-- orOp > arrowOp
orOp :: Op
orOp = op "||" 1 AssociativityLeft

-- plusOp > shiftLeftOp
plusOp :: Op
plusOp = op "+" 8 AssociativityLeft

-- shiftLeftOp == shiftRightOp == shiftRightZeroFillOp > equalOp
shiftLeftOp :: Op
shiftLeftOp = op "<<" 7 AssociativityLeft

-- shiftLeftOp == shiftRightOp == shiftRightZeroFillOp > equalOp
shiftRightOp :: Op
shiftRightOp = op ">>" 7 AssociativityLeft

-- shiftLeftOp == shiftRightOp == shiftRightZeroFillOp > equalOp
shiftRightZeroFillOp :: Op
shiftRightZeroFillOp = op ">>>" 7 AssociativityLeft

-- timesOp == divideOp == modOp > plusOp
timesOp :: Op
timesOp = op "*" 9 AssociativityLeft
