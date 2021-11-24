-- | Defines a Haskell-like language ("HydraScript") for pretty printing Hydra terms and types
-- Haskell operator precendence and associativity are drawn from:
--   https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- Other operators were investigated using GHCi, e.g. ":info (->)"
-- Operator names are drawn (loosely) from:
--   https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Util.Codetree.Script where

import Hydra.Util.Codetree.Ast
import Hydra.Util.Codetree.Print


andOp :: Op
andOp = op "&&" 3 AssociativityRight

apOp :: Op
apOp = op "<*>" 4 AssociativityLeft

appOp :: Op
appOp = Op "" (Padding WsNone WsSpace) 0 AssociativityLeft -- No source

applyOp :: Op
applyOp = op "$" 0 AssociativityRight

arrowOp :: Op
arrowOp = op "->" (negate 1) AssociativityRight

--assignOp = op "<-"

bindOp :: Op
bindOp = op ">>=" 1 AssociativityLeft

caseOp :: Op
caseOp = op "->" 0 AssociativityNone -- No source

commaOp :: Op
commaOp = Op "," (Padding WsNone WsSpace) 0 AssociativityNone -- No source

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
lambdaOp = op "->" 0 AssociativityNone -- No source

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

--typeOp = op "::"

----------------------------------------

ifx :: Op -> Expr -> Expr -> Expr
ifx op lhs rhs = ExprOp $ OpExpr op lhs rhs

caseStatement :: Expr -> [(Expr, Expr)] -> Expr
caseStatement cond cases = ifx ofOp lhs rhs
  where
    lhs = spaceSep [cst "case", cond]
    rhs = newlineSep (uncurry (ifx caseOp) <$> cases)
    ofOp = Op "of" (Padding WsSpace WsBreakIndent) 0 AssociativityNone

commaSep :: [Expr] -> Expr
commaSep l = case l of
  [x] -> x
  (h:r) -> ifx commaOp h $ commaSep r
        
cst :: String -> Expr
cst = ExprConst

indentBlock :: Expr -> [Expr] -> Expr
indentBlock head els = ifx idtOp head $ newlineSep els 
  where
    idtOp = Op "" (Padding WsSpace WsBreakIndent) 0 AssociativityNone

htuple :: [Expr] -> Expr
htuple els = case els of
  [] -> cst "()"
  _ -> brackets parentheses $ commaSep els 

indentLines :: [Expr] -> Expr
indentLines els = ifx topOp (cst "") (newlineSep els)
  where
    topOp = Op "" (Padding WsNone WsBreakIndent) 0 AssociativityNone
  
lam :: [Symbol] -> Expr -> Expr
lam vars = ifx lambdaOp $ cst $ "\\" ++ unwords vars

hlist :: [Expr] -> Expr
hlist els = case els of
  [] -> cst "[]"
  _ -> brackets squareBrackets $ commaSep els 

newlineSep :: [Expr] -> Expr
newlineSep = sep $ Op "" (Padding WsNone WsBreak) 0 AssociativityNone

num :: Int -> Expr
num = cst . show

op :: Symbol -> Precedence -> Associativity -> Op
op s = Op s (Padding WsSpace WsSpace)

prefix :: String -> Expr -> Expr
prefix p = ifx preOp (cst "")
  where
    preOp = Op p (Padding WsNone WsNone) 0 AssociativityNone
    
sep :: Op -> [Expr] -> Expr
sep op els =  case els of
  [x] -> x
  (h:r) -> ifx op h $ sep op r
                 
spaceSep :: [Expr] -> Expr
spaceSep = sep $ Op "" (Padding WsSpace WsNone) 0 AssociativityNone
