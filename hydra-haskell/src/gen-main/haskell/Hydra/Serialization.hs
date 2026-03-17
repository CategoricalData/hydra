-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.

module Hydra.Serialization where

import qualified Hydra.Ast as Ast
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

angleBraces :: Ast.Brackets
angleBraces =
    Ast.Brackets {
      Ast.bracketsOpen = (Ast.Symbol "<"),
      Ast.bracketsClose = (Ast.Symbol ">")}

angleBracesList :: Ast.BlockStyle -> [Ast.Expr] -> Ast.Expr
angleBracesList style els = Logic.ifElse (Lists.null els) (cst "<>") (brackets angleBraces style (commaSep style els))

-- | Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression.
bracesListAdaptive :: [Ast.Expr] -> Ast.Expr
bracesListAdaptive els =

      let inlineList = curlyBracesList Nothing inlineStyle els
      in (Logic.ifElse (Equality.gt (expressionLength inlineList) 70) (curlyBracesList Nothing halfBlockStyle els) inlineList)

bracketList :: Ast.BlockStyle -> [Ast.Expr] -> Ast.Expr
bracketList style els = Logic.ifElse (Lists.null els) (cst "[]") (brackets squareBrackets style (commaSep style els))

-- | Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression.
bracketListAdaptive :: [Ast.Expr] -> Ast.Expr
bracketListAdaptive els =

      let inlineList = bracketList inlineStyle els
      in (Logic.ifElse (Equality.gt (expressionLength inlineList) 70) (bracketList halfBlockStyle els) inlineList)

brackets :: Ast.Brackets -> Ast.BlockStyle -> Ast.Expr -> Ast.Expr
brackets br style e =
    Ast.ExprBrackets (Ast.BracketExpr {
      Ast.bracketExprBrackets = br,
      Ast.bracketExprEnclosed = e,
      Ast.bracketExprStyle = style})

commaSep :: Ast.BlockStyle -> [Ast.Expr] -> Ast.Expr
commaSep = symbolSep ","

curlyBlock :: Ast.BlockStyle -> Ast.Expr -> Ast.Expr
curlyBlock style e = curlyBracesList Nothing style [
  e]

curlyBraces :: Ast.Brackets
curlyBraces =
    Ast.Brackets {
      Ast.bracketsOpen = (Ast.Symbol "{"),
      Ast.bracketsClose = (Ast.Symbol "}")}

curlyBracesList :: Maybe String -> Ast.BlockStyle -> [Ast.Expr] -> Ast.Expr
curlyBracesList msymb style els =
    Logic.ifElse (Lists.null els) (cst "{}") (brackets curlyBraces style (symbolSep (Maybes.fromMaybe "," msymb) style els))

cst :: String -> Ast.Expr
cst s = Ast.ExprConst (sym s)

customIndent :: String -> String -> String
customIndent idt s = Strings.cat (Lists.intersperse "\n" (Lists.map (\line -> Strings.cat2 idt line) (Strings.lines s)))

customIndentBlock :: String -> [Ast.Expr] -> Ast.Expr
customIndentBlock idt els =

      let idtOp =
              Ast.Op {
                Ast.opSymbol = (sym ""),
                Ast.opPadding = Ast.Padding {
                  Ast.paddingLeft = Ast.WsSpace,
                  Ast.paddingRight = (Ast.WsBreakAndIndent idt)},
                Ast.opPrecedence = (Ast.Precedence 0),
                Ast.opAssociativity = Ast.AssociativityNone}
      in (Maybes.maybe (cst "") (\head -> Logic.ifElse (Equality.equal (Lists.length els) 1) head (ifx idtOp head (newlineSep (Lists.drop 1 els)))) (Lists.safeHead els))

dotSep :: [Ast.Expr] -> Ast.Expr
dotSep =
    sep (Ast.Op {
      Ast.opSymbol = (sym "."),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone})

doubleNewlineSep :: [Ast.Expr] -> Ast.Expr
doubleNewlineSep =
    sep (Ast.Op {
      Ast.opSymbol = (sym ""),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsBreak,
        Ast.paddingRight = Ast.WsBreak},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone})

doubleSpace :: String
doubleSpace = "  "

-- | Find the approximate length (number of characters, including spaces and newlines) of an expression without actually printing it.
expressionLength :: Ast.Expr -> Int
expressionLength e =

      let symbolLength = \s -> Strings.length (Ast.unSymbol s)
          wsLength =
                  \ws -> case ws of
                    Ast.WsNone -> 0
                    Ast.WsSpace -> 1
                    Ast.WsBreak -> 10000
                    Ast.WsBreakAndIndent _ -> 10000
                    Ast.WsDoubleBreak -> 10000
          blockStyleLength =
                  \style ->
                    let mindentLen = Maybes.maybe 0 Strings.length (Ast.blockStyleIndent style)
                        nlBeforeLen = Logic.ifElse (Ast.blockStyleNewlineBeforeContent style) 1 0
                        nlAfterLen = Logic.ifElse (Ast.blockStyleNewlineAfterContent style) 1 0
                    in (Math.add mindentLen (Math.add nlBeforeLen nlAfterLen))
          bracketsLength =
                  \brackets -> Math.add (symbolLength (Ast.bracketsOpen brackets)) (symbolLength (Ast.bracketsClose brackets))
          bracketExprLength =
                  \be -> Math.add (bracketsLength (Ast.bracketExprBrackets be)) (Math.add (expressionLength (Ast.bracketExprEnclosed be)) (blockStyleLength (Ast.bracketExprStyle be)))
          indentedExpressionLength =
                  \ie ->
                    let baseLen = expressionLength (Ast.indentedExpressionExpr ie)
                        indentLen =
                                case (Ast.indentedExpressionStyle ie) of
                                  Ast.IndentStyleAllLines v0 -> Strings.length v0
                                  Ast.IndentStyleSubsequentLines v0 -> Strings.length v0
                    in (Math.add baseLen indentLen)
          opLength =
                  \op ->
                    let symLen = symbolLength (Ast.opSymbol op)
                        padding = Ast.opPadding op
                        leftLen = wsLength (Ast.paddingLeft padding)
                        rightLen = wsLength (Ast.paddingRight padding)
                    in (Math.add symLen (Math.add leftLen rightLen))
          opExprLength =
                  \oe ->
                    let opLen = opLength (Ast.opExprOp oe)
                        leftLen = expressionLength (Ast.opExprLhs oe)
                        rightLen = expressionLength (Ast.opExprRhs oe)
                    in (Math.add opLen (Math.add leftLen rightLen))
          seqExprLength =
                  \se ->
                    let sopLen = opLength (Ast.seqExprOp se)
                        elementLens = Lists.map expressionLength (Ast.seqExprElements se)
                        totalElLen = Lists.foldl Math.add 0 elementLens
                        numSeps = Math.sub (Lists.length (Ast.seqExprElements se)) 1
                    in (Math.add totalElLen (Math.mul sopLen (Logic.ifElse (Equality.gt numSeps 0) numSeps 0)))
      in case e of
        Ast.ExprConst v0 -> symbolLength v0
        Ast.ExprIndent v0 -> indentedExpressionLength v0
        Ast.ExprOp v0 -> opExprLength v0
        Ast.ExprBrackets v0 -> bracketExprLength v0
        Ast.ExprSeq v0 -> seqExprLength v0

fullBlockStyle :: Ast.BlockStyle
fullBlockStyle =
    Ast.BlockStyle {
      Ast.blockStyleIndent = (Just doubleSpace),
      Ast.blockStyleNewlineBeforeContent = True,
      Ast.blockStyleNewlineAfterContent = True}

halfBlockStyle :: Ast.BlockStyle
halfBlockStyle =
    Ast.BlockStyle {
      Ast.blockStyleIndent = (Just doubleSpace),
      Ast.blockStyleNewlineBeforeContent = True,
      Ast.blockStyleNewlineAfterContent = False}

ifx :: Ast.Op -> Ast.Expr -> Ast.Expr -> Ast.Expr
ifx op lhs rhs =
    Ast.ExprOp (Ast.OpExpr {
      Ast.opExprOp = op,
      Ast.opExprLhs = lhs,
      Ast.opExprRhs = rhs})

indent :: String -> String
indent = customIndent doubleSpace

indentBlock :: [Ast.Expr] -> Ast.Expr
indentBlock = customIndentBlock doubleSpace

indentSubsequentLines :: String -> Ast.Expr -> Ast.Expr
indentSubsequentLines idt e =
    Ast.ExprIndent (Ast.IndentedExpression {
      Ast.indentedExpressionStyle = (Ast.IndentStyleSubsequentLines idt),
      Ast.indentedExpressionExpr = e})

infixWs :: String -> Ast.Expr -> Ast.Expr -> Ast.Expr
infixWs op l r =
    spaceSep [
      l,
      (cst op),
      r]

infixWsList :: String -> [Ast.Expr] -> Ast.Expr
infixWsList op opers =

      let opExpr = cst op
          foldFun = \e -> \r -> Logic.ifElse (Lists.null e) [
                r] (Lists.cons r (Lists.cons opExpr e))
      in (spaceSep (Lists.foldl foldFun [] (Lists.reverse opers)))

inlineStyle :: Ast.BlockStyle
inlineStyle =
    Ast.BlockStyle {
      Ast.blockStyleIndent = Nothing,
      Ast.blockStyleNewlineBeforeContent = False,
      Ast.blockStyleNewlineAfterContent = False}

newlineSep :: [Ast.Expr] -> Ast.Expr
newlineSep =
    sep (Ast.Op {
      Ast.opSymbol = (sym ""),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsBreak},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone})

noPadding :: Ast.Padding
noPadding =
    Ast.Padding {
      Ast.paddingLeft = Ast.WsNone,
      Ast.paddingRight = Ast.WsNone}

noSep :: [Ast.Expr] -> Ast.Expr
noSep =
    sep (Ast.Op {
      Ast.opSymbol = (sym ""),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsNone,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone})

num :: Int -> Ast.Expr
num i = cst (Literals.showInt32 i)

op :: String -> Int -> Ast.Associativity -> Ast.Op
op s p assoc =
    Ast.Op {
      Ast.opSymbol = (sym s),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsSpace,
        Ast.paddingRight = Ast.WsSpace},
      Ast.opPrecedence = (Ast.Precedence p),
      Ast.opAssociativity = assoc}

orOp :: Bool -> Ast.Op
orOp newlines =
    Ast.Op {
      Ast.opSymbol = (sym "|"),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsSpace,
        Ast.paddingRight = (Logic.ifElse newlines Ast.WsBreak Ast.WsSpace)},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone}

orSep :: Ast.BlockStyle -> [Ast.Expr] -> Ast.Expr
orSep style l =

      let newlines = Ast.blockStyleNewlineBeforeContent style
      in (Maybes.maybe (cst "") (\h -> Lists.foldl (\acc -> \el -> ifx (orOp newlines) acc el) h (Lists.drop 1 l)) (Lists.safeHead l))

parenList :: Bool -> [Ast.Expr] -> Ast.Expr
parenList newlines els =

      let style = Logic.ifElse (Logic.and newlines (Equality.gt (Lists.length els) 1)) halfBlockStyle inlineStyle
      in (Logic.ifElse (Lists.null els) (cst "()") (brackets parentheses style (commaSep style els)))

parens :: Ast.Expr -> Ast.Expr
parens = brackets parentheses inlineStyle

parentheses :: Ast.Brackets
parentheses =
    Ast.Brackets {
      Ast.bracketsOpen = (Ast.Symbol "("),
      Ast.bracketsClose = (Ast.Symbol ")")}

parenthesize :: Ast.Expr -> Ast.Expr
parenthesize exp =

      let assocLeft =
              \a -> case a of
                Ast.AssociativityRight -> False
                _ -> True
          assocRight =
                  \a -> case a of
                    Ast.AssociativityLeft -> False
                    _ -> True
      in case exp of
        Ast.ExprBrackets v0 -> Ast.ExprBrackets (Ast.BracketExpr {
          Ast.bracketExprBrackets = (Ast.bracketExprBrackets v0),
          Ast.bracketExprEnclosed = (parenthesize (Ast.bracketExprEnclosed v0)),
          Ast.bracketExprStyle = (Ast.bracketExprStyle v0)})
        Ast.ExprConst _ -> exp
        Ast.ExprIndent v0 -> Ast.ExprIndent (Ast.IndentedExpression {
          Ast.indentedExpressionStyle = (Ast.indentedExpressionStyle v0),
          Ast.indentedExpressionExpr = (parenthesize (Ast.indentedExpressionExpr v0))})
        Ast.ExprSeq v0 -> Ast.ExprSeq (Ast.SeqExpr {
          Ast.seqExprOp = (Ast.seqExprOp v0),
          Ast.seqExprElements = (Lists.map parenthesize (Ast.seqExprElements v0))})
        Ast.ExprOp v0 ->
          let op = Ast.opExprOp v0
              prec = Ast.unPrecedence (Ast.opPrecedence op)
              assoc = Ast.opAssociativity op
              lhs = Ast.opExprLhs v0
              rhs = Ast.opExprRhs v0
              lhs_ = parenthesize lhs
              rhs_ = parenthesize rhs
              lhs2 =
                      case lhs_ of
                        Ast.ExprOp v1 ->
                          let lop = Ast.opExprOp v1
                              lprec = Ast.unPrecedence (Ast.opPrecedence lop)
                              lassoc = Ast.opAssociativity lop
                              comparison = Equality.compare prec lprec
                          in case comparison of
                            Util.ComparisonLessThan -> lhs_
                            Util.ComparisonGreaterThan -> parens lhs_
                            Util.ComparisonEqualTo -> Logic.ifElse (Logic.and (assocLeft assoc) (assocLeft lassoc)) lhs_ (parens lhs_)
                        _ -> lhs_
              rhs2 =
                      case rhs_ of
                        Ast.ExprOp v1 ->
                          let rop = Ast.opExprOp v1
                              rprec = Ast.unPrecedence (Ast.opPrecedence rop)
                              rassoc = Ast.opAssociativity rop
                              comparison = Equality.compare prec rprec
                          in case comparison of
                            Util.ComparisonLessThan -> rhs_
                            Util.ComparisonGreaterThan -> parens rhs_
                            Util.ComparisonEqualTo -> Logic.ifElse (Logic.and (assocRight assoc) (assocRight rassoc)) rhs_ (parens rhs_)
                        _ -> rhs_
          in (Ast.ExprOp (Ast.OpExpr {
            Ast.opExprOp = op,
            Ast.opExprLhs = lhs2,
            Ast.opExprRhs = rhs2}))

prefix :: String -> Ast.Expr -> Ast.Expr
prefix p expr =

      let preOp =
              Ast.Op {
                Ast.opSymbol = (sym p),
                Ast.opPadding = Ast.Padding {
                  Ast.paddingLeft = Ast.WsNone,
                  Ast.paddingRight = Ast.WsNone},
                Ast.opPrecedence = (Ast.Precedence 0),
                Ast.opAssociativity = Ast.AssociativityNone}
      in (ifx preOp (cst "") expr)

printExpr :: Ast.Expr -> String
printExpr e =

      let pad =
              \ws -> case ws of
                Ast.WsNone -> ""
                Ast.WsSpace -> " "
                Ast.WsBreak -> "\n"
                Ast.WsBreakAndIndent _ -> "\n"
                Ast.WsDoubleBreak -> "\n\n"
          idt =
                  \ws -> \s -> case ws of
                    Ast.WsBreakAndIndent v0 -> customIndent v0 s
                    _ -> s
      in case e of
        Ast.ExprConst v0 -> Ast.unSymbol v0
        Ast.ExprIndent v0 ->
          let style = Ast.indentedExpressionStyle v0
              expr = Ast.indentedExpressionExpr v0
              lns = Strings.lines (printExpr expr)
              ilns =
                      case style of
                        Ast.IndentStyleAllLines v1 -> Lists.map (\line -> Strings.cat2 v1 line) lns
                        Ast.IndentStyleSubsequentLines v1 -> Logic.ifElse (Equality.equal (Lists.length lns) 1) lns (Lists.cons (Lists.head lns) (Lists.map (\line -> Strings.cat2 v1 line) (Lists.tail lns)))
          in (Strings.intercalate "\n" ilns)
        Ast.ExprSeq v0 ->
          let sop = Ast.seqExprOp v0
              ssym = Ast.unSymbol (Ast.opSymbol sop)
              spadding = Ast.opPadding sop
              spadl = Ast.paddingLeft spadding
              spadr = Ast.paddingRight spadding
              selements = Ast.seqExprElements v0
              separator = Strings.cat2 (Strings.cat2 (pad spadl) ssym) (pad spadr)
              printedElements = Lists.map (\el -> idt spadr (printExpr el)) selements
          in (Strings.intercalate separator printedElements)
        Ast.ExprOp v0 ->
          let op = Ast.opExprOp v0
              sym = Ast.unSymbol (Ast.opSymbol op)
              padding = Ast.opPadding op
              padl = Ast.paddingLeft padding
              padr = Ast.paddingRight padding
              l = Ast.opExprLhs v0
              r = Ast.opExprRhs v0
              lhs = idt padl (printExpr l)
              rhs = idt padr (printExpr r)
          in (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 lhs (pad padl)) sym) (pad padr)) rhs)
        Ast.ExprBrackets v0 ->
          let brs = Ast.bracketExprBrackets v0
              l = Ast.unSymbol (Ast.bracketsOpen brs)
              r = Ast.unSymbol (Ast.bracketsClose brs)
              e = Ast.bracketExprEnclosed v0
              style = Ast.bracketExprStyle v0
              body = printExpr e
              doIndent = Ast.blockStyleIndent style
              nlBefore = Ast.blockStyleNewlineBeforeContent style
              nlAfter = Ast.blockStyleNewlineAfterContent style
              ibody = Maybes.maybe body (\idt -> customIndent idt body) doIndent
              pre = Logic.ifElse nlBefore "\n" ""
              suf = Logic.ifElse nlAfter "\n" ""
          in (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 l pre) ibody) suf) r)

semicolonSep :: [Ast.Expr] -> Ast.Expr
semicolonSep = symbolSep ";" inlineStyle

sep :: Ast.Op -> [Ast.Expr] -> Ast.Expr
sep op els =
    Maybes.maybe (cst "") (\h -> Lists.foldl (\acc -> \el -> ifx op acc el) h (Lists.drop 1 els)) (Lists.safeHead els)

spaceSep :: [Ast.Expr] -> Ast.Expr
spaceSep =
    sep (Ast.Op {
      Ast.opSymbol = (sym ""),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsSpace,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone})

-- | Like sep, but produces a SeqExpr instead of an OpExpr chain. SeqExpr is treated as structural layout and is not subject to parenthesization.
structuralSep :: Ast.Op -> [Ast.Expr] -> Ast.Expr
structuralSep op els =
    Logic.ifElse (Lists.null els) (cst "") (Logic.ifElse (Equality.equal (Lists.length els) 1) (Lists.head els) (Ast.ExprSeq (Ast.SeqExpr {
      Ast.seqExprOp = op,
      Ast.seqExprElements = els})))

-- | Like spaceSep, but produces a SeqExpr. Use for structural layout that should not trigger parenthesization of children.
structuralSpaceSep :: [Ast.Expr] -> Ast.Expr
structuralSpaceSep =
    structuralSep (Ast.Op {
      Ast.opSymbol = (sym ""),
      Ast.opPadding = Ast.Padding {
        Ast.paddingLeft = Ast.WsSpace,
        Ast.paddingRight = Ast.WsNone},
      Ast.opPrecedence = (Ast.Precedence 0),
      Ast.opAssociativity = Ast.AssociativityNone})

squareBrackets :: Ast.Brackets
squareBrackets =
    Ast.Brackets {
      Ast.bracketsOpen = (Ast.Symbol "["),
      Ast.bracketsClose = (Ast.Symbol "]")}

-- | Append a suffix string to an expression
suffix :: String -> Ast.Expr -> Ast.Expr
suffix s expr =

      let sufOp =
              Ast.Op {
                Ast.opSymbol = (sym s),
                Ast.opPadding = Ast.Padding {
                  Ast.paddingLeft = Ast.WsNone,
                  Ast.paddingRight = Ast.WsNone},
                Ast.opPrecedence = (Ast.Precedence 0),
                Ast.opAssociativity = Ast.AssociativityNone}
      in (ifx sufOp expr (cst ""))

sym :: String -> Ast.Symbol
sym s = Ast.Symbol s

symbolSep :: String -> Ast.BlockStyle -> [Ast.Expr] -> Ast.Expr
symbolSep symb style l =

      let breakCount =
              Lists.length (Lists.filter (\x_ -> x_) [
                Ast.blockStyleNewlineBeforeContent style,
                (Ast.blockStyleNewlineAfterContent style)])
          break =
                  Logic.ifElse (Equality.equal breakCount 0) Ast.WsSpace (Logic.ifElse (Equality.equal breakCount 1) Ast.WsBreak Ast.WsDoubleBreak)
          commaOp =
                  Ast.Op {
                    Ast.opSymbol = (sym symb),
                    Ast.opPadding = Ast.Padding {
                      Ast.paddingLeft = Ast.WsNone,
                      Ast.paddingRight = break},
                    Ast.opPrecedence = (Ast.Precedence 0),
                    Ast.opAssociativity = Ast.AssociativityNone}
      in (Maybes.maybe (cst "") (\h -> Lists.foldl (\acc -> \el -> ifx commaOp acc el) h (Lists.drop 1 l)) (Lists.safeHead l))

tabIndent :: Ast.Expr -> Ast.Expr
tabIndent e =
    Ast.ExprIndent (Ast.IndentedExpression {
      Ast.indentedExpressionStyle = (Ast.IndentStyleAllLines "    "),
      Ast.indentedExpressionExpr = e})

tabIndentDoubleSpace :: [Ast.Expr] -> Ast.Expr
tabIndentDoubleSpace exprs = tabIndent (doubleNewlineSep exprs)

tabIndentSingleSpace :: [Ast.Expr] -> Ast.Expr
tabIndentSingleSpace exprs = tabIndent (newlineSep exprs)

unsupportedType :: String -> Ast.Expr
unsupportedType label = cst (Strings.cat2 (Strings.cat2 "[" label) "]")

unsupportedVariant :: String -> String -> Ast.Expr
unsupportedVariant label obj =
    cst (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "[unsupported " label) ": ") (Literals.showString obj)) "]")

withComma :: Ast.Expr -> Ast.Expr
withComma e =
    noSep [
      e,
      (cst ",")]

withSemi :: Ast.Expr -> Ast.Expr
withSemi e =
    noSep [
      e,
      (cst ";")]
