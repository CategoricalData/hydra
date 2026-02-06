
module Hydra.Sources.Kernel.Terms.Serialization where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  angleBraces, angleBracesList, braces, bracesList, brackets, bracketsList, bracesListAdaptive,
  bracketList, bracketListAdaptive, cat, commaSep, cst, curlyBlock, curlyBraces, curlyBracesList,
  customIndent, customIndentBlock, dotSep, doubleNewlineSep, doubleQuoted, doubleSpace,
  expressionLength, fullBlockStyle, fullName, halfBlockStyle, halfIndent, ifx, indent, indentBlock,
  indentLines, indentSubsequentLines, infixWs, infixWsList, inlineStyle, newline, newlineSep, noSep,
  noPad, noPadding, num, op, orOp, orSep, parenList, parens, parensList, parentheses, parenthesize,
  prefix, printExpr, printGraph, semicolonSep, sep, singleQuoted, space, spaceSep, squareBrackets,
  suffix, sym, symbolSep, tabIndent, tabIndentDoubleSpace, tabIndentSingleSpace, unsupportedType,
  unsupportedVariant, withComma, withSemi)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Meta.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Coders       as Coders
import qualified Hydra.Dsl.Meta.Compute      as Compute
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Meta.Json         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows    as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Meta.Module       as Module
import qualified Hydra.Dsl.Meta.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Meta.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Meta.Typing       as Typing
import qualified Hydra.Dsl.Meta.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import Hydra.Ast


ns :: Namespace
ns = Namespace "hydra.serialization"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just ("Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.")
  where
   elements = [
     toBinding angleBraces,
     toBinding angleBracesList,
     toBinding bracesListAdaptive,
     toBinding bracketList,
     toBinding bracketListAdaptive,
     toBinding brackets,
     toBinding commaSep,
     toBinding curlyBlock,
     toBinding curlyBraces,
     toBinding curlyBracesList,
     toBinding cst,
     toBinding customIndent,
     toBinding customIndentBlock,
     toBinding dotSep,
     toBinding doubleNewlineSep,
     toBinding doubleSpace,
     toBinding expressionLength,
     toBinding fullBlockStyle,
     toBinding halfBlockStyle,
     toBinding ifx,
     toBinding indent,
     toBinding indentBlock,
     toBinding indentSubsequentLines,
     toBinding infixWs,
     toBinding infixWsList,
     toBinding inlineStyle,
     toBinding newlineSep,
     toBinding noPadding,
     toBinding noSep,
     toBinding num,
     toBinding op,
     toBinding orOp,
     toBinding orSep,
     toBinding parenList,
     toBinding parens,
     toBinding parentheses,
     toBinding parenthesize,
     toBinding prefix,
     toBinding printExpr,
     toBinding semicolonSep,
     toBinding sep,
     toBinding spaceSep,
     toBinding squareBrackets,
     toBinding suffix,
     toBinding sym,
     toBinding symbolSep,
     toBinding tabIndent,
     toBinding tabIndentDoubleSpace,
     toBinding tabIndentSingleSpace,
     toBinding unsupportedType,
     toBinding unsupportedVariant,
     toBinding withComma,
     toBinding withSemi]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

angleBraces :: TBinding Brackets
angleBraces = define "angleBraces" $
  Ast.brackets (sym @@ string "<") (sym @@ string ">")

angleBracesList :: TBinding (BlockStyle -> [Expr] -> Expr)
angleBracesList = define "angleBracesList" $
  "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "<>")
      (brackets @@ angleBraces @@ var "style" @@ (commaSep @@ var "style" @@ var "els"))

bracesListAdaptive :: TBinding ([Expr] -> Expr)
bracesListAdaptive = define "bracesListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  "els" ~>
  "inlineList" <~ curlyBracesList @@ nothing @@ inlineStyle @@ var "els" $
  Logic.ifElse (Equality.gt (expressionLength @@ var "inlineList") (int32 70))
    (curlyBracesList @@ nothing @@ halfBlockStyle @@ var "els")
    (var "inlineList")

bracketListAdaptive :: TBinding ([Expr] -> Expr)
bracketListAdaptive = define "bracketListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  "els" ~>
  "inlineList" <~ bracketList @@ inlineStyle @@ var "els" $
  Logic.ifElse (Equality.gt (expressionLength @@ var "inlineList") (int32 70))
    (bracketList @@ halfBlockStyle @@ var "els")
    (var "inlineList")

bracketList :: TBinding (BlockStyle -> [Expr] -> Expr)
bracketList = define "bracketList" $
  "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "[]")
      (brackets @@ squareBrackets @@ var "style" @@ (commaSep @@ var "style" @@ var "els"))

brackets :: TBinding (Brackets -> BlockStyle -> Expr -> Expr)
brackets = define "brackets" $
  "br" ~> "style" ~> "e" ~>
    Ast.exprBrackets $ Ast.bracketExpr (var "br") (var "e") (var "style")

commaSep :: TBinding (BlockStyle -> [Expr] -> Expr)
commaSep = define "commaSep" $
  symbolSep @@ string ","

cst :: TBinding (String -> Expr)
cst = define "cst" $
  "s" ~> Ast.exprConst $ sym @@ var "s"

curlyBlock :: TBinding (BlockStyle -> Expr -> Expr)
curlyBlock = define "curlyBlock" $
  "style" ~> "e" ~>
    curlyBracesList @@ nothing @@ var "style" @@ (list [var "e"])

curlyBraces :: TBinding Brackets
curlyBraces = define "curlyBraces" $
  Ast.brackets (sym @@ string "{") (sym @@ string "}")

curlyBracesList :: TBinding (Maybe String -> BlockStyle -> [Expr] -> Expr)
curlyBracesList = define "curlyBracesList" $
  "msymb" ~> "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "{}")
      (brackets @@ curlyBraces @@ var "style" @@
        (symbolSep @@ (Maybes.fromMaybe (string ",") (var "msymb")) @@ var "style" @@ var "els"))

customIndentBlock :: TBinding (String -> [Expr] -> Expr)
customIndentBlock = define "customIndentBlock" $
  "idt" ~> "els" ~>
    "idtOp" <~ (Ast.op
      (sym @@ string "")
      (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ var "idt"))
      (Ast.precedence $ int32 0)
      Ast.associativityNone) $
    Maybes.maybe (cst @@ string "")
      ("head" ~> Logic.ifElse (Equality.equal (Lists.length $ var "els") (int32 1))
        (var "head")
        (ifx @@ var "idtOp" @@ var "head" @@ (newlineSep @@ Lists.drop (int32 1) (var "els"))))
      (Lists.safeHead $ var "els")

customIndent :: TBinding (String -> String -> String)
customIndent = define "customIndent" $
  "idt" ~> "s" ~> Strings.cat $
    Lists.intersperse (string "\n") $ Lists.map ("line" ~> var "idt" ++ var "line") $ Strings.lines $ var "s"

dotSep :: TBinding ([Expr] -> Expr)
dotSep = define "dotSep" $
  sep @@ (Ast.op
    (sym @@ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleNewlineSep :: TBinding ([Expr] -> Expr)
doubleNewlineSep = define "doubleNewlineSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsBreak Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleSpace :: TBinding String
doubleSpace = define "doubleSpace" $
  string "  "

expressionLength :: TBinding (Expr -> Int)
expressionLength = define "expressionLength" $
  doc "Find the approximate length (number of characters, including spaces and newlines) of an expression without actually printing it." $
  "e" ~>
  "symbolLength" <~ ("s" ~> Strings.length $ Ast.unSymbol $ var "s") $
  "wsLength" <~ ("ws" ~> cases _Ws (var "ws") Nothing [
    _Ws_none>>: constant $ int32 0,
    _Ws_space>>: constant $ int32 1,
    _Ws_break>>: constant $ int32 1,
    _Ws_breakAndIndent>>: "s" ~> Math.add (int32 1) (Strings.length $ var "s"),
    _Ws_doubleBreak>>: constant $ int32 2]) $
  "blockStyleLength" <~ ("style" ~>
    "mindentLen" <~ Maybes.maybe (int32 0) (unaryFunction Strings.length) (Ast.blockStyleIndent $ var "style") $
    "nlBeforeLen" <~ Logic.ifElse (Ast.blockStyleNewlineBeforeContent $ var "style") (int32 1) (int32 0) $
    "nlAfterLen" <~ Logic.ifElse (Ast.blockStyleNewlineAfterContent $ var "style") (int32 1) (int32 0) $
    Math.add (var "mindentLen") $ Math.add (var "nlBeforeLen") (var "nlAfterLen")) $
  "bracketsLength" <~ ("brackets" ~>
    Math.add
      (var "symbolLength" @@ (Ast.bracketsOpen $ var "brackets"))
      (var "symbolLength" @@ (Ast.bracketsClose $ var "brackets"))) $
  "bracketExprLength" <~ ("be" ~>
    Math.add
      (var "bracketsLength" @@ (Ast.bracketExprBrackets $ var "be"))
      (Math.add
        (expressionLength @@ (Ast.bracketExprEnclosed $ var "be"))
        (var "blockStyleLength" @@ (Ast.bracketExprStyle $ var "be")))) $
  "indentedExpressionLength" <~ ("ie" ~>
    "baseLen" <~ expressionLength @@ (Ast.indentedExpressionExpr $ var "ie") $
    "indentLen" <~ cases _IndentStyle (Ast.indentedExpressionStyle $ var "ie") Nothing [
      _IndentStyle_allLines>>: "s" ~> Strings.length $ var "s",
      _IndentStyle_subsequentLines>>: "s" ~> Strings.length $ var "s"] $
    Math.add (var "baseLen") (var "indentLen")) $
  "opLength" <~ ("op" ~>
    "symLen" <~ var "symbolLength" @@ (Ast.opSymbol $ var "op") $
    "padding" <~ Ast.opPadding (var "op") $
    "leftLen" <~ var "wsLength" @@ (Ast.paddingLeft $ var "padding") $
    "rightLen" <~ var "wsLength" @@ (Ast.paddingRight $ var "padding") $
    Math.add (var "symLen") $ Math.add (var "leftLen") (var "rightLen")) $
  "opExprLength" <~ ("oe" ~>
    "opLen" <~ var "opLength" @@ (Ast.opExprOp $ var "oe") $
    "leftLen" <~ expressionLength @@ (Ast.opExprLhs $ var "oe") $
    "rightLen" <~ expressionLength @@ (Ast.opExprRhs $ var "oe") $
    Math.add (var "opLen") $ Math.add (var "leftLen") (var "rightLen")) $
  cases _Expr (var "e") Nothing [
    _Expr_const>>: "s" ~> var "symbolLength" @@ var "s",
    _Expr_indent>>: "ie" ~> var "indentedExpressionLength" @@ var "ie",
    _Expr_op>>: "oe" ~> var "opExprLength" @@ var "oe",
    _Expr_brackets>>: "be" ~> var "bracketExprLength" @@ var "be"]

fullBlockStyle :: TBinding BlockStyle
fullBlockStyle = define "fullBlockStyle" $
  Ast.blockStyle (just doubleSpace) true true

halfBlockStyle :: TBinding BlockStyle
halfBlockStyle = define "halfBlockStyle" $
  Ast.blockStyle (just doubleSpace) true false

ifx :: TBinding (Op -> Expr -> Expr -> Expr)
ifx = define "ifx" $
  "op" ~> "lhs" ~> "rhs" ~>
    Ast.exprOp $ Ast.opExpr (var "op") (var "lhs") (var "rhs")

indentBlock :: TBinding ([Expr] -> Expr)
indentBlock = define "indentBlock" $
  customIndentBlock @@ doubleSpace

indent :: TBinding (String -> String)
indent = define "indent" $
  customIndent @@ doubleSpace

indentSubsequentLines :: TBinding (String -> Expr -> Expr)
indentSubsequentLines = define "indentSubsequentLines" $
  "idt" ~> "e" ~>
    Ast.exprIndent $ Ast.indentedExpression (Ast.indentStyleSubsequentLines $ var "idt") (var "e")

infixWs :: TBinding (String -> Expr -> Expr -> Expr)
infixWs = define "infixWs" $
  "op" ~> "l" ~> "r" ~>
    spaceSep @@ list [var "l", cst @@ var "op", var "r"]

infixWsList :: TBinding (String -> [Expr] -> Expr)
infixWsList = define "infixWsList" $
  "op" ~> "opers" ~>
  "opExpr" <~ cst @@ var "op" $
  "foldFun" <~ ("e" ~> "r" ~>
    Logic.ifElse (Lists.null $ var "e")
      (list [var "r"])
      (Lists.cons (var "r") $ Lists.cons (var "opExpr") (var "e"))) $
  spaceSep @@ (Lists.foldl (var "foldFun") (list ([] :: [TTerm Expr])) $ Lists.reverse $ var "opers")

inlineStyle :: TBinding BlockStyle
inlineStyle = define "inlineStyle" $
  Ast.blockStyle nothing false false

newlineSep :: TBinding ([Expr] -> Expr)
newlineSep = define "newlineSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

noPadding :: TBinding Padding
noPadding = define "noPadding" $
  Ast.padding Ast.wsNone Ast.wsNone

noSep :: TBinding ([Expr] -> Expr)
noSep = define "noSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

num :: TBinding (Int -> Expr)
num = define "num" $
  "i" ~> cst @@ (Literals.showInt32 $ var "i")

op :: TBinding (String -> Int -> Associativity -> Op)
op = define "op" $
  "s" ~> "p" ~> "assoc" ~>
    Ast.op
      (sym @@ var "s")
      (Ast.padding Ast.wsSpace Ast.wsSpace)
      (Ast.precedence $ var "p")
      (var "assoc")

orOp :: TBinding (Bool -> Op)
orOp = define "orOp" $
  "newlines" ~> Ast.op
    (sym @@ string "|")
    (Ast.padding Ast.wsSpace (Logic.ifElse (var "newlines") Ast.wsBreak Ast.wsSpace))
    (Ast.precedence $ int32 0)
    Ast.associativityNone

orSep :: TBinding (BlockStyle -> [Expr] -> Expr)
orSep = define "orSep" $
  "style" ~> "l" ~>
    "newlines" <~ Ast.blockStyleNewlineBeforeContent (var "style") $
    Maybes.maybe (cst @@ string "")
      ("h" ~> Lists.foldl ("acc" ~> "el" ~> ifx @@ (orOp @@ var "newlines") @@ var "acc" @@ var "el") (var "h") (Lists.drop (int32 1) (var "l")))
      (Lists.safeHead $ var "l")

parenList :: TBinding (Bool -> [Expr] -> Expr)
parenList = define "parenList" $
  "newlines" ~> "els" ~>
    "style" <~ (Logic.ifElse (Logic.and (var "newlines") (Equality.gt (Lists.length $ var "els") (int32 1)))
      (halfBlockStyle)
      (inlineStyle)) $
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "()")
      (brackets @@ parentheses @@ var "style" @@ (commaSep @@ var "style" @@ var "els"))

parens :: TBinding (Expr -> Expr)
parens = define "parens" $
  brackets @@ parentheses @@ inlineStyle

parentheses :: TBinding Brackets
parentheses = define "parentheses" $
  Ast.brackets (sym @@ string "(") (sym @@ string ")")

parenthesize :: TBinding (Expr -> Expr)
parenthesize = define "parenthesize" $
  "exp" ~>
    "assocLeft" <~ ("a" ~> cases _Associativity (var "a")
      (Just true) [
      _Associativity_right>>: constant false]) $
    "assocRight" <~ ("a" ~> cases _Associativity (var "a")
      (Just true) [
      _Associativity_left>>: constant false]) $
    cases _Expr (var "exp") Nothing [
      _Expr_brackets>>: "bracketExpr" ~>
        Ast.exprBrackets $ Ast.bracketExpr
          (Ast.bracketExprBrackets $ var "bracketExpr")
          (parenthesize @@ (Ast.bracketExprEnclosed $ var "bracketExpr"))
          (Ast.bracketExprStyle $ var "bracketExpr"),
      _Expr_const>>: "ignored" ~> var "exp",
      _Expr_indent>>: "indentExpr" ~>
        Ast.exprIndent $ Ast.indentedExpression
          (Ast.indentedExpressionStyle $ var "indentExpr")
          (parenthesize @@ (Ast.indentedExpressionExpr $ var "indentExpr")),
      _Expr_op>>: "opExpr" ~>
        "op" <~ Ast.opExprOp (var "opExpr") $
        "prec" <~ Ast.unPrecedence (Ast.opPrecedence $ var "op") $
        "assoc" <~ Ast.opAssociativity (var "op") $
        "lhs" <~ Ast.opExprLhs (var "opExpr") $
        "rhs" <~ Ast.opExprRhs (var "opExpr") $
        "lhs'" <~ parenthesize @@ var "lhs" $
        "rhs'" <~ parenthesize @@ var "rhs" $
        "lhs2" <~ cases _Expr (var "lhs'")
          (Just $ var "lhs'") [
          _Expr_op>>: "lopExpr" ~>
            "lop" <~ Ast.opExprOp (var "lopExpr") $
            "lprec" <~ Ast.unPrecedence (Ast.opPrecedence $ var "lop") $
            "lassoc" <~ Ast.opAssociativity (var "lop") $
            "comparison" <~ Equality.compare (var "prec") (var "lprec") $
            cases _Comparison (var "comparison") Nothing [
              _Comparison_lessThan>>: constant $ var "lhs'",
              _Comparison_greaterThan>>: constant (parens @@ var "lhs'"),
              _Comparison_equalTo>>: constant $ Logic.ifElse
                (Logic.and (var "assocLeft" @@ var "assoc") (var "assocLeft" @@ var "lassoc"))
                (var "lhs'")
                (parens @@ var "lhs'")]] $
        "rhs2" <~ cases _Expr (var "rhs'") (Just $ var "rhs'") [
          _Expr_op>>: "ropExpr" ~>
            "rop" <~ Ast.opExprOp (var "ropExpr") $
            "rprec" <~ Ast.unPrecedence (Ast.opPrecedence $ var "rop") $
            "rassoc" <~ Ast.opAssociativity (var "rop") $
            "comparison" <~ Equality.compare (var "prec") (var "rprec") $
            cases _Comparison (var "comparison") Nothing [
              _Comparison_lessThan>>: constant $ var "rhs'",
              _Comparison_greaterThan>>: constant (parens @@ var "rhs'"),
              _Comparison_equalTo>>: constant $ Logic.ifElse
                (Logic.and (var "assocRight" @@ var "assoc") (var "assocRight" @@ var "rassoc"))
                (var "rhs'")
                (parens @@ var "rhs'")]] $
        Ast.exprOp $ Ast.opExpr (var "op") (var "lhs2") (var "rhs2")]

prefix :: TBinding (String -> Expr -> Expr)
prefix = define "prefix" $
  "p" ~> "expr" ~>
  "preOp" <~ Ast.op
    (sym @@ var "p")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  ifx @@ var "preOp" @@ (cst @@ string "") @@ var "expr"

printExpr :: TBinding (Expr -> String)
printExpr = define "printExpr" $
  "e" ~>
  "pad" <~ ("ws" ~> cases _Ws (var "ws") Nothing [
    _Ws_none>>: constant $ string "",
    _Ws_space>>: constant $ string " ",
    _Ws_break>>: constant $ string "\n",
    _Ws_breakAndIndent>>: "ignored" ~> string "\n",
    _Ws_doubleBreak>>: constant $ string "\n\n"]) $
  "idt" <~ ("ws" ~> "s" ~> cases _Ws (var "ws") (Just $ var "s") [
    _Ws_breakAndIndent>>: "indentStr" ~> customIndent @@ var "indentStr" @@ var "s"]) $
  cases _Expr (var "e") Nothing [
    _Expr_const>>: "symbol" ~> Ast.unSymbol $ var "symbol",
    _Expr_indent>>: "indentExpr" ~>
      "style" <~ Ast.indentedExpressionStyle (var "indentExpr") $
      "expr" <~ Ast.indentedExpressionExpr (var "indentExpr") $
      "lns" <~ Strings.lines (printExpr @@ var "expr") $
      "ilns" <~ cases _IndentStyle (var "style") Nothing [
        _IndentStyle_allLines>>: "idt" ~> Lists.map ("line" ~> var "idt" ++ var "line") (var "lns"),
        _IndentStyle_subsequentLines>>: "idt" ~>
          Logic.ifElse (Equality.equal (Lists.length $ var "lns") (int32 1))
            (var "lns")
            (Lists.cons (Lists.head $ var "lns") $ Lists.map ("line" ~> var "idt" ++ var "line") $ Lists.tail $ var "lns")] $
      Strings.intercalate (string "\n") (var "ilns"),
    _Expr_op>>: "opExpr" ~>
      "op" <~ Ast.opExprOp (var "opExpr") $
      "sym" <~ Ast.unSymbol (Ast.opSymbol $ var "op") $
      "padding" <~ Ast.opPadding (var "op") $
      "padl" <~ Ast.paddingLeft (var "padding") $
      "padr" <~ Ast.paddingRight (var "padding") $
      "l" <~ Ast.opExprLhs (var "opExpr") $
      "r" <~ Ast.opExprRhs (var "opExpr") $
      "lhs" <~ var "idt" @@ var "padl" @@ (printExpr @@ var "l") $
      "rhs" <~ var "idt" @@ var "padr" @@ (printExpr @@ var "r") $
      var "lhs" ++ (var "pad" @@ var "padl") ++ var "sym" ++ (var "pad" @@ var "padr") ++ var "rhs",
    _Expr_brackets>>: "bracketExpr" ~>
      "brackets" <~ Ast.bracketExprBrackets (var "bracketExpr") $
      "l" <~ Ast.unSymbol (Ast.bracketsOpen $ var "brackets") $
      "r" <~ Ast.unSymbol (Ast.bracketsClose $ var "brackets") $
      "e" <~ Ast.bracketExprEnclosed (var "bracketExpr") $
      "style" <~ Ast.bracketExprStyle (var "bracketExpr") $
      "body" <~ printExpr @@ var "e" $
      "doIndent" <~ Ast.blockStyleIndent (var "style") $
      "nlBefore" <~ Ast.blockStyleNewlineBeforeContent (var "style") $
      "nlAfter" <~ Ast.blockStyleNewlineAfterContent (var "style") $
      "ibody" <~ Maybes.maybe (var "body") ("idt" ~> customIndent @@ var "idt" @@ var "body") (var "doIndent") $
      "pre" <~ Logic.ifElse (var "nlBefore") (string "\n") (string "") $
      "suf" <~ Logic.ifElse (var "nlAfter") (string "\n") (string "") $
      var "l" ++ var "pre" ++ var "ibody" ++ var "suf" ++ var "r"]

semicolonSep :: TBinding ([Expr] -> Expr)
semicolonSep = define "semicolonSep" $
  symbolSep @@ string ";" @@ inlineStyle

sep :: TBinding (Op -> [Expr] -> Expr)
sep = define "sep" $
  "op" ~> "els" ~>
    Maybes.maybe (cst @@ string "")
      ("h" ~> Lists.foldl ("acc" ~> "el" ~> ifx @@ var "op" @@ var "acc" @@ var "el") (var "h") (Lists.drop (int32 1) (var "els")))
      (Lists.safeHead $ var "els")

spaceSep :: TBinding ([Expr] -> Expr)
spaceSep = define "spaceSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsSpace Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

squareBrackets :: TBinding Brackets
squareBrackets = define "squareBrackets" $
  Ast.brackets (sym @@ string "[") (sym @@ string "]")

suffix :: TBinding (String -> Expr -> Expr)
suffix = define "suffix" $
  doc "Append a suffix string to an expression" $
  "s" ~> "expr" ~>
  "sufOp" <~ Ast.op
    (sym @@ var "s")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  ifx @@ var "sufOp" @@ var "expr" @@ (cst @@ string "")

sym :: TBinding (String -> Symbol)
sym = define "sym" $
  "s" ~> Ast.symbol (var "s")

symbolSep :: TBinding (String -> BlockStyle -> [Expr] -> Expr)
symbolSep = define "symbolSep" $
  "symb" ~> "style" ~> "l" ~>
    "breakCount" <~ (Lists.length $ Lists.filter identity $ list [
      Ast.blockStyleNewlineBeforeContent $ var "style",
      Ast.blockStyleNewlineAfterContent $ var "style"]) $
    "break" <~ (Logic.ifElse (Equality.equal (var "breakCount") (int32 0))
      Ast.wsSpace
      (Logic.ifElse (Equality.equal (var "breakCount") (int32 1))
        Ast.wsBreak
        Ast.wsDoubleBreak)) $
    "commaOp" <~ (Ast.op
      (sym @@ var "symb")
      (Ast.padding Ast.wsNone (var "break"))
      (Ast.precedence $ int32 0)
      Ast.associativityNone) $
    Maybes.maybe (cst @@ string "")
      ("h" ~> Lists.foldl ("acc" ~> "el" ~> ifx @@ var "commaOp" @@ var "acc" @@ var "el") (var "h") (Lists.drop (int32 1) (var "l")))
      (Lists.safeHead $ var "l")

tabIndent :: TBinding (Expr -> Expr)
tabIndent = define "tabIndent" $
  "e" ~> Ast.exprIndent $ Ast.indentedExpression
    (Ast.indentStyleAllLines $ string "    ")
    (var "e")

tabIndentDoubleSpace :: TBinding ([Expr] -> Expr)
tabIndentDoubleSpace = define "tabIndentDoubleSpace" $
  "exprs" ~> tabIndent @@ (doubleNewlineSep @@ var "exprs")

tabIndentSingleSpace :: TBinding ([Expr] -> Expr)
tabIndentSingleSpace = define "tabIndentSingleSpace" $
  "exprs" ~> tabIndent @@ (newlineSep @@ var "exprs")

unsupportedType :: TBinding (String -> Expr)
unsupportedType = define "unsupportedType" $
  "label" ~> cst @@ (string "[" ++ var "label" ++ string "]")

unsupportedVariant :: TBinding (String -> a -> Expr)
unsupportedVariant = define "unsupportedVariant" $
  "label" ~> "obj" ~>
    cst @@ (string "[unsupported " ++ var "label" ++ string ": " ++ (Literals.showString $ var "obj") ++ string "]")

withComma :: TBinding (Expr -> Expr)
withComma = define "withComma" $
  "e" ~> noSep @@ list [var "e", cst @@ string ","]

withSemi :: TBinding (Expr -> Expr)
withSemi = define "withSemi" $
  "e" ~> noSep @@ list [var "e", cst @@ string ";"]
