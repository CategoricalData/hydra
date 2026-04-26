
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
  structuralSep, structuralSpaceSep,
  suffix, sym, symbolSep, tabIndent, tabIndentDoubleSpace, tabIndentSingleSpace, unsupportedType,
  unsupportedVariant, withComma, withSemi)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
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
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
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
module_ = Module ns definitions
    []
    kernelTypesNamespaces $
    Just ("Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.")
  where
   definitions = [
     toDefinition angleBraces,
     toDefinition angleBracesList,
     toDefinition bracesListAdaptive,
     toDefinition bracketList,
     toDefinition bracketListAdaptive,
     toDefinition brackets,
     toDefinition commaSep,
     toDefinition curlyBlock,
     toDefinition curlyBraces,
     toDefinition curlyBracesList,
     toDefinition cst,
     toDefinition customIndent,
     toDefinition customIndentBlock,
     toDefinition dotSep,
     toDefinition doubleNewlineSep,
     toDefinition doubleSpace,
     toDefinition expressionLength,
     toDefinition fullBlockStyle,
     toDefinition halfBlockStyle,
     toDefinition ifx,
     toDefinition indent,
     toDefinition indentBlock,
     toDefinition indentSubsequentLines,
     toDefinition infixWs,
     toDefinition infixWsList,
     toDefinition inlineStyle,
     toDefinition newlineSep,
     toDefinition noPadding,
     toDefinition noSep,
     toDefinition num,
     toDefinition op,
     toDefinition orOp,
     toDefinition orSep,
     toDefinition parenList,
     toDefinition parens,
     toDefinition parentheses,
     toDefinition parenthesize,
     toDefinition prefix,
     toDefinition printExpr,
     toDefinition semicolonSep,
     toDefinition sep,
     toDefinition spaceSep,
     toDefinition structuralSep,
     toDefinition structuralSpaceSep,
     toDefinition squareBrackets,
     toDefinition suffix,
     toDefinition sym,
     toDefinition symbolSep,
     toDefinition tabIndent,
     toDefinition tabIndentDoubleSpace,
     toDefinition tabIndentSingleSpace,
     toDefinition unsupportedType,
     toDefinition unsupportedVariant,
     toDefinition withComma,
     toDefinition withSemi]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

angleBraces :: TTermDefinition Brackets
angleBraces = define "angleBraces" $
  Ast.brackets (Ast.symbol (string "<")) (Ast.symbol (string ">"))

angleBracesList :: TTermDefinition (BlockStyle -> [Expr] -> Expr)
angleBracesList = define "angleBracesList" $
  "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "<>")
      (brackets @@ angleBraces @@ var "style" @@ (commaSep @@ var "style" @@ var "els"))

bracesListAdaptive :: TTermDefinition ([Expr] -> Expr)
bracesListAdaptive = define "bracesListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  "els" ~>
  "inlineList" <~ curlyBracesList @@ nothing @@ inlineStyle @@ var "els" $
  Logic.ifElse (Equality.gt (expressionLength @@ var "inlineList") (int32 70))
    (curlyBracesList @@ nothing @@ halfBlockStyle @@ var "els")
    (var "inlineList")

bracketListAdaptive :: TTermDefinition ([Expr] -> Expr)
bracketListAdaptive = define "bracketListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  "els" ~>
  "inlineList" <~ bracketList @@ inlineStyle @@ var "els" $
  Logic.ifElse (Equality.gt (expressionLength @@ var "inlineList") (int32 70))
    (bracketList @@ halfBlockStyle @@ var "els")
    (var "inlineList")

bracketList :: TTermDefinition (BlockStyle -> [Expr] -> Expr)
bracketList = define "bracketList" $
  "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "[]")
      (brackets @@ squareBrackets @@ var "style" @@ (commaSep @@ var "style" @@ var "els"))

brackets :: TTermDefinition (Brackets -> BlockStyle -> Expr -> Expr)
brackets = define "brackets" $
  "br" ~> "style" ~> "e" ~>
    Ast.exprBrackets $ Ast.bracketExpr (var "br") (var "e") (var "style")

commaSep :: TTermDefinition (BlockStyle -> [Expr] -> Expr)
commaSep = define "commaSep" $
  symbolSep @@ string ","

cst :: TTermDefinition (String -> Expr)
cst = define "cst" $
  "s" ~> Ast.exprConst $ sym @@ var "s"

curlyBlock :: TTermDefinition (BlockStyle -> Expr -> Expr)
curlyBlock = define "curlyBlock" $
  "style" ~> "e" ~>
    curlyBracesList @@ nothing @@ var "style" @@ (list [var "e"])

curlyBraces :: TTermDefinition Brackets
curlyBraces = define "curlyBraces" $
  Ast.brackets (Ast.symbol (string "{")) (Ast.symbol (string "}"))

curlyBracesList :: TTermDefinition (Maybe String -> BlockStyle -> [Expr] -> Expr)
curlyBracesList = define "curlyBracesList" $
  "msymb" ~> "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "{}")
      (brackets @@ curlyBraces @@ var "style" @@
        (symbolSep @@ (Maybes.fromMaybe (string ",") (var "msymb")) @@ var "style" @@ var "els"))

customIndentBlock :: TTermDefinition (String -> [Expr] -> Expr)
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
      (Lists.maybeHead $ var "els")

customIndent :: TTermDefinition (String -> String -> String)
customIndent = define "customIndent" $
  doc ("Indent every non-empty line of `s` by `idt`. Empty lines stay empty"
    <> " (no trailing whitespace) so downstream byte-identity checks don't"
    <> " care about indent depth.") $
  "idt" ~> "s" ~> Strings.cat $
    Lists.intersperse (string "\n") $
      Lists.map ("line" ~>
        Logic.ifElse (Equality.equal (var "line") (string "")) (var "line") (var "idt" ++ var "line")) $
      Strings.lines $ var "s"

dotSep :: TTermDefinition ([Expr] -> Expr)
dotSep = define "dotSep" $
  sep @@ (Ast.op
    (sym @@ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleNewlineSep :: TTermDefinition ([Expr] -> Expr)
doubleNewlineSep = define "doubleNewlineSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsBreak Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleSpace :: TTermDefinition String
doubleSpace = define "doubleSpace" $
  string "  "

expressionLength :: TTermDefinition (Expr -> Int)
expressionLength = define "expressionLength" $
  doc "Find the approximate length (number of characters, including spaces and newlines) of an expression without actually printing it." $
  "e" ~>
  "symbolLength" <~ ("s" ~> Strings.length $ Ast.unSymbol $ var "s") $
  "wsLength" <~ ("ws" ~> cases _Ws (var "ws") Nothing [
    _Ws_none>>: constant $ int32 0,
    _Ws_space>>: constant $ int32 1,
    _Ws_break>>: constant $ int32 10000,
    _Ws_breakAndIndent>>: "s" ~> int32 10000,
    _Ws_doubleBreak>>: constant $ int32 10000]) $
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
  "seqExprLength" <~ ("se" ~>
    "sopLen" <~ var "opLength" @@ (Ast.seqExprOp $ var "se") $
    "elementLens" <~ Lists.map (expressionLength) (Ast.seqExprElements $ var "se") $
    "totalElLen" <~ Lists.foldl (binaryFunction Math.add) (int32 0) (var "elementLens") $
    "numSeps" <~ Math.sub (Lists.length $ Ast.seqExprElements $ var "se") (int32 1) $
    Math.add (var "totalElLen") (Math.mul (var "sopLen") (Logic.ifElse (Equality.gt (var "numSeps") (int32 0)) (var "numSeps") (int32 0)))) $
  cases _Expr (var "e") Nothing [
    _Expr_const>>: "s" ~> var "symbolLength" @@ var "s",
    _Expr_indent>>: "ie" ~> var "indentedExpressionLength" @@ var "ie",
    _Expr_op>>: "oe" ~> var "opExprLength" @@ var "oe",
    _Expr_brackets>>: "be" ~> var "bracketExprLength" @@ var "be",
    _Expr_seq>>: "se" ~> var "seqExprLength" @@ var "se"]

fullBlockStyle :: TTermDefinition BlockStyle
fullBlockStyle = define "fullBlockStyle" $
  Ast.blockStyle (just doubleSpace) true true

halfBlockStyle :: TTermDefinition BlockStyle
halfBlockStyle = define "halfBlockStyle" $
  Ast.blockStyle (just doubleSpace) true false

ifx :: TTermDefinition (Op -> Expr -> Expr -> Expr)
ifx = define "ifx" $
  "op" ~> "lhs" ~> "rhs" ~>
    Ast.exprOp $ Ast.opExpr (var "op") (var "lhs") (var "rhs")

indentBlock :: TTermDefinition ([Expr] -> Expr)
indentBlock = define "indentBlock" $
  customIndentBlock @@ doubleSpace

indent :: TTermDefinition (String -> String)
indent = define "indent" $
  customIndent @@ doubleSpace

indentSubsequentLines :: TTermDefinition (String -> Expr -> Expr)
indentSubsequentLines = define "indentSubsequentLines" $
  "idt" ~> "e" ~>
    Ast.exprIndent $ Ast.indentedExpression (Ast.indentStyleSubsequentLines $ var "idt") (var "e")

infixWs :: TTermDefinition (String -> Expr -> Expr -> Expr)
infixWs = define "infixWs" $
  "op" ~> "l" ~> "r" ~>
    spaceSep @@ list [var "l", cst @@ var "op", var "r"]

infixWsList :: TTermDefinition (String -> [Expr] -> Expr)
infixWsList = define "infixWsList" $
  "op" ~> "opers" ~>
  "opExpr" <~ cst @@ var "op" $
  "foldFun" <~ ("e" ~> "r" ~>
    Logic.ifElse (Lists.null $ var "e")
      (list [var "r"])
      (Lists.cons (var "r") $ Lists.cons (var "opExpr") (var "e"))) $
  spaceSep @@ (Lists.foldl (var "foldFun") (list ([] :: [TTerm Expr])) $ Lists.reverse $ var "opers")

inlineStyle :: TTermDefinition BlockStyle
inlineStyle = define "inlineStyle" $
  Ast.blockStyle nothing false false

newlineSep :: TTermDefinition ([Expr] -> Expr)
newlineSep = define "newlineSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

noPadding :: TTermDefinition Padding
noPadding = define "noPadding" $
  Ast.padding Ast.wsNone Ast.wsNone

noSep :: TTermDefinition ([Expr] -> Expr)
noSep = define "noSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

num :: TTermDefinition (Int -> Expr)
num = define "num" $
  "i" ~> cst @@ (Literals.showInt32 $ var "i")

op :: TTermDefinition (String -> Int -> Associativity -> Op)
op = define "op" $
  "s" ~> "p" ~> "assoc" ~>
    Ast.op
      (sym @@ var "s")
      (Ast.padding Ast.wsSpace Ast.wsSpace)
      (Ast.precedence $ var "p")
      (var "assoc")

orOp :: TTermDefinition (Bool -> Op)
orOp = define "orOp" $
  "newlines" ~> Ast.op
    (sym @@ string "|")
    (Ast.padding Ast.wsSpace (Logic.ifElse (var "newlines") Ast.wsBreak Ast.wsSpace))
    (Ast.precedence $ int32 0)
    Ast.associativityNone

orSep :: TTermDefinition (BlockStyle -> [Expr] -> Expr)
orSep = define "orSep" $
  "style" ~> "l" ~>
    "newlines" <~ Ast.blockStyleNewlineBeforeContent (var "style") $
    Maybes.maybe (cst @@ string "")
      ("h" ~> Lists.foldl ("acc" ~> "el" ~> ifx @@ (orOp @@ var "newlines") @@ var "acc" @@ var "el") (var "h") (Lists.drop (int32 1) (var "l")))
      (Lists.maybeHead $ var "l")

parenList :: TTermDefinition (Bool -> [Expr] -> Expr)
parenList = define "parenList" $
  "newlines" ~> "els" ~>
    "style" <~ (Logic.ifElse (Logic.and (var "newlines") (Equality.gt (Lists.length $ var "els") (int32 1)))
      (halfBlockStyle)
      (inlineStyle)) $
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "()")
      (brackets @@ parentheses @@ var "style" @@ (commaSep @@ var "style" @@ var "els"))

parens :: TTermDefinition (Expr -> Expr)
parens = define "parens" $
  brackets @@ parentheses @@ inlineStyle

parentheses :: TTermDefinition Brackets
parentheses = define "parentheses" $
  Ast.brackets (Ast.symbol (string "(")) (Ast.symbol (string ")"))

parenthesize :: TTermDefinition (Expr -> Expr)
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
      _Expr_seq>>: "seqExpr" ~>
        Ast.exprSeq $ Ast.seqExpr
          (Ast.seqExprOp $ var "seqExpr")
          (Lists.map (parenthesize) (Ast.seqExprElements $ var "seqExpr")),
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

prefix :: TTermDefinition (String -> Expr -> Expr)
prefix = define "prefix" $
  "p" ~> "expr" ~>
  "preOp" <~ Ast.op
    (sym @@ var "p")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  ifx @@ var "preOp" @@ (cst @@ string "") @@ var "expr"

printExpr :: TTermDefinition (Expr -> String)
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
      -- Indent prefix is applied only to non-empty lines; otherwise
      -- empty lines pick up trailing whitespace and downstream tools
      -- (host writers, byte-identity comparisons in the bootstrap
      -- demo) have to strip it post-hoc.
      "indentLine" <~ ("idt" ~> "line" ~>
        Logic.ifElse (Equality.equal (var "line") (string "")) (var "line") (var "idt" ++ var "line")) $
      "ilns" <~ cases _IndentStyle (var "style") Nothing [
        _IndentStyle_allLines>>: "idt" ~> Lists.map (var "indentLine" @@ var "idt") (var "lns"),
        _IndentStyle_subsequentLines>>: "idt" ~>
          Logic.ifElse (Equality.equal (Lists.length $ var "lns") (int32 1))
            (var "lns")
            (Maybes.fromMaybe (var "lns") $
              Maybes.map
                ("uc" ~> Lists.cons (Pairs.first $ var "uc") $
                  Lists.map (var "indentLine" @@ var "idt") (Pairs.second $ var "uc"))
                (Lists.uncons $ var "lns"))] $
      Strings.intercalate (string "\n") (var "ilns"),
    _Expr_seq>>: "seqExpr" ~>
      "sop" <~ Ast.seqExprOp (var "seqExpr") $
      "ssym" <~ Ast.unSymbol (Ast.opSymbol $ var "sop") $
      "spadding" <~ Ast.opPadding (var "sop") $
      "spadl" <~ Ast.paddingLeft (var "spadding") $
      "spadr" <~ Ast.paddingRight (var "spadding") $
      "selements" <~ Ast.seqExprElements (var "seqExpr") $
      "printedElements" <~ Lists.map ("el" ~> var "idt" @@ var "spadr" @@ (printExpr @@ var "el")) (var "selements") $
      -- Trailing-whitespace defense: when stitching elements with the
      -- separator `pad(spadl) ++ ssym ++ pad(spadr)`, whitespace in
      -- the separator can dangle before/after a newline if an element
      -- starts with `\n`. Result: "prev \nel" or "prev<sym> \nel".
      --
      -- Fix: build the joined string element-by-element. For each
      -- element after the head, if it starts with `\n` then suppress
      -- the whitespace parts of the separator (only the symbol stays).
      "isNewlineWs" <~ ("ws" ~> cases _Ws (var "ws") (Just $ boolean False) [
        _Ws_break>>: constant true,
        _Ws_breakAndIndent>>: constant true,
        _Ws_doubleBreak>>: constant true]) $
      "spadlIsNewline" <~ var "isNewlineWs" @@ var "spadl" $
      "spadrIsNewline" <~ var "isNewlineWs" @@ var "spadr" $
      "joinElements" <~ ("acc" ~> "el" ~>
        "elStartsWithNewline" <~ Maybes.maybe
          (boolean False)
          ("c" ~> Equality.equal (var "c") (int32 10))
          (Strings.maybeCharAt (int32 0) (var "el")) $
        "elIsEmpty" <~ Equality.equal (var "el") (string "") $
        -- Suppress whitespace padding when the next element either
        -- starts with `\n` (whitespace would dangle on previous line)
        -- or is empty (whitespace would be at end of output unattached
        -- to anything). Symbol stays in either case.
        "padlEff" <~ Logic.ifElse
          (Logic.or
            (Logic.and (var "elStartsWithNewline") (Logic.not $ var "spadlIsNewline"))
            (Logic.and (var "elIsEmpty") (Equality.equal (var "ssym") (string ""))))
          (string "")
          (var "pad" @@ var "spadl") $
        "padrEff" <~ Logic.ifElse
          (Logic.or
            (Logic.and (var "elStartsWithNewline") (Logic.not $ var "spadrIsNewline"))
            (Logic.and (var "elIsEmpty") (Equality.equal (var "ssym") (string ""))))
          (string "")
          (var "pad" @@ var "spadr") $
        Strings.cat $ list [var "acc", var "padlEff", var "ssym", var "padrEff", var "el"]) $
      Maybes.maybe
        (string "")
        ("h" ~> Lists.foldl (var "joinElements") (var "h") (Lists.drop (int32 1) (var "printedElements")))
        (Lists.maybeHead $ var "printedElements"),
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
      -- Two trailing-whitespace defenses around the inner separator:
      --
      -- 1. When sym is empty AND padr is one of the newline-emitting Ws
      --    values, padl whitespace would land at the end of a line
      --    (lhs + " " + "" + "\n" + rhs → "code \nrhs"). Suppress padl.
      --
      -- 2. When padr's pad string would land before a leading newline in
      --    rhs (rhs starts with \n because the inner expression breaks),
      --    drop padr's pad. Without this, "lhs sym <space>\nrhs" emits
      --    a trailing space on the sym's line.
      --
      -- Newline-emitting Ws values are WsBreak, WsBreakAndIndent, and
      -- WsDoubleBreak.
      "padrIsNewline" <~ cases _Ws (var "padr") (Just $ boolean False) [
        _Ws_break>>: constant true,
        _Ws_breakAndIndent>>: constant true,
        _Ws_doubleBreak>>: constant true] $
      -- Suppress padl whitespace when:
      --  (a) sym is empty AND padr is a newline (lhs's trailing space
      --      would land at end of previous line), OR
      --  (b) sym is empty AND rhs is empty (the entire trailing
      --      separator is just whitespace with nothing after).
      "padlEffective" <~ Logic.ifElse
        (Logic.and (Equality.equal (var "sym") (string ""))
                   (Logic.or (var "padrIsNewline")
                             (Equality.equal (var "rhs") (string ""))))
        (string "")
        (var "pad" @@ var "padl") $
      "padrPad" <~ (var "pad" @@ var "padr") $
      "rhsStartsWithNewline" <~ Maybes.maybe
        (boolean False)
        ("c" ~> Equality.equal (var "c") (int32 10))
        (Strings.maybeCharAt (int32 0) (var "rhs")) $
      "padrEffective" <~ Logic.ifElse
        (Logic.and (var "rhsStartsWithNewline")
                   (Logic.not $ var "padrIsNewline"))
        (string "")
        (var "padrPad") $
      var "lhs" ++ var "padlEffective" ++ var "sym" ++ var "padrEffective" ++ var "rhs",
    _Expr_brackets>>: "bracketExpr" ~>
      "brs" <~ Ast.bracketExprBrackets (var "bracketExpr") $
      "l" <~ Ast.unSymbol (Ast.bracketsOpen $ var "brs") $
      "r" <~ Ast.unSymbol (Ast.bracketsClose $ var "brs") $
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

semicolonSep :: TTermDefinition ([Expr] -> Expr)
semicolonSep = define "semicolonSep" $
  symbolSep @@ string ";" @@ inlineStyle

sep :: TTermDefinition (Op -> [Expr] -> Expr)
sep = define "sep" $
  "op" ~> "els" ~>
    Maybes.maybe (cst @@ string "")
      ("h" ~> Lists.foldl ("acc" ~> "el" ~> ifx @@ var "op" @@ var "acc" @@ var "el") (var "h") (Lists.drop (int32 1) (var "els")))
      (Lists.maybeHead $ var "els")

spaceSep :: TTermDefinition ([Expr] -> Expr)
spaceSep = define "spaceSep" $
  sep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsSpace Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

structuralSep :: TTermDefinition (Op -> [Expr] -> Expr)
structuralSep = define "structuralSep" $
  doc "Like sep, but produces a SeqExpr instead of an OpExpr chain. SeqExpr is treated as structural layout and is not subject to parenthesization." $
  "op" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (cst @@ string "")
      (Logic.ifElse (Equality.equal (Lists.length $ var "els") (int32 1))
        (Maybes.fromMaybe (cst @@ string "") (Lists.maybeHead $ var "els"))
        (Ast.exprSeq $ Ast.seqExpr (var "op") (var "els")))

structuralSpaceSep :: TTermDefinition ([Expr] -> Expr)
structuralSpaceSep = define "structuralSpaceSep" $
  doc "Like spaceSep, but produces a SeqExpr. Use for structural layout that should not trigger parenthesization of children." $
  structuralSep @@ (Ast.op
    (sym @@ string "")
    (Ast.padding Ast.wsSpace Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

squareBrackets :: TTermDefinition Brackets
squareBrackets = define "squareBrackets" $
  Ast.brackets (Ast.symbol (string "[")) (Ast.symbol (string "]"))

suffix :: TTermDefinition (String -> Expr -> Expr)
suffix = define "suffix" $
  doc "Append a suffix string to an expression" $
  "s" ~> "expr" ~>
  "sufOp" <~ Ast.op
    (sym @@ var "s")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  ifx @@ var "sufOp" @@ var "expr" @@ (cst @@ string "")

sym :: TTermDefinition (String -> Symbol)
sym = define "sym" $
  "s" ~> Ast.symbol (var "s")

symbolSep :: TTermDefinition (String -> BlockStyle -> [Expr] -> Expr)
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
      (Lists.maybeHead $ var "l")

tabIndent :: TTermDefinition (Expr -> Expr)
tabIndent = define "tabIndent" $
  "e" ~> Ast.exprIndent $ Ast.indentedExpression
    (Ast.indentStyleAllLines $ string "    ")
    (var "e")

tabIndentDoubleSpace :: TTermDefinition ([Expr] -> Expr)
tabIndentDoubleSpace = define "tabIndentDoubleSpace" $
  "exprs" ~> tabIndent @@ (doubleNewlineSep @@ var "exprs")

tabIndentSingleSpace :: TTermDefinition ([Expr] -> Expr)
tabIndentSingleSpace = define "tabIndentSingleSpace" $
  "exprs" ~> tabIndent @@ (newlineSep @@ var "exprs")

unsupportedType :: TTermDefinition (String -> Expr)
unsupportedType = define "unsupportedType" $
  "label" ~> cst @@ (string "[" ++ var "label" ++ string "]")

unsupportedVariant :: TTermDefinition (String -> a -> Expr)
unsupportedVariant = define "unsupportedVariant" $
  "label" ~> "obj" ~>
    cst @@ (string "[unsupported " ++ var "label" ++ string ": " ++ (Literals.showString $ var "obj") ++ string "]")

withComma :: TTermDefinition (Expr -> Expr)
withComma = define "withComma" $
  "e" ~> noSep @@ list [var "e", cst @@ string ","]

withSemi :: TTermDefinition (Expr -> Expr)
withSemi = define "withSemi" $
  "e" ~> noSep @@ list [var "e", cst @@ string ";"]
