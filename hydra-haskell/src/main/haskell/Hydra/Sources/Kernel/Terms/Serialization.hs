{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Serialization where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import Hydra.Ast


module_ :: Module
module_ = Module (Namespace "hydra.serialization") elements
    []
    kernelTypesModules $
    Just ("Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.")
  where
   elements = [
     el angleBracesDef,
     el angleBracesListDef,
     el bracesListAdaptiveDef,
     el bracketListDef,
     el bracketListAdaptiveDef,
     el bracketsDef,
     el commaSepDef,
     el curlyBlockDef,
     el curlyBracesDef,
     el curlyBracesListDef,
     el cstDef,
     el customIndentDef,
     el customIndentBlockDef,
     el dotSepDef,
     el doubleNewlineSepDef,
     el doubleSpaceDef,
     el expressionLengthDef,
     el fullBlockStyleDef,
     el halfBlockStyleDef,
     el ifxDef,
     el indentDef,
     el indentBlockDef,
     el indentSubsequentLinesDef,
     el infixWsDef,
     el infixWsListDef,
     el inlineStyleDef,
     el newlineSepDef,
     el noPaddingDef,
     el noSepDef,
     el numDef,
     el opDef,
     el orOpDef,
     el orSepDef,
     el parenListDef,
     el parensDef,
     el parenthesesDef,
     el parenthesizeDef,
     el prefixDef,
     el printExprDef,
     el semicolonSepDef,
     el sepDef,
     el spaceSepDef,
     el squareBracketsDef,
     el symDef,
     el symbolSepDef,
     el tabIndentDef,
     el tabIndentDoubleSpaceDef,
     el tabIndentSingleSpaceDef,
     el unsupportedTypeDef,
     el unsupportedVariantDef,
     el withCommaDef,
     el withSemiDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

angleBracesDef :: TBinding Brackets
angleBracesDef = define "angleBraces" $
  Ast.brackets (ref symDef @@ string "<") (ref symDef @@ string ">")

angleBracesListDef :: TBinding (BlockStyle -> [Expr] -> Expr)
angleBracesListDef = define "angleBracesList" $
  "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "<>")
      (ref bracketsDef @@ ref angleBracesDef @@ var "style" @@ (ref commaSepDef @@ var "style" @@ var "els"))

bracesListAdaptiveDef :: TBinding ([Expr] -> Expr)
bracesListAdaptiveDef = define "bracesListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  "els" ~>
  "inlineList" <~ ref curlyBracesListDef @@ nothing @@ ref inlineStyleDef @@ var "els" $
  Logic.ifElse (Equality.gt (ref expressionLengthDef @@ var "inlineList") (int32 70))
    (ref curlyBracesListDef @@ nothing @@ ref halfBlockStyleDef @@ var "els")
    (var "inlineList")

bracketListAdaptiveDef :: TBinding ([Expr] -> Expr)
bracketListAdaptiveDef = define "bracketListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  "els" ~>
  "inlineList" <~ ref bracketListDef @@ ref inlineStyleDef @@ var "els" $
  Logic.ifElse (Equality.gt (ref expressionLengthDef @@ var "inlineList") (int32 70))
    (ref bracketListDef @@ ref halfBlockStyleDef @@ var "els")
    (var "inlineList")

bracketListDef :: TBinding (BlockStyle -> [Expr] -> Expr)
bracketListDef = define "bracketList" $
  "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "[]")
      (ref bracketsDef @@ ref squareBracketsDef @@ var "style" @@ (ref commaSepDef @@ var "style" @@ var "els"))

bracketsDef :: TBinding (Brackets -> BlockStyle -> Expr -> Expr)
bracketsDef = define "brackets" $
  "br" ~> "style" ~> "e" ~>
    Ast.exprBrackets $ Ast.bracketExpr (var "br") (var "e") (var "style")

commaSepDef :: TBinding (BlockStyle -> [Expr] -> Expr)
commaSepDef = define "commaSep" $
  ref symbolSepDef @@ string ","

cstDef :: TBinding (String -> Expr)
cstDef = define "cst" $
  "s" ~> Ast.exprConst $ ref symDef @@ var "s"

curlyBlockDef :: TBinding (BlockStyle -> Expr -> Expr)
curlyBlockDef = define "curlyBlock" $
  "style" ~> "e" ~>
    ref curlyBracesListDef @@ nothing @@ var "style" @@ (list [var "e"])

curlyBracesDef :: TBinding Brackets
curlyBracesDef = define "curlyBraces" $
  Ast.brackets (ref symDef @@ string "{") (ref symDef @@ string "}")

curlyBracesListDef :: TBinding (Maybe String -> BlockStyle -> [Expr] -> Expr)
curlyBracesListDef = define "curlyBracesList" $
  "msymb" ~> "style" ~> "els" ~>
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "{}")
      (ref bracketsDef @@ ref curlyBracesDef @@ var "style" @@
        (ref symbolSepDef @@ (Maybes.fromMaybe (string ",") (var "msymb")) @@ var "style" @@ var "els"))

customIndentBlockDef :: TBinding (String -> [Expr] -> Expr)
customIndentBlockDef = define "customIndentBlock" $
  "idt" ~> "els" ~>
    "head" <~ Lists.head (var "els") $
    "rest" <~ Lists.tail (var "els") $
    "idtOp" <~ (Ast.op
      (ref symDef @@ string "")
      (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ var "idt"))
      (Ast.precedence $ int32 0)
      Ast.associativityNone) $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equal (Lists.length $ var "els") (int32 1))
        (Lists.head $ var "els")
        (ref ifxDef @@ var "idtOp" @@ var "head" @@ (ref newlineSepDef @@ var "rest")))

customIndentDef :: TBinding (String -> String -> String)
customIndentDef = define "customIndent" $
  "idt" ~> "s" ~> Strings.cat $
    Lists.intersperse (string "\n") $ Lists.map ("line" ~> var "idt" ++ var "line") $ Strings.lines $ var "s"

dotSepDef :: TBinding ([Expr] -> Expr)
dotSepDef = define "dotSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleNewlineSepDef :: TBinding ([Expr] -> Expr)
doubleNewlineSepDef = define "doubleNewlineSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsBreak Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleSpaceDef :: TBinding String
doubleSpaceDef = define "doubleSpace" $
  string "  "

expressionLengthDef :: TBinding (Expr -> Int)
expressionLengthDef = define "expressionLength" $
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
        (ref expressionLengthDef @@ (Ast.bracketExprEnclosed $ var "be"))
        (var "blockStyleLength" @@ (Ast.bracketExprStyle $ var "be")))) $
  "indentedExpressionLength" <~ ("ie" ~>
    "baseLen" <~ ref expressionLengthDef @@ (Ast.indentedExpressionExpr $ var "ie") $
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
    "leftLen" <~ ref expressionLengthDef @@ (Ast.opExprLhs $ var "oe") $
    "rightLen" <~ ref expressionLengthDef @@ (Ast.opExprRhs $ var "oe") $
    Math.add (var "opLen") $ Math.add (var "leftLen") (var "rightLen")) $
  cases _Expr (var "e") Nothing [
    _Expr_const>>: "s" ~> var "symbolLength" @@ var "s",
    _Expr_indent>>: "ie" ~> var "indentedExpressionLength" @@ var "ie",
    _Expr_op>>: "oe" ~> var "opExprLength" @@ var "oe",
    _Expr_brackets>>: "be" ~> var "bracketExprLength" @@ var "be"]

fullBlockStyleDef :: TBinding BlockStyle
fullBlockStyleDef = define "fullBlockStyle" $
  Ast.blockStyle (just $ ref doubleSpaceDef) true true

halfBlockStyleDef :: TBinding BlockStyle
halfBlockStyleDef = define "halfBlockStyle" $
  Ast.blockStyle (just $ ref doubleSpaceDef) true false

ifxDef :: TBinding (Op -> Expr -> Expr -> Expr)
ifxDef = define "ifx" $
  "op" ~> "lhs" ~> "rhs" ~>
    Ast.exprOp $ Ast.opExpr (var "op") (var "lhs") (var "rhs")

indentBlockDef :: TBinding ([Expr] -> Expr)
indentBlockDef = define "indentBlock" $
  ref customIndentBlockDef @@ ref doubleSpaceDef

indentDef :: TBinding (String -> String)
indentDef = define "indent" $
  ref customIndentDef @@ ref doubleSpaceDef

indentSubsequentLinesDef :: TBinding (String -> Expr -> Expr)
indentSubsequentLinesDef = define "indentSubsequentLines" $
  "idt" ~> "e" ~>
    Ast.exprIndent $ Ast.indentedExpression (Ast.indentStyleSubsequentLines $ var "idt") (var "e")

infixWsDef :: TBinding (String -> Expr -> Expr -> Expr)
infixWsDef = define "infixWs" $
  "op" ~> "l" ~> "r" ~>
    ref spaceSepDef @@ list [var "l", ref cstDef @@ var "op", var "r"]

infixWsListDef :: TBinding (String -> [Expr] -> Expr)
infixWsListDef = define "infixWsList" $
  "op" ~> "opers" ~>
  "opExpr" <~ ref cstDef @@ var "op" $
  "foldFun" <~ ("e" ~> "r" ~>
    Logic.ifElse (Lists.null $ var "e")
      (list [var "r"])
      (Lists.cons (var "r") $ Lists.cons (var "opExpr") (var "e"))) $
  ref spaceSepDef @@ (Lists.foldl (var "foldFun") (list []) $ Lists.reverse $ var "opers")

inlineStyleDef :: TBinding BlockStyle
inlineStyleDef = define "inlineStyle" $
  Ast.blockStyle nothing false false

newlineSepDef :: TBinding ([Expr] -> Expr)
newlineSepDef = define "newlineSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsNone Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

noPaddingDef :: TBinding Padding
noPaddingDef = define "noPadding" $
  Ast.padding Ast.wsNone Ast.wsNone

noSepDef :: TBinding ([Expr] -> Expr)
noSepDef = define "noSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

numDef :: TBinding (Int -> Expr)
numDef = define "num" $
  "i" ~> ref cstDef @@ (Literals.showInt32 $ var "i")

opDef :: TBinding (String -> Int -> Associativity -> Op)
opDef = define "op" $
  "s" ~> "p" ~> "assoc" ~>
    Ast.op
      (ref symDef @@ var "s")
      (Ast.padding Ast.wsSpace Ast.wsSpace)
      (Ast.precedence $ var "p")
      (var "assoc")

orOpDef :: TBinding (Bool -> Op)
orOpDef = define "orOp" $
  "newlines" ~> Ast.op
    (ref symDef @@ string "|")
    (Ast.padding Ast.wsSpace (Logic.ifElse (var "newlines") Ast.wsBreak Ast.wsSpace))
    (Ast.precedence $ int32 0)
    Ast.associativityNone

orSepDef :: TBinding (BlockStyle -> [Expr] -> Expr)
orSepDef = define "orSep" $
  "style" ~> "l" ~>
    "h" <~ Lists.head (var "l") $
    "r" <~ Lists.tail (var "l") $
    "newlines" <~ Ast.blockStyleNewlineBeforeContent (var "style") $
    Logic.ifElse (Lists.null $ var "l")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equal (Lists.length $ var "l") (int32 1))
        (Lists.head $ var "l")
        (ref ifxDef @@ (ref orOpDef @@ var "newlines") @@ var "h" @@ (ref orSepDef @@ var "style" @@ var "r")))

parenListDef :: TBinding (Bool -> [Expr] -> Expr)
parenListDef = define "parenList" $
  "newlines" ~> "els" ~>
    "style" <~ (Logic.ifElse (Logic.and (var "newlines") (Equality.gt (Lists.length $ var "els") (int32 1)))
      (ref halfBlockStyleDef)
      (ref inlineStyleDef)) $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "()")
      (ref bracketsDef @@ ref parenthesesDef @@ var "style" @@ (ref commaSepDef @@ var "style" @@ var "els"))

parensDef :: TBinding (Expr -> Expr)
parensDef = define "parens" $
  ref bracketsDef @@ ref parenthesesDef @@ ref inlineStyleDef

parenthesesDef :: TBinding Brackets
parenthesesDef = define "parentheses" $
  Ast.brackets (ref symDef @@ string "(") (ref symDef @@ string ")")

parenthesizeDef :: TBinding (Expr -> Expr)
parenthesizeDef = define "parenthesize" $
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
          (ref parenthesizeDef @@ (Ast.bracketExprEnclosed $ var "bracketExpr"))
          (Ast.bracketExprStyle $ var "bracketExpr"),
      _Expr_const>>: "ignored" ~> var "exp",
      _Expr_indent>>: "indentExpr" ~>
        Ast.exprIndent $ Ast.indentedExpression
          (Ast.indentedExpressionStyle $ var "indentExpr")
          (ref parenthesizeDef @@ (Ast.indentedExpressionExpr $ var "indentExpr")),
      _Expr_op>>: "opExpr" ~>
        "op" <~ Ast.opExprOp (var "opExpr") $
        "prec" <~ Ast.unPrecedence (Ast.opPrecedence $ var "op") $
        "assoc" <~ Ast.opAssociativity (var "op") $
        "lhs" <~ Ast.opExprLhs (var "opExpr") $
        "rhs" <~ Ast.opExprRhs (var "opExpr") $
        "lhs'" <~ ref parenthesizeDef @@ var "lhs" $
        "rhs'" <~ ref parenthesizeDef @@ var "rhs" $
        "lhs2" <~ cases _Expr (var "lhs'")
          (Just $ var "lhs'") [
          _Expr_op>>: "lopExpr" ~>
            "lop" <~ Ast.opExprOp (var "lopExpr") $
            "lprec" <~ Ast.unPrecedence (Ast.opPrecedence $ var "lop") $
            "lassoc" <~ Ast.opAssociativity (var "lop") $
            "comparison" <~ Equality.compare (var "prec") (var "lprec") $
            cases _Comparison (var "comparison") Nothing [
              _Comparison_lessThan>>: constant $ var "lhs'",
              _Comparison_greaterThan>>: constant (ref parensDef @@ var "lhs'"),
              _Comparison_equalTo>>: constant $ Logic.ifElse
                (Logic.and (var "assocLeft" @@ var "assoc") (var "assocLeft" @@ var "lassoc"))
                (var "lhs'")
                (ref parensDef @@ var "lhs'")]] $
        "rhs2" <~ cases _Expr (var "rhs'") (Just $ var "rhs'") [
          _Expr_op>>: "ropExpr" ~>
            "rop" <~ Ast.opExprOp (var "ropExpr") $
            "rprec" <~ Ast.unPrecedence (Ast.opPrecedence $ var "rop") $
            "rassoc" <~ Ast.opAssociativity (var "rop") $
            "comparison" <~ Equality.compare (var "prec") (var "rprec") $
            cases _Comparison (var "comparison") Nothing [
              _Comparison_lessThan>>: constant $ var "rhs'",
              _Comparison_greaterThan>>: constant (ref parensDef @@ var "rhs'"),
              _Comparison_equalTo>>: constant $ Logic.ifElse
                (Logic.and (var "assocRight" @@ var "assoc") (var "assocRight" @@ var "rassoc"))
                (var "rhs'")
                (ref parensDef @@ var "rhs'")]] $
        Ast.exprOp $ Ast.opExpr (var "op") (var "lhs2") (var "rhs2")]

prefixDef :: TBinding (String -> Expr -> Expr)
prefixDef = define "prefix" $
  "p" ~> "expr" ~>
  "preOp" <~ Ast.op
    (ref symDef @@ var "p")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone $
  ref ifxDef @@ var "preOp" @@ (ref cstDef @@ string "") @@ var "expr"

printExprDef :: TBinding (Expr -> String)
printExprDef = define "printExpr" $
  "e" ~>
  "pad" <~ ("ws" ~> cases _Ws (var "ws") Nothing [
    _Ws_none>>: constant $ string "",
    _Ws_space>>: constant $ string " ",
    _Ws_break>>: constant $ string "\n",
    _Ws_breakAndIndent>>: "ignored" ~> string "\n",
    _Ws_doubleBreak>>: constant $ string "\n\n"]) $
  "idt" <~ ("ws" ~> "s" ~> cases _Ws (var "ws") (Just $ var "s") [
    _Ws_breakAndIndent>>: "indentStr" ~> ref customIndentDef @@ var "indentStr" @@ var "s"]) $
  cases _Expr (var "e") Nothing [
    _Expr_const>>: "symbol" ~> Ast.unSymbol $ var "symbol",
    _Expr_indent>>: "indentExpr" ~>
      "style" <~ Ast.indentedExpressionStyle (var "indentExpr") $
      "expr" <~ Ast.indentedExpressionExpr (var "indentExpr") $
      "lns" <~ Strings.lines (ref printExprDef @@ var "expr") $
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
      "lhs" <~ var "idt" @@ var "padl" @@ (ref printExprDef @@ var "l") $
      "rhs" <~ var "idt" @@ var "padr" @@ (ref printExprDef @@ var "r") $
      var "lhs" ++ (var "pad" @@ var "padl") ++ var "sym" ++ (var "pad" @@ var "padr") ++ var "rhs",
    _Expr_brackets>>: "bracketExpr" ~>
      "brackets" <~ Ast.bracketExprBrackets (var "bracketExpr") $
      "l" <~ Ast.unSymbol (Ast.bracketsOpen $ var "brackets") $
      "r" <~ Ast.unSymbol (Ast.bracketsClose $ var "brackets") $
      "e" <~ Ast.bracketExprEnclosed (var "bracketExpr") $
      "style" <~ Ast.bracketExprStyle (var "bracketExpr") $
      "body" <~ ref printExprDef @@ var "e" $
      "doIndent" <~ Ast.blockStyleIndent (var "style") $
      "nlBefore" <~ Ast.blockStyleNewlineBeforeContent (var "style") $
      "nlAfter" <~ Ast.blockStyleNewlineAfterContent (var "style") $
      "ibody" <~ Maybes.maybe (var "body") ("idt" ~> ref customIndentDef @@ var "idt" @@ var "body") (var "doIndent") $
      "pre" <~ Logic.ifElse (var "nlBefore") (string "\n") (string "") $
      "suf" <~ Logic.ifElse (var "nlAfter") (string "\n") (string "") $
      var "l" ++ var "pre" ++ var "ibody" ++ var "suf" ++ var "r"]

semicolonSepDef :: TBinding ([Expr] -> Expr)
semicolonSepDef = define "semicolonSep" $
  ref symbolSepDef @@ string ";" @@ ref inlineStyleDef

sepDef :: TBinding (Op -> [Expr] -> Expr)
sepDef = define "sep" $
  "op" ~> "els" ~>
    "h" <~ Lists.head (var "els") $
    "r" <~ Lists.tail (var "els") $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equal (Lists.length $ var "els") (int32 1))
        (Lists.head $ var "els")
        (ref ifxDef @@ var "op" @@ var "h" @@ (ref sepDef @@ var "op" @@ var "r")))

spaceSepDef :: TBinding ([Expr] -> Expr)
spaceSepDef = define "spaceSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsSpace Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

squareBracketsDef :: TBinding Brackets
squareBracketsDef = define "squareBrackets" $
  Ast.brackets (ref symDef @@ string "[") (ref symDef @@ string "]")

symDef :: TBinding (String -> Symbol)
symDef = define "sym" $
  "s" ~> Ast.symbol (var "s")

symbolSepDef :: TBinding (String -> BlockStyle -> [Expr] -> Expr)
symbolSepDef = define "symbolSep" $
  "symb" ~> "style" ~> "l" ~>
    "h" <~  Lists.head (var "l") $
    "r" <~ Lists.tail (var "l") $
    "breakCount" <~ (Lists.length $ Lists.filter identity $ list [
      Ast.blockStyleNewlineBeforeContent $ var "style",
      Ast.blockStyleNewlineAfterContent $ var "style"]) $
    "break" <~ (Logic.ifElse (Equality.equal (var "breakCount") (int32 0))
      Ast.wsSpace
      (Logic.ifElse (Equality.equal (var "breakCount") (int32 1))
        Ast.wsBreak
        Ast.wsDoubleBreak)) $
    "commaOp" <~ (Ast.op
      (ref symDef @@ var "symb")
      (Ast.padding Ast.wsNone (var "break"))
      (Ast.precedence $ int32 0)
      Ast.associativityNone) $
    Logic.ifElse (Lists.null $ var "l")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equal (Lists.length $ var "l") (int32 1))
        (Lists.head $ var "l")
        (ref ifxDef @@ var "commaOp" @@ var "h" @@ (ref symbolSepDef @@ var "symb" @@ var "style" @@ var "r")))

tabIndentDef :: TBinding (Expr -> Expr)
tabIndentDef = define "tabIndent" $
  "e" ~> Ast.exprIndent $ Ast.indentedExpression
    (Ast.indentStyleAllLines $ string "    ")
    (var "e")

tabIndentDoubleSpaceDef :: TBinding ([Expr] -> Expr)
tabIndentDoubleSpaceDef = define "tabIndentDoubleSpace" $
  "exprs" ~> ref tabIndentDef @@ (ref doubleNewlineSepDef @@ var "exprs")

tabIndentSingleSpaceDef :: TBinding ([Expr] -> Expr)
tabIndentSingleSpaceDef = define "tabIndentSingleSpace" $
  "exprs" ~> ref tabIndentDef @@ (ref newlineSepDef @@ var "exprs")

unsupportedTypeDef :: TBinding (String -> Expr)
unsupportedTypeDef = define "unsupportedType" $
  "label" ~> ref cstDef @@ ("[" ++ var "label" ++ "]")

unsupportedVariantDef :: TBinding (String -> a -> Expr)
unsupportedVariantDef = define "unsupportedVariant" $
  "label" ~> "obj" ~>
    ref cstDef @@ ("[unsupported " ++ var "label" ++ ": " ++ (Literals.showString $ var "obj") ++ "]")

withCommaDef :: TBinding (Expr -> Expr)
withCommaDef = define "withComma" $
  "e" ~> ref noSepDef @@ list [var "e", ref cstDef @@ string ","]

withSemiDef :: TBinding (Expr -> Expr)
withSemiDef = define "withSemi" $
  "e" ~> ref noSepDef @@ list [var "e", ref cstDef @@ string ";"]
