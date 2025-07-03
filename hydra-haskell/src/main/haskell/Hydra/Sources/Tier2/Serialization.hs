{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Serialization where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants

import Hydra.Ast
import qualified Hydra.Dsl.Ast as Ast


serializationDefinition :: String -> TTerm a -> TElement a
serializationDefinition = definitionInModule hydraSerializationModule

hydraSerializationModule :: Module
hydraSerializationModule = Module (Namespace "hydra.serialization") elements
    []
    [Tier1.hydraAstModule, Tier1.hydraGraphModule] $
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

angleBracesDef :: TElement Brackets
angleBracesDef = serializationDefinition "angleBraces" $
  Ast.brackets (ref symDef @@ string "<") (ref symDef @@ string ">")

angleBracesListDef :: TElement (BlockStyle -> [Expr] -> Expr)
angleBracesListDef = serializationDefinition "angleBracesList" $
  lambdas ["style", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "<>")
      (ref bracketsDef @@ ref angleBracesDef @@ var "style" @@ (ref commaSepDef @@ var "style" @@ var "els"))

bracesListAdaptiveDef :: TElement ([Expr] -> Expr)
bracesListAdaptiveDef = serializationDefinition "bracesListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  lambda "els" $ lets [
    "inlineList">: ref curlyBracesListDef @@ nothing @@ ref inlineStyleDef @@ var "els"]
    $ Logic.ifElse (Equality.gtInt32 (ref expressionLengthDef @@ var "inlineList") (int32 70))
      (ref curlyBracesListDef @@ nothing @@ ref halfBlockStyleDef @@ var "els")
      (var "inlineList")

bracketListAdaptiveDef :: TElement ([Expr] -> Expr)
bracketListAdaptiveDef = serializationDefinition "bracketListAdaptive" $
  doc "Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width of the expression." $
  lambda "els" $ lets [
    "inlineList">: ref bracketListDef @@ ref inlineStyleDef @@ var "els"]
    $ Logic.ifElse (Equality.gtInt32 (ref expressionLengthDef @@ var "inlineList") (int32 70))
      (ref bracketListDef @@ ref halfBlockStyleDef @@ var "els")
      (var "inlineList")

bracketListDef :: TElement (BlockStyle -> [Expr] -> Expr)
bracketListDef = serializationDefinition "bracketList" $
  lambdas ["style", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "[]")
      (ref bracketsDef @@ ref squareBracketsDef @@ var "style" @@ (ref commaSepDef @@ var "style" @@ var "els"))

bracketsDef :: TElement (Brackets -> BlockStyle -> Expr -> Expr)
bracketsDef = serializationDefinition "brackets" $
  lambdas ["br", "style", "e"] $
    Ast.exprBrackets $ Ast.bracketExpr (var "br") (var "e") (var "style")

commaSepDef :: TElement (BlockStyle -> [Expr] -> Expr)
commaSepDef = serializationDefinition "commaSep" $
  ref symbolSepDef @@ string ","

cstDef :: TElement (String -> Expr)
cstDef = serializationDefinition "cst" $
  lambda "s" $ Ast.exprConst $ ref symDef @@ var "s"

curlyBlockDef :: TElement (BlockStyle -> Expr -> Expr)
curlyBlockDef = serializationDefinition "curlyBlock" $
  lambdas ["style", "e"] $
    ref curlyBracesListDef @@ nothing @@ var "style" @@ (list [var "e"])

curlyBracesDef :: TElement Brackets
curlyBracesDef = serializationDefinition "curlyBraces" $
  Ast.brackets (ref symDef @@ string "{") (ref symDef @@ string "}")

curlyBracesListDef :: TElement (Maybe String -> BlockStyle -> [Expr] -> Expr)
curlyBracesListDef = serializationDefinition "curlyBracesList" $
  lambdas ["msymb", "style", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "{}")
      (ref bracketsDef @@ ref curlyBracesDef @@ var "style" @@
        (ref symbolSepDef @@ (Optionals.fromMaybe (string ",") (var "msymb")) @@ var "style" @@ var "els"))

customIndentBlockDef :: TElement (String -> [Expr] -> Expr)
customIndentBlockDef = serializationDefinition "customIndentBlock" $
  lambdas ["idt", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equalInt32 (Lists.length $ var "els") (int32 1))
        (Lists.head $ var "els")
        (lets [
          "head">: Lists.head $ var "els",
          "rest">: Lists.tail $ var "els",
          "idtOp">: Ast.op
            (ref symDef @@ string "")
            (Ast.padding Ast.wsSpace (Ast.wsBreakAndIndent $ var "idt"))
            (Ast.precedence $ int32 0)
            Ast.associativityNone]
          $ ref ifxDef @@ var "idtOp" @@ var "head" @@ (ref newlineSepDef @@ var "rest")))

customIndentDef :: TElement (String -> String -> String)
customIndentDef = serializationDefinition "customIndent" $
  lambdas ["idt", "s"] $ Strings.cat $
    Lists.intersperse (string "\n") $ Lists.map (lambda "line" $ var "idt" ++ var "line") $ Strings.lines $ var "s"

dotSepDef :: TElement ([Expr] -> Expr)
dotSepDef = serializationDefinition "dotSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleNewlineSepDef :: TElement ([Expr] -> Expr)
doubleNewlineSepDef = serializationDefinition "doubleNewlineSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsBreak Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

doubleSpaceDef :: TElement String
doubleSpaceDef = serializationDefinition "doubleSpace" $
  string "  "

expressionLengthDef :: TElement (Expr -> Int)
expressionLengthDef = serializationDefinition "expressionLength" $
  doc "Find the approximate length (number of characters, including spaces and newlines) of an expression without actually printing it." $
  lambda "e" $ lets [
    "symbolLength">: lambda "s" $ Strings.length $ Ast.unSymbol $ var "s",
    "wsLength">: lambda "ws" $ cases _Ws (var "ws") Nothing [
      _Ws_none>>: constant $ int32 0,
      _Ws_space>>: constant $ int32 1,
      _Ws_break>>: constant $ int32 1,
      _Ws_breakAndIndent>>: lambda "s" $ Math.addInt32 (int32 1) (Strings.length $ var "s"),
      _Ws_doubleBreak>>: constant $ int32 2],
    "blockStyleLength">: lambda "style" $ lets [
      "mindentLen">: Optionals.maybe (int32 0) (unaryFunction Strings.length) (Ast.blockStyleIndent $ var "style"),
      "nlBeforeLen">: Logic.ifElse (Ast.blockStyleNewlineBeforeContent $ var "style") (int32 1) (int32 0),
      "nlAfterLen">: Logic.ifElse (Ast.blockStyleNewlineAfterContent $ var "style") (int32 1) (int32 0)]
      $ Math.addInt32 (var "mindentLen") $ Math.addInt32 (var "nlBeforeLen") (var "nlAfterLen"),
    "bracketsLength">: lambda "brackets" $
      Math.addInt32
        (var "symbolLength" @@ (Ast.bracketsOpen $ var "brackets"))
        (var "symbolLength" @@ (Ast.bracketsClose $ var "brackets")),
    "bracketExprLength">: lambda "be" $
      Math.addInt32
        (var "bracketsLength" @@ (Ast.bracketExprBrackets $ var "be"))
        (Math.addInt32
          (ref expressionLengthDef @@ (Ast.bracketExprEnclosed $ var "be"))
          (var "blockStyleLength" @@ (Ast.bracketExprStyle $ var "be"))),
    "indentedExpressionLength">: lambda "ie" $ lets [
      "baseLen">: ref expressionLengthDef @@ (Ast.indentedExpressionExpr $ var "ie"),
      "indentLen">: cases _IndentStyle (Ast.indentedExpressionStyle $ var "ie") Nothing [
        _IndentStyle_allLines>>: lambda "s" $ Strings.length $ var "s",
        _IndentStyle_subsequentLines>>: lambda "s" $ Strings.length $ var "s"]]
      $ Math.addInt32 (var "baseLen") (var "indentLen"),
    "opLength">: lambda "op" $ lets [
      "symLen">: var "symbolLength" @@ (Ast.opSymbol $ var "op"),
      "padding">: Ast.opPadding $ var "op",
      "leftLen">: var "wsLength" @@ (Ast.paddingLeft $ var "padding"),
      "rightLen">: var "wsLength" @@ (Ast.paddingRight $ var "padding")]
      $ Math.addInt32 (var "symLen") $ Math.addInt32 (var "leftLen") (var "rightLen"),
    "opExprLength">: lambda "oe" $ lets [
      "opLen">: var "opLength" @@ (Ast.opExprOp $ var "oe"),
      "leftLen">: ref expressionLengthDef @@ (Ast.opExprLhs $ var "oe"),
      "rightLen">: ref expressionLengthDef @@ (Ast.opExprRhs $ var "oe")]
      $ Math.addInt32 (var "opLen") $ Math.addInt32 (var "leftLen") (var "rightLen")]
    $ cases _Expr (var "e") Nothing [
      _Expr_const>>: lambda "s" $ var "symbolLength" @@ var "s",
      _Expr_indent>>: lambda "ie" $ var "indentedExpressionLength" @@ var "ie",
      _Expr_op>>: lambda "oe" $ var "opExprLength" @@ var "oe",
      _Expr_brackets>>: lambda "be" $ var "bracketExprLength" @@ var "be"]

fullBlockStyleDef :: TElement BlockStyle
fullBlockStyleDef = serializationDefinition "fullBlockStyle" $
  Ast.blockStyle (just $ ref doubleSpaceDef) true true

halfBlockStyleDef :: TElement BlockStyle
halfBlockStyleDef = serializationDefinition "halfBlockStyle" $
  Ast.blockStyle (just $ ref doubleSpaceDef) true false

ifxDef :: TElement (Op -> Expr -> Expr -> Expr)
ifxDef = serializationDefinition "ifx" $
  lambdas ["op", "lhs", "rhs"] $
    Ast.exprOp $ Ast.opExpr (var "op") (var "lhs") (var "rhs")

indentBlockDef :: TElement ([Expr] -> Expr)
indentBlockDef = serializationDefinition "indentBlock" $
  ref customIndentBlockDef @@ ref doubleSpaceDef

indentDef :: TElement (String -> String)
indentDef = serializationDefinition "indent" $
  ref customIndentDef @@ ref doubleSpaceDef

indentSubsequentLinesDef :: TElement (String -> Expr -> Expr)
indentSubsequentLinesDef = serializationDefinition "indentSubsequentLines" $
  lambdas ["idt", "e"] $
    Ast.exprIndent $ Ast.indentedExpression (Ast.indentStyleSubsequentLines $ var "idt") (var "e")

infixWsDef :: TElement (String -> Expr -> Expr -> Expr)
infixWsDef = serializationDefinition "infixWs" $
  lambdas ["op", "l", "r"] $
    ref spaceSepDef @@ list [var "l", ref cstDef @@ var "op", var "r"]

infixWsListDef :: TElement (String -> [Expr] -> Expr)
infixWsListDef = serializationDefinition "infixWsList" $
  lambdas ["op", "opers"] $ lets [
    "opExpr">: ref cstDef @@ var "op",
    "foldFun">: lambdas ["e", "r"] $
      Logic.ifElse (Lists.null $ var "e")
        (list [var "r"])
        (Lists.cons (var "r") $ Lists.cons (var "opExpr") (var "e"))]
    $ ref spaceSepDef @@ (Lists.foldl (var "foldFun") (list []) $ Lists.reverse $ var "opers")

inlineStyleDef :: TElement BlockStyle
inlineStyleDef = serializationDefinition "inlineStyle" $
  Ast.blockStyle nothing false false

newlineSepDef :: TElement ([Expr] -> Expr)
newlineSepDef = serializationDefinition "newlineSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsNone Ast.wsBreak)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

noPaddingDef :: TElement Padding
noPaddingDef = serializationDefinition "noPadding" $
  Ast.padding Ast.wsNone Ast.wsNone

noSepDef :: TElement ([Expr] -> Expr)
noSepDef = serializationDefinition "noSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

numDef :: TElement (Int -> Expr)
numDef = serializationDefinition "num" $
  lambda "i" $ ref cstDef @@ (Literals.showInt32 $ var "i")

opDef :: TElement (String -> Int -> Associativity -> Op)
opDef = serializationDefinition "op" $
  lambdas ["s", "p", "assoc"] $
    Ast.op
      (ref symDef @@ var "s")
      (Ast.padding Ast.wsSpace Ast.wsSpace)
      (Ast.precedence $ var "p")
      (var "assoc")

orOpDef :: TElement (Bool -> Op)
orOpDef = serializationDefinition "orOp" $
  lambda "newlines" $
    Ast.op
      (ref symDef @@ string "|")
      (Ast.padding Ast.wsSpace (Logic.ifElse (var "newlines") Ast.wsBreak Ast.wsSpace))
      (Ast.precedence $ int32 0)
      Ast.associativityNone

orSepDef :: TElement (BlockStyle -> [Expr] -> Expr)
orSepDef = serializationDefinition "orSep" $
  lambdas ["style", "l"] $
    Logic.ifElse (Lists.null $ var "l")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equalInt32 (Lists.length $ var "l") (int32 1))
        (Lists.head $ var "l")
        (lets [
          "h">: Lists.head $ var "l",
          "r">: Lists.tail $ var "l",
          "newlines">: Ast.blockStyleNewlineBeforeContent $ var "style"]
          $ ref ifxDef @@ (ref orOpDef @@ var "newlines") @@ var "h" @@ (ref orSepDef @@ var "style" @@ var "r")))

parenListDef :: TElement (Bool -> [Expr] -> Expr)
parenListDef = serializationDefinition "parenList" $
  lambdas ["newlines", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "()")
      (lets [
        "style">: Logic.ifElse (Logic.and (var "newlines") (Equality.gtInt32 (Lists.length $ var "els") (int32 1)))
          (ref halfBlockStyleDef)
          (ref inlineStyleDef)]
        $ ref bracketsDef @@ ref parenthesesDef @@ var "style" @@ (ref commaSepDef @@ var "style" @@ var "els"))

parensDef :: TElement (Expr -> Expr)
parensDef = serializationDefinition "parens" $
  ref bracketsDef @@ ref parenthesesDef @@ ref inlineStyleDef

parenthesesDef :: TElement Brackets
parenthesesDef = serializationDefinition "parentheses" $
  Ast.brackets (ref symDef @@ string "(") (ref symDef @@ string ")")

parenthesizeDef :: TElement (Expr -> Expr)
parenthesizeDef = serializationDefinition "parenthesize" $
  lambda "exp" $ lets [
    "assocLeft">: lambda "a" $ cases _Associativity (var "a") (Just true) [
      _Associativity_right>>: constant false],
    "assocRight">: lambda "a" $ cases _Associativity (var "a") (Just true) [
      _Associativity_left>>: constant false]]
    $ cases _Expr (var "exp") Nothing [
      _Expr_brackets>>: lambda "bracketExpr" $
        Ast.exprBrackets $ Ast.bracketExpr
          (Ast.bracketExprBrackets $ var "bracketExpr")
          (ref parenthesizeDef @@ (Ast.bracketExprEnclosed $ var "bracketExpr"))
          (Ast.bracketExprStyle $ var "bracketExpr"),
      _Expr_const>>: lambda "ignored" $ var "exp",
      _Expr_indent>>: lambda "indentExpr" $
        Ast.exprIndent $ Ast.indentedExpression
          (Ast.indentedExpressionStyle $ var "indentExpr")
          (ref parenthesizeDef @@ (Ast.indentedExpressionExpr $ var "indentExpr")),
      _Expr_op>>: lambda "opExpr" $ lets [
        "op">: Ast.opExprOp $ var "opExpr",
        "prec">: Ast.unPrecedence $ Ast.opPrecedence $ var "op",
        "assoc">: Ast.opAssociativity $ var "op",
        "lhs">: Ast.opExprLhs $ var "opExpr",
        "rhs">: Ast.opExprRhs $ var "opExpr",
        "lhs'">: ref parenthesizeDef @@ var "lhs",
        "rhs'">: ref parenthesizeDef @@ var "rhs",
        "lhs2">: cases _Expr (var "lhs'") (Just $ var "lhs'") [
          _Expr_op>>: lambda "lopExpr" $ lets [
            "lop">: Ast.opExprOp $ var "lopExpr",
            "lprec">: Ast.unPrecedence $ Ast.opPrecedence $ var "lop",
            "lassoc">: Ast.opAssociativity $ var "lop",
            "comparison">: Equality.compareInt32 (var "prec") (var "lprec")]
            $ cases _Comparison (var "comparison") Nothing [
              _Comparison_lessThan>>: constant $ var "lhs'",
              _Comparison_greaterThan>>: constant (ref parensDef @@ var "lhs'"),
              _Comparison_equalTo>>: constant $ Logic.ifElse
                (Logic.and (var "assocLeft" @@ var "assoc") (var "assocLeft" @@ var "lassoc"))
                (var "lhs'")
                (ref parensDef @@ var "lhs'")]],
        "rhs2">: cases _Expr (var "rhs'") (Just $ var "rhs'") [
          _Expr_op>>: lambda "ropExpr" $ lets [
            "rop">: Ast.opExprOp $ var "ropExpr",
            "rprec">: Ast.unPrecedence $ Ast.opPrecedence $ var "rop",
            "rassoc">: Ast.opAssociativity $ var "rop",
            "comparison">: Equality.compareInt32 (var "prec") (var "rprec")]
            $ cases _Comparison (var "comparison") Nothing [
              _Comparison_lessThan>>: constant $ var "rhs'",
              _Comparison_greaterThan>>: constant (ref parensDef @@ var "rhs'"),
              _Comparison_equalTo>>: constant $ Logic.ifElse
                (Logic.and (var "assocRight" @@ var "assoc") (var "assocRight" @@ var "rassoc"))
                (var "rhs'")
                (ref parensDef @@ var "rhs'")]]]
        $ Ast.exprOp $ Ast.opExpr (var "op") (var "lhs2") (var "rhs2")]

prefixDef :: TElement (String -> Expr -> Expr)
prefixDef = serializationDefinition "prefix" $
  lambdas ["p", "expr"] $ lets [
    "preOp">: Ast.op
      (ref symDef @@ var "p")
      (Ast.padding Ast.wsNone Ast.wsNone)
      (Ast.precedence $ int32 0)
      Ast.associativityNone]
    $ ref ifxDef @@ var "preOp" @@ (ref cstDef @@ string "") @@ var "expr"

printExprDef :: TElement (Expr -> String)
printExprDef = serializationDefinition "printExpr" $
  lambda "e" $ lets [
    "pad">: lambda "ws" $ cases _Ws (var "ws") Nothing [
      _Ws_none>>: constant $ string "",
      _Ws_space>>: constant $ string " ",
      _Ws_break>>: constant $ string "\n",
      _Ws_breakAndIndent>>: lambda "ignored" $ string "\n",
      _Ws_doubleBreak>>: constant $ string "\n\n"],
    "idt">: lambdas ["ws", "s"] $ cases _Ws (var "ws") (Just $ var "s") [
      _Ws_breakAndIndent>>: lambda "indentStr" $ ref customIndentDef @@ var "indentStr" @@ var "s"]]
    $ cases _Expr (var "e") Nothing [
      _Expr_const>>: lambda "symbol" $ Ast.unSymbol $ var "symbol",
      _Expr_indent>>: lambda "indentExpr" $ lets [
        "style">: Ast.indentedExpressionStyle $ var "indentExpr",
        "expr">: Ast.indentedExpressionExpr $ var "indentExpr",
        "lns">: Strings.lines $ ref printExprDef @@ var "expr"]
        $ Strings.intercalate (string "\n") $ cases _IndentStyle (var "style") Nothing [
          _IndentStyle_allLines>>: lambda "idt" $ Lists.map (lambda "line" $ var "idt" ++ var "line") (var "lns"),
          _IndentStyle_subsequentLines>>: lambda "idt" $
            Logic.ifElse (Equality.equalInt32 (Lists.length $ var "lns") (int32 1))
              (var "lns")
              (Lists.cons (Lists.head $ var "lns") $ Lists.map (lambda "line" $ var "idt" ++ var "line") $ Lists.tail $ var "lns")],
      _Expr_op>>: lambda "opExpr" $ lets [
        "op">: Ast.opExprOp $ var "opExpr",
        "sym">: Ast.unSymbol $ Ast.opSymbol $ var "op",
        "padding">: Ast.opPadding $ var "op",
        "padl">: Ast.paddingLeft $ var "padding",
        "padr">: Ast.paddingRight $ var "padding",
        "l">: Ast.opExprLhs $ var "opExpr",
        "r">: Ast.opExprRhs $ var "opExpr",
        "lhs">: var "idt" @@ var "padl" @@ (ref printExprDef @@ var "l"),
        "rhs">: var "idt" @@ var "padr" @@ (ref printExprDef @@ var "r")]
        $ var "lhs" ++ (var "pad" @@ var "padl") ++ var "sym" ++ (var "pad" @@ var "padr") ++ var "rhs",
      _Expr_brackets>>: lambda "bracketExpr" $ lets [
        "brackets">: Ast.bracketExprBrackets $ var "bracketExpr",
        "l">: Ast.unSymbol $ Ast.bracketsOpen $ var "brackets",
        "r">: Ast.unSymbol $ Ast.bracketsClose $ var "brackets",
        "e">: Ast.bracketExprEnclosed $ var "bracketExpr",
        "style">: Ast.bracketExprStyle $ var "bracketExpr",
        "body">: ref printExprDef @@ var "e",
        "doIndent">: Ast.blockStyleIndent $ var "style",
        "nlBefore">: Ast.blockStyleNewlineBeforeContent $ var "style",
        "nlAfter">: Ast.blockStyleNewlineAfterContent $ var "style",
        "ibody">: Optionals.maybe (var "body") (lambda "idt" $ ref customIndentDef @@ var "idt" @@ var "body") (var "doIndent"),
        "pre">: Logic.ifElse (var "nlBefore") (string "\n") (string ""),
        "suf">: Logic.ifElse (var "nlAfter") (string "\n") (string "")]
        $ var "l" ++ var "pre" ++ var "ibody" ++ var "suf" ++ var "r"]

semicolonSepDef :: TElement ([Expr] -> Expr)
semicolonSepDef = serializationDefinition "semicolonSep" $
  ref symbolSepDef @@ string ";" @@ ref inlineStyleDef

sepDef :: TElement (Op -> [Expr] -> Expr)
sepDef = serializationDefinition "sep" $
  lambdas ["op", "els"] $
    Logic.ifElse (Lists.null $ var "els")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equalInt32 (Lists.length $ var "els") (int32 1))
        (Lists.head $ var "els")
        (lets [
          "h">: Lists.head $ var "els",
          "r">: Lists.tail $ var "els"]
          $ ref ifxDef @@ var "op" @@ var "h" @@ (ref sepDef @@ var "op" @@ var "r")))

spaceSepDef :: TElement ([Expr] -> Expr)
spaceSepDef = serializationDefinition "spaceSep" $
  ref sepDef @@ (Ast.op
    (ref symDef @@ string "")
    (Ast.padding Ast.wsSpace Ast.wsNone)
    (Ast.precedence $ int32 0)
    Ast.associativityNone)

squareBracketsDef :: TElement Brackets
squareBracketsDef = serializationDefinition "squareBrackets" $
  Ast.brackets (ref symDef @@ string "[") (ref symDef @@ string "]")

symDef :: TElement (String -> Symbol)
symDef = serializationDefinition "sym" $
  lambda "s" $ Ast.symbol $ var "s"

symbolSepDef :: TElement (String -> BlockStyle -> [Expr] -> Expr)
symbolSepDef = serializationDefinition "symbolSep" $
  lambdas ["symb", "style", "l"] $
    Logic.ifElse (Lists.null $ var "l")
      (ref cstDef @@ string "")
      (Logic.ifElse (Equality.equalInt32 (Lists.length $ var "l") (int32 1))
        (Lists.head $ var "l")
        (lets [
          "h">: Lists.head $ var "l",
          "r">: Lists.tail $ var "l",
          "breakCount">: Lists.length $ Lists.filter identity $ list [
            Ast.blockStyleNewlineBeforeContent $ var "style",
            Ast.blockStyleNewlineAfterContent $ var "style"],
          "break">: Logic.ifElse (Equality.equalInt32 (var "breakCount") (int32 0))
            Ast.wsSpace
            (Logic.ifElse (Equality.equalInt32 (var "breakCount") (int32 1))
              Ast.wsBreak
              Ast.wsDoubleBreak),
          "commaOp">: Ast.op
            (ref symDef @@ var "symb")
            (Ast.padding Ast.wsNone (var "break"))
            (Ast.precedence $ int32 0)
            Ast.associativityNone]
          $ ref ifxDef @@ var "commaOp" @@ var "h" @@ (ref symbolSepDef @@ var "symb" @@ var "style" @@ var "r")))

tabIndentDef :: TElement (Expr -> Expr)
tabIndentDef = serializationDefinition "tabIndent" $
  lambda "e" $ Ast.exprIndent $ Ast.indentedExpression
    (Ast.indentStyleAllLines $ string "    ")
    (var "e")

tabIndentDoubleSpaceDef :: TElement ([Expr] -> Expr)
tabIndentDoubleSpaceDef = serializationDefinition "tabIndentDoubleSpace" $
  lambda "exprs" $ ref tabIndentDef @@ (ref doubleNewlineSepDef @@ var "exprs")

tabIndentSingleSpaceDef :: TElement ([Expr] -> Expr)
tabIndentSingleSpaceDef = serializationDefinition "tabIndentSingleSpace" $
  lambda "exprs" $ ref tabIndentDef @@ (ref newlineSepDef @@ var "exprs")

unsupportedTypeDef :: TElement (String -> Expr)
unsupportedTypeDef = serializationDefinition "unsupportedType" $
  lambda "label" $ ref cstDef @@ ("[" ++ var "label" ++ "]")

unsupportedVariantDef :: TElement (String -> a -> Expr)
unsupportedVariantDef = serializationDefinition "unsupportedVariant" $
  lambdas ["label", "obj"] $ ref cstDef @@
    ("[unsupported " ++ var "label" ++ ": " ++ (Literals.showString $ var "obj") ++ "]")

withCommaDef :: TElement (Expr -> Expr)
withCommaDef = serializationDefinition "withComma" $
  lambda "e" $ ref noSepDef @@ list [var "e", ref cstDef @@ string ","]

withSemiDef :: TElement (Expr -> Expr)
withSemiDef = serializationDefinition "withSemi" $
  lambda "e" $ ref noSepDef @@ list [var "e", ref cstDef @@ string ";"]
