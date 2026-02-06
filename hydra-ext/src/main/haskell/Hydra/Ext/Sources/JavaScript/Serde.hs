-- | Serialization functions for JavaScript AST to abstract syntax expressions.
--
-- This module provides functions to convert JavaScript AST types to Hydra's
-- abstract Expr type, which can then be rendered to concrete syntax.

module Hydra.Ext.Sources.JavaScript.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Ext.JavaScript.Syntax as JS
import qualified Hydra.Ext.Sources.JavaScript.Syntax as JavaScriptSyntax
import qualified Hydra.Ext.Sources.JavaScript.Operators as JavaScriptOperators


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.javaScript.serde"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, Serialization.ns, JavaScriptOperators.ns]
    (JavaScriptSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting JavaScript AST to abstract expressions"
  where
    elements = [
      -- Core conversions
      toBinding identifierToExpr,
      toBinding literalToExpr,
      toBinding stringLiteralToExpr,
      toBinding numericLiteralToExpr,

      -- Expression conversions
      toBinding expressionToExpr,
      toBinding arrayExpressionToExpr,
      toBinding objectExpressionToExpr,
      toBinding propertyToExpr,
      toBinding functionExpressionToExpr,
      toBinding arrowFunctionExpressionToExpr,
      toBinding callExpressionToExpr,
      toBinding memberExpressionToExpr,
      toBinding conditionalExpressionToExpr,
      toBinding binaryExpressionToExpr,
      toBinding unaryExpressionToExpr,

      -- Pattern conversions
      toBinding patternToExpr,

      -- Statement conversions
      toBinding statementToExpr,
      toBinding blockStatementToExpr,
      toBinding variableDeclarationToExpr,
      toBinding variableDeclaratorToExpr,
      toBinding variableKindToExpr,
      toBinding ifStatementToExpr,
      toBinding switchStatementToExpr,
      toBinding switchCaseToExpr,
      toBinding returnStatementToExpr,
      toBinding throwStatementToExpr,
      toBinding tryStatementToExpr,

      -- Declaration conversions
      toBinding functionDeclarationToExpr,
      toBinding classDeclarationToExpr,
      toBinding methodDefinitionToExpr,

      -- Module conversions
      toBinding programToExpr,
      toBinding moduleItemToExpr,
      toBinding importDeclarationToExpr,
      toBinding exportDeclarationToExpr,

      -- Operators
      toBinding binaryOperatorToExpr,
      toBinding unaryOperatorToString,
      toBinding assignmentOperatorToString,

      -- Comments
      toBinding documentationCommentToExpr,
      toBinding toJavaScriptComments,
      toBinding toLineComment,

      -- With comments variants
      toBinding moduleItemWithCommentsToExpr,
      toBinding functionDeclarationWithCommentsToExpr,
      toBinding classDeclarationWithCommentsToExpr]


-- ============================================================================
-- Core Conversions
-- ============================================================================

identifierToExpr :: TBinding (JS.Identifier -> Expr)
identifierToExpr = define "identifierToExpr" $
  doc "Convert an identifier to an AST expression" $
  lambda "id" $ Serialization.cst @@ (unwrap JS._Identifier @@ var "id")

literalToExpr :: TBinding (JS.Literal -> Expr)
literalToExpr = define "literalToExpr" $
  doc "Convert a literal to an AST expression" $
  lambda "lit" $
    cases JS._Literal (var "lit") Nothing [
      JS._Literal_string>>: lambda "s" $ stringLiteralToExpr @@ var "s",
      JS._Literal_number>>: lambda "n" $ numericLiteralToExpr @@ var "n",
      JS._Literal_boolean>>: lambda "b" $
        Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false")),
      JS._Literal_null>>: constant $ Serialization.cst @@ string "null",
      JS._Literal_undefined>>: constant $ Serialization.cst @@ string "undefined",
      JS._Literal_bigInt>>: lambda "n" $
        Serialization.cst @@ (Strings.cat2 (Literals.showBigint $ var "n") (string "n")),
      JS._Literal_template>>: lambda "t" $ templateLiteralToExpr @@ var "t"]

stringLiteralToExpr :: TBinding (JS.StringLiteral -> Expr)
stringLiteralToExpr = define "stringLiteralToExpr" $
  doc "Convert a string literal to an AST expression" $
  lambda "s" $ lets [
    "value">: project JS._StringLiteral JS._StringLiteral_value @@ var "s",
    "singleQuote">: project JS._StringLiteral JS._StringLiteral_singleQuote @@ var "s",
    "quote">: Logic.ifElse (var "singleQuote") (string "'") (string "\""),
    "escaped">: escapeString @@ var "value" @@ var "singleQuote"] $
    Serialization.cst @@ (Strings.cat $ list [var "quote", var "escaped", var "quote"])

escapeString :: TBinding (String -> Bool -> String)
escapeString = define "escapeString" $
  doc "Escape special characters in a string for JavaScript" $
  lambda "s" $ lambda "singleQuote" $
    -- Simple implementation: escape backslashes, quotes, and newlines
    -- A full implementation would handle more escape sequences
    var "s"  -- TODO: implement proper escaping

templateLiteralToExpr :: TBinding (JS.TemplateLiteral -> Expr)
templateLiteralToExpr = define "templateLiteralToExpr" $
  doc "Convert a template literal to an AST expression" $
  lambda "t" $ lets [
    "quasis">: project JS._TemplateLiteral JS._TemplateLiteral_quasis @@ var "t",
    "exprs">: project JS._TemplateLiteral JS._TemplateLiteral_expressions @@ var "t"] $
    -- For now, just output the first quasi if no expressions
    -- Full implementation would interleave quasis and expressions
    Serialization.cst @@ (Strings.cat $ list [
      string "`",
      Strings.intercalate (string "") (Lists.map (lambda "q" $
        project JS._TemplateElement JS._TemplateElement_value @@ var "q") (var "quasis")),
      string "`"])

numericLiteralToExpr :: TBinding (JS.NumericLiteral -> Expr)
numericLiteralToExpr = define "numericLiteralToExpr" $
  doc "Convert a numeric literal to an AST expression" $
  lambda "n" $
    cases JS._NumericLiteral (var "n") Nothing [
      JS._NumericLiteral_integer>>: lambda "i" $
        Serialization.cst @@ (Literals.showInt64 $ var "i"),
      JS._NumericLiteral_float>>: lambda "f" $
        Serialization.cst @@ (Literals.showFloat64 $ var "f")]


-- ============================================================================
-- Expression Conversions
-- ============================================================================

expressionToExpr :: TBinding (JS.Expression -> Expr)
expressionToExpr = define "expressionToExpr" $
  doc "Convert a JavaScript expression to an AST expression" $
  lambda "expr" $
    cases JS._Expression (var "expr") Nothing [
      JS._Expression_identifier>>: lambda "id" $ identifierToExpr @@ var "id",
      JS._Expression_literal>>: lambda "lit" $ literalToExpr @@ var "lit",
      JS._Expression_array>>: lambda "arr" $ arrayExpressionToExpr @@ var "arr",
      JS._Expression_object>>: lambda "obj" $ objectExpressionToExpr @@ var "obj",
      JS._Expression_function>>: lambda "fn" $ functionExpressionToExpr @@ var "fn",
      JS._Expression_arrow>>: lambda "arrow" $ arrowFunctionExpressionToExpr @@ var "arrow",
      JS._Expression_call>>: lambda "call" $ callExpressionToExpr @@ var "call",
      JS._Expression_member>>: lambda "mem" $ memberExpressionToExpr @@ var "mem",
      JS._Expression_conditional>>: lambda "cond" $ conditionalExpressionToExpr @@ var "cond",
      JS._Expression_binary>>: lambda "bin" $ binaryExpressionToExpr @@ var "bin",
      JS._Expression_unary>>: lambda "un" $ unaryExpressionToExpr @@ var "un",
      JS._Expression_assignment>>: lambda "assign" $ assignmentExpressionToExpr @@ var "assign",
      JS._Expression_sequence>>: lambda "exprs" $
        Serialization.parenList @@ false @@ (Lists.map (expressionToExpr) (var "exprs")),
      JS._Expression_this>>: constant $ Serialization.cst @@ string "this",
      JS._Expression_new>>: lambda "call" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "new",
          callExpressionToExpr @@ var "call"],
      JS._Expression_yield>>: lambda "maybeExpr" $
        Maybes.maybe
          (Serialization.cst @@ string "yield")
          (lambda "e" $ Serialization.spaceSep @@ list [
            Serialization.cst @@ string "yield",
            expressionToExpr @@ var "e"])
          (var "maybeExpr"),
      JS._Expression_await>>: lambda "e" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "await",
          expressionToExpr @@ var "e"],
      JS._Expression_spread>>: lambda "spread" $
        Serialization.prefix @@ string "..." @@ (expressionToExpr @@ (unwrap JS._SpreadElement @@ var "spread")),
      JS._Expression_parenthesized>>: lambda "e" $
        Serialization.parenthesize @@ (expressionToExpr @@ var "e")]

arrayExpressionToExpr :: TBinding (JS.ArrayExpression -> Expr)
arrayExpressionToExpr = define "arrayExpressionToExpr" $
  doc "Convert an array expression to an AST expression" $
  lambda "arr" $
    Serialization.bracketList @@ Serialization.inlineStyle @@
      (Lists.map (arrayElementToExpr) (var "arr"))

arrayElementToExpr :: TBinding (JS.ArrayElement -> Expr)
arrayElementToExpr = define "arrayElementToExpr" $
  doc "Convert an array element to an AST expression" $
  lambda "elem" $
    cases JS._ArrayElement (var "elem") Nothing [
      JS._ArrayElement_expression>>: lambda "e" $ expressionToExpr @@ var "e",
      JS._ArrayElement_spread>>: lambda "s" $
        Serialization.prefix @@ string "..." @@ (expressionToExpr @@ (unwrap JS._SpreadElement @@ var "s")),
      JS._ArrayElement_hole>>: constant $ Serialization.cst @@ string ""]

objectExpressionToExpr :: TBinding (JS.ObjectExpression -> Expr)
objectExpressionToExpr = define "objectExpressionToExpr" $
  doc "Convert an object expression to an AST expression" $
  lambda "obj" $
    Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      (Lists.map (propertyToExpr) (var "obj"))

propertyToExpr :: TBinding (JS.Property -> Expr)
propertyToExpr = define "propertyToExpr" $
  doc "Convert an object property to an AST expression" $
  lambda "prop" $ lets [
    "key">: project JS._Property JS._Property_key @@ var "prop",
    "value">: project JS._Property JS._Property_value @@ var "prop",
    "shorthand">: project JS._Property JS._Property_shorthand @@ var "prop",
    "computed">: project JS._Property JS._Property_computed @@ var "prop",
    "keyExpr">: Logic.ifElse (var "computed")
      (Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@ (expressionToExpr @@ var "key"))
      (expressionToExpr @@ var "key")] $
    Logic.ifElse (var "shorthand")
      (var "keyExpr")
      (Serialization.ifx @@ JavaScriptOperators.colonOp @@ var "keyExpr" @@ (expressionToExpr @@ var "value"))

functionExpressionToExpr :: TBinding (JS.FunctionExpression -> Expr)
functionExpressionToExpr = define "functionExpressionToExpr" $
  doc "Convert a function expression to an AST expression" $
  lambda "fn" $ lets [
    "mid">: project JS._FunctionExpression JS._FunctionExpression_id @@ var "fn",
    "params">: project JS._FunctionExpression JS._FunctionExpression_params @@ var "fn",
    "body">: project JS._FunctionExpression JS._FunctionExpression_body @@ var "fn",
    "async">: project JS._FunctionExpression JS._FunctionExpression_async @@ var "fn",
    "generator">: project JS._FunctionExpression JS._FunctionExpression_generator @@ var "fn",
    "asyncKw">: Logic.ifElse (var "async") (list [Serialization.cst @@ string "async"]) (list ([] :: [TTerm Expr])),
    "funcKw">: Logic.ifElse (var "generator")
      (Serialization.cst @@ string "function*")
      (Serialization.cst @@ string "function"),
    "nameExpr">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "id" $ list [identifierToExpr @@ var "id"])
      (var "mid"),
    "paramsExpr">: Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "params"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "asyncKw",
      list [var "funcKw"],
      var "nameExpr",
      list [var "paramsExpr", blockStatementToExpr @@ var "body"]])

arrowFunctionExpressionToExpr :: TBinding (JS.ArrowFunctionExpression -> Expr)
arrowFunctionExpressionToExpr = define "arrowFunctionExpressionToExpr" $
  doc "Convert an arrow function expression to an AST expression" $
  lambda "arrow" $ lets [
    "params">: project JS._ArrowFunctionExpression JS._ArrowFunctionExpression_params @@ var "arrow",
    "body">: project JS._ArrowFunctionExpression JS._ArrowFunctionExpression_body @@ var "arrow",
    "async">: project JS._ArrowFunctionExpression JS._ArrowFunctionExpression_async @@ var "arrow",
    "asyncKw">: Logic.ifElse (var "async")
      (list [Serialization.cst @@ string "async"])
      (list ([] :: [TTerm Expr])),
    "paramsExpr">: Logic.ifElse (Equality.equal (Lists.length $ var "params") (int32 1))
      (patternToExpr @@ (Lists.head $ var "params"))
      (Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "params"))),
    "bodyExpr">: cases JS._ArrowFunctionBody (var "body") Nothing [
      JS._ArrowFunctionBody_expression>>: lambda "e" $ expressionToExpr @@ var "e",
      JS._ArrowFunctionBody_block>>: lambda "b" $ blockStatementToExpr @@ var "b"]] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "asyncKw",
      list [Serialization.ifx @@ JavaScriptOperators.arrowOp @@ var "paramsExpr" @@ var "bodyExpr"]])

callExpressionToExpr :: TBinding (JS.CallExpression -> Expr)
callExpressionToExpr = define "callExpressionToExpr" $
  doc "Convert a call expression to an AST expression" $
  lambda "call" $ lets [
    "callee">: project JS._CallExpression JS._CallExpression_callee @@ var "call",
    "args">: project JS._CallExpression JS._CallExpression_arguments @@ var "call",
    "optional">: project JS._CallExpression JS._CallExpression_optional @@ var "call",
    "calleeExpr">: expressionToExpr @@ var "callee",
    "argsExpr">: Serialization.parenList @@ false @@ (Lists.map (expressionToExpr) (var "args")),
    "optionalDot">: Logic.ifElse (var "optional") (string "?.") (string "")] $
    Serialization.spaceSep @@ list [
      var "calleeExpr",
      Serialization.cst @@ var "optionalDot",
      var "argsExpr"]

memberExpressionToExpr :: TBinding (JS.MemberExpression -> Expr)
memberExpressionToExpr = define "memberExpressionToExpr" $
  doc "Convert a member expression to an AST expression" $
  lambda "mem" $ lets [
    "obj">: project JS._MemberExpression JS._MemberExpression_object @@ var "mem",
    "prop">: project JS._MemberExpression JS._MemberExpression_property @@ var "mem",
    "computed">: project JS._MemberExpression JS._MemberExpression_computed @@ var "mem",
    "optional">: project JS._MemberExpression JS._MemberExpression_optional @@ var "mem",
    "objExpr">: expressionToExpr @@ var "obj",
    "propExpr">: expressionToExpr @@ var "prop"] $
    Logic.ifElse (var "computed")
      -- obj[prop] or obj?.[prop]
      (Serialization.spaceSep @@ list [
        var "objExpr",
        Logic.ifElse (var "optional") (Serialization.cst @@ string "?.") (Serialization.cst @@ string ""),
        Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@ var "propExpr"])
      -- obj.prop or obj?.prop
      (Serialization.ifx @@
        (Logic.ifElse (var "optional") JavaScriptOperators.optionalChainOp JavaScriptOperators.memberOp) @@
        var "objExpr" @@ var "propExpr")

conditionalExpressionToExpr :: TBinding (JS.ConditionalExpression -> Expr)
conditionalExpressionToExpr = define "conditionalExpressionToExpr" $
  doc "Convert a conditional expression to an AST expression" $
  lambda "cond" $ lets [
    "test">: project JS._ConditionalExpression JS._ConditionalExpression_test @@ var "cond",
    "consequent">: project JS._ConditionalExpression JS._ConditionalExpression_consequent @@ var "cond",
    "alternate">: project JS._ConditionalExpression JS._ConditionalExpression_alternate @@ var "cond"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "test",
      Serialization.cst @@ string "?",
      expressionToExpr @@ var "consequent",
      Serialization.cst @@ string ":",
      expressionToExpr @@ var "alternate"]

binaryExpressionToExpr :: TBinding (JS.BinaryExpression -> Expr)
binaryExpressionToExpr = define "binaryExpressionToExpr" $
  doc "Convert a binary expression to an AST expression" $
  lambda "bin" $ lets [
    "op">: project JS._BinaryExpression JS._BinaryExpression_operator @@ var "bin",
    "left">: project JS._BinaryExpression JS._BinaryExpression_left @@ var "bin",
    "right">: project JS._BinaryExpression JS._BinaryExpression_right @@ var "bin"] $
    Serialization.ifx @@
      (binaryOperatorToExpr @@ var "op") @@
      (expressionToExpr @@ var "left") @@
      (expressionToExpr @@ var "right")

unaryExpressionToExpr :: TBinding (JS.UnaryExpression -> Expr)
unaryExpressionToExpr = define "unaryExpressionToExpr" $
  doc "Convert a unary expression to an AST expression" $
  lambda "un" $ lets [
    "op">: project JS._UnaryExpression JS._UnaryExpression_operator @@ var "un",
    "arg">: project JS._UnaryExpression JS._UnaryExpression_argument @@ var "un",
    "prefix">: project JS._UnaryExpression JS._UnaryExpression_prefix @@ var "un",
    "opStr">: unaryOperatorToString @@ var "op",
    "argExpr">: expressionToExpr @@ var "arg"] $
    Logic.ifElse (var "prefix")
      (Serialization.prefix @@ var "opStr" @@ var "argExpr")
      (Serialization.suffix @@ var "opStr" @@ var "argExpr")

assignmentExpressionToExpr :: TBinding (JS.AssignmentExpression -> Expr)
assignmentExpressionToExpr = define "assignmentExpressionToExpr" $
  doc "Convert an assignment expression to an AST expression" $
  lambda "assign" $ lets [
    "op">: project JS._AssignmentExpression JS._AssignmentExpression_operator @@ var "assign",
    "left">: project JS._AssignmentExpression JS._AssignmentExpression_left @@ var "assign",
    "right">: project JS._AssignmentExpression JS._AssignmentExpression_right @@ var "assign",
    "opStr">: assignmentOperatorToString @@ var "op"] $
    Serialization.spaceSep @@ list [
      patternToExpr @@ var "left",
      Serialization.cst @@ var "opStr",
      expressionToExpr @@ var "right"]


-- ============================================================================
-- Pattern Conversions
-- ============================================================================

patternToExpr :: TBinding (JS.Pattern -> Expr)
patternToExpr = define "patternToExpr" $
  doc "Convert a pattern to an AST expression" $
  lambda "pat" $
    cases JS._Pattern (var "pat") Nothing [
      JS._Pattern_identifier>>: lambda "id" $ identifierToExpr @@ var "id",
      JS._Pattern_object>>: lambda "obj" $ objectPatternToExpr @@ var "obj",
      JS._Pattern_array>>: lambda "arr" $ arrayPatternToExpr @@ var "arr",
      JS._Pattern_assignment>>: lambda "assign" $ assignmentPatternToExpr @@ var "assign",
      JS._Pattern_rest>>: lambda "rest" $
        Serialization.prefix @@ string "..." @@ (patternToExpr @@ (unwrap JS._RestElement @@ var "rest"))]

objectPatternToExpr :: TBinding (JS.ObjectPattern -> Expr)
objectPatternToExpr = define "objectPatternToExpr" $
  doc "Convert an object pattern to an AST expression" $
  lambda "obj" $ lets [
    "props">: project JS._ObjectPattern JS._ObjectPattern_properties @@ var "obj"] $
    Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@
      (Lists.map (objectPatternPropertyToExpr) (var "props"))

objectPatternPropertyToExpr :: TBinding (JS.ObjectPatternProperty -> Expr)
objectPatternPropertyToExpr = define "objectPatternPropertyToExpr" $
  doc "Convert an object pattern property to an AST expression" $
  lambda "prop" $
    cases JS._ObjectPatternProperty (var "prop") Nothing [
      JS._ObjectPatternProperty_property>>: lambda "p" $ propertyToExpr @@ var "p",
      JS._ObjectPatternProperty_rest>>: lambda "r" $
        Serialization.prefix @@ string "..." @@ (patternToExpr @@ (unwrap JS._RestElement @@ var "r"))]

arrayPatternToExpr :: TBinding (JS.ArrayPattern -> Expr)
arrayPatternToExpr = define "arrayPatternToExpr" $
  doc "Convert an array pattern to an AST expression" $
  lambda "arr" $
    Serialization.bracketList @@ Serialization.inlineStyle @@
      (Lists.map
        (lambda "maybeP" $ Maybes.maybe (Serialization.cst @@ string "") (patternToExpr) (var "maybeP"))
        (var "arr"))

assignmentPatternToExpr :: TBinding (JS.AssignmentPattern -> Expr)
assignmentPatternToExpr = define "assignmentPatternToExpr" $
  doc "Convert an assignment pattern to an AST expression" $
  lambda "assign" $ lets [
    "left">: project JS._AssignmentPattern JS._AssignmentPattern_left @@ var "assign",
    "right">: project JS._AssignmentPattern JS._AssignmentPattern_right @@ var "assign"] $
    Serialization.ifx @@ JavaScriptOperators.defineOp @@
      (patternToExpr @@ var "left") @@
      (expressionToExpr @@ var "right")


-- ============================================================================
-- Statement Conversions
-- ============================================================================

statementToExpr :: TBinding (JS.Statement -> Expr)
statementToExpr = define "statementToExpr" $
  doc "Convert a statement to an AST expression" $
  lambda "stmt" $
    cases JS._Statement (var "stmt") Nothing [
      JS._Statement_expression>>: lambda "e" $
        Serialization.suffix @@ string ";" @@ (expressionToExpr @@ var "e"),
      JS._Statement_block>>: lambda "b" $ blockStatementToExpr @@ var "b",
      JS._Statement_empty>>: constant $ Serialization.cst @@ string ";",
      JS._Statement_debugger>>: constant $ Serialization.cst @@ string "debugger;",
      JS._Statement_return>>: lambda "r" $ returnStatementToExpr @@ var "r",
      JS._Statement_break>>: lambda "b" $ breakStatementToExpr @@ var "b",
      JS._Statement_continue>>: lambda "c" $ continueStatementToExpr @@ var "c",
      JS._Statement_if>>: lambda "i" $ ifStatementToExpr @@ var "i",
      JS._Statement_switch>>: lambda "s" $ switchStatementToExpr @@ var "s",
      JS._Statement_throw>>: lambda "t" $ throwStatementToExpr @@ var "t",
      JS._Statement_try>>: lambda "t" $ tryStatementToExpr @@ var "t",
      JS._Statement_while>>: lambda "w" $ whileStatementToExpr @@ var "w",
      JS._Statement_doWhile>>: lambda "d" $ doWhileStatementToExpr @@ var "d",
      JS._Statement_for>>: lambda "f" $ forStatementToExpr @@ var "f",
      JS._Statement_forIn>>: lambda "f" $ forInStatementToExpr @@ var "f",
      JS._Statement_forOf>>: lambda "f" $ forOfStatementToExpr @@ var "f",
      JS._Statement_variableDeclaration>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
      JS._Statement_functionDeclaration>>: lambda "f" $ functionDeclarationToExpr @@ var "f",
      JS._Statement_classDeclaration>>: lambda "c" $ classDeclarationToExpr @@ var "c",
      JS._Statement_labeled>>: lambda "l" $ labeledStatementToExpr @@ var "l"]

blockStatementToExpr :: TBinding (JS.BlockStatement -> Expr)
blockStatementToExpr = define "blockStatementToExpr" $
  doc "Convert a block statement to an AST expression" $
  lambda "block" $
    Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@
      (Lists.map (statementToExpr) (var "block"))

variableDeclarationToExpr :: TBinding (JS.VariableDeclaration -> Expr)
variableDeclarationToExpr = define "variableDeclarationToExpr" $
  doc "Convert a variable declaration to an AST expression" $
  lambda "decl" $ lets [
    "kind">: project JS._VariableDeclaration JS._VariableDeclaration_kind @@ var "decl",
    "declarations">: project JS._VariableDeclaration JS._VariableDeclaration_declarations @@ var "decl"] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        variableKindToExpr @@ var "kind",
        Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map (variableDeclaratorToExpr) (var "declarations"))])

variableDeclaratorToExpr :: TBinding (JS.VariableDeclarator -> Expr)
variableDeclaratorToExpr = define "variableDeclaratorToExpr" $
  doc "Convert a variable declarator to an AST expression" $
  lambda "decl" $ lets [
    "id">: project JS._VariableDeclarator JS._VariableDeclarator_id @@ var "decl",
    "init">: project JS._VariableDeclarator JS._VariableDeclarator_init @@ var "decl"] $
    Maybes.maybe
      (patternToExpr @@ var "id")
      (lambda "e" $ Serialization.ifx @@ JavaScriptOperators.defineOp @@
        (patternToExpr @@ var "id") @@
        (expressionToExpr @@ var "e"))
      (var "init")

variableKindToExpr :: TBinding (JS.VariableKind -> Expr)
variableKindToExpr = define "variableKindToExpr" $
  doc "Convert a variable kind to an AST expression" $
  lambda "kind" $
    cases JS._VariableKind (var "kind") Nothing [
      JS._VariableKind_var>>: constant $ Serialization.cst @@ string "var",
      JS._VariableKind_let>>: constant $ Serialization.cst @@ string "let",
      JS._VariableKind_const>>: constant $ Serialization.cst @@ string "const"]

ifStatementToExpr :: TBinding (JS.IfStatement -> Expr)
ifStatementToExpr = define "ifStatementToExpr" $
  doc "Convert an if statement to an AST expression" $
  lambda "ifStmt" $ lets [
    "test">: project JS._IfStatement JS._IfStatement_test @@ var "ifStmt",
    "consequent">: project JS._IfStatement JS._IfStatement_consequent @@ var "ifStmt",
    "alternate">: project JS._IfStatement JS._IfStatement_alternate @@ var "ifStmt",
    "ifPart">: Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      Serialization.parenthesize @@ (expressionToExpr @@ var "test"),
      statementToExpr @@ var "consequent"]] $
    Maybes.maybe
      (var "ifPart")
      (lambda "alt" $ Serialization.spaceSep @@ list [
        var "ifPart",
        Serialization.cst @@ string "else",
        statementToExpr @@ var "alt"])
      (var "alternate")

switchStatementToExpr :: TBinding (JS.SwitchStatement -> Expr)
switchStatementToExpr = define "switchStatementToExpr" $
  doc "Convert a switch statement to an AST expression" $
  lambda "switchStmt" $ lets [
    "discriminant">: project JS._SwitchStatement JS._SwitchStatement_discriminant @@ var "switchStmt",
    "cases">: project JS._SwitchStatement JS._SwitchStatement_cases @@ var "switchStmt"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "switch",
      Serialization.parenthesize @@ (expressionToExpr @@ var "discriminant"),
      Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@
        (Lists.map (switchCaseToExpr) (var "cases"))]

switchCaseToExpr :: TBinding (JS.SwitchCase -> Expr)
switchCaseToExpr = define "switchCaseToExpr" $
  doc "Convert a switch case to an AST expression" $
  lambda "c" $ lets [
    "test">: project JS._SwitchCase JS._SwitchCase_test @@ var "c",
    "consequent">: project JS._SwitchCase JS._SwitchCase_consequent @@ var "c",
    "caseLabel">: Maybes.maybe
      (Serialization.cst @@ string "default:")
      (lambda "t" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "case",
        expressionToExpr @@ var "t",
        Serialization.cst @@ string ":"])
      (var "test")] $
    Serialization.newlineSep @@ (Lists.cons (var "caseLabel") (Lists.map (statementToExpr) (var "consequent")))

returnStatementToExpr :: TBinding (JS.ReturnStatement -> Expr)
returnStatementToExpr = define "returnStatementToExpr" $
  doc "Convert a return statement to an AST expression" $
  lambda "r" $
    Maybes.maybe
      (Serialization.cst @@ string "return;")
      (lambda "e" $ Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "return",
          expressionToExpr @@ var "e"]))
      (var "r")

throwStatementToExpr :: TBinding (JS.ThrowStatement -> Expr)
throwStatementToExpr = define "throwStatementToExpr" $
  doc "Convert a throw statement to an AST expression" $
  lambda "t" $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "throw",
        expressionToExpr @@ (unwrap JS._ThrowStatement @@ var "t")])

tryStatementToExpr :: TBinding (JS.TryStatement -> Expr)
tryStatementToExpr = define "tryStatementToExpr" $
  doc "Convert a try statement to an AST expression" $
  lambda "t" $ lets [
    "block">: project JS._TryStatement JS._TryStatement_block @@ var "t",
    "handler">: project JS._TryStatement JS._TryStatement_handler @@ var "t",
    "finalizer">: project JS._TryStatement JS._TryStatement_finalizer @@ var "t",
    "tryPart">: Serialization.spaceSep @@ list [
      Serialization.cst @@ string "try",
      blockStatementToExpr @@ var "block"],
    "catchPart">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "c" $ list [catchClauseToExpr @@ var "c"])
      (var "handler"),
    "finallyPart">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "f" $ list [Serialization.spaceSep @@ list [
        Serialization.cst @@ string "finally",
        blockStatementToExpr @@ var "f"]])
      (var "finalizer")] $
    Serialization.spaceSep @@ (Lists.concat $ list [list [var "tryPart"], var "catchPart", var "finallyPart"])

catchClauseToExpr :: TBinding (JS.CatchClause -> Expr)
catchClauseToExpr = define "catchClauseToExpr" $
  doc "Convert a catch clause to an AST expression" $
  lambda "c" $ lets [
    "param">: project JS._CatchClause JS._CatchClause_param @@ var "c",
    "body">: project JS._CatchClause JS._CatchClause_body @@ var "c",
    "catchKw">: Maybes.maybe
      (Serialization.cst @@ string "catch")
      (lambda "p" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "catch",
        Serialization.parenthesize @@ (patternToExpr @@ var "p")])
      (var "param")] $
    Serialization.spaceSep @@ list [var "catchKw", blockStatementToExpr @@ var "body"]

breakStatementToExpr :: TBinding (JS.BreakStatement -> Expr)
breakStatementToExpr = define "breakStatementToExpr" $
  doc "Convert a break statement to an AST expression" $
  lambda "b" $
    Maybes.maybe
      (Serialization.cst @@ string "break;")
      (lambda "label" $ Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "break",
          identifierToExpr @@ var "label"]))
      (var "b")

continueStatementToExpr :: TBinding (JS.ContinueStatement -> Expr)
continueStatementToExpr = define "continueStatementToExpr" $
  doc "Convert a continue statement to an AST expression" $
  lambda "c" $
    Maybes.maybe
      (Serialization.cst @@ string "continue;")
      (lambda "label" $ Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "continue",
          identifierToExpr @@ var "label"]))
      (var "c")

whileStatementToExpr :: TBinding (JS.WhileStatement -> Expr)
whileStatementToExpr = define "whileStatementToExpr" $
  doc "Convert a while statement to an AST expression" $
  lambda "w" $ lets [
    "test">: project JS._WhileStatement JS._WhileStatement_test @@ var "w",
    "body">: project JS._WhileStatement JS._WhileStatement_body @@ var "w"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "while",
      Serialization.parenthesize @@ (expressionToExpr @@ var "test"),
      statementToExpr @@ var "body"]

doWhileStatementToExpr :: TBinding (JS.DoWhileStatement -> Expr)
doWhileStatementToExpr = define "doWhileStatementToExpr" $
  doc "Convert a do-while statement to an AST expression" $
  lambda "d" $ lets [
    "body">: project JS._DoWhileStatement JS._DoWhileStatement_body @@ var "d",
    "test">: project JS._DoWhileStatement JS._DoWhileStatement_test @@ var "d"] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "do",
        statementToExpr @@ var "body",
        Serialization.cst @@ string "while",
        Serialization.parenthesize @@ (expressionToExpr @@ var "test")])

forStatementToExpr :: TBinding (JS.ForStatement -> Expr)
forStatementToExpr = define "forStatementToExpr" $
  doc "Convert a for statement to an AST expression" $
  lambda "f" $ lets [
    "init">: project JS._ForStatement JS._ForStatement_init @@ var "f",
    "test">: project JS._ForStatement JS._ForStatement_test @@ var "f",
    "update">: project JS._ForStatement JS._ForStatement_update @@ var "f",
    "body">: project JS._ForStatement JS._ForStatement_body @@ var "f",
    "initExpr">: Maybes.maybe (Serialization.cst @@ string "")
      (lambda "i" $ cases JS._ForInit (var "i") Nothing [
        JS._ForInit_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
        JS._ForInit_expression>>: lambda "e" $ expressionToExpr @@ var "e"])
      (var "init"),
    "testExpr">: Maybes.maybe (Serialization.cst @@ string "") (expressionToExpr) (var "test"),
    "updateExpr">: Maybes.maybe (Serialization.cst @@ string "") (expressionToExpr) (var "update")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "for",
      Serialization.parenList @@ false @@ list [var "initExpr", var "testExpr", var "updateExpr"],
      statementToExpr @@ var "body"]

forInStatementToExpr :: TBinding (JS.ForInStatement -> Expr)
forInStatementToExpr = define "forInStatementToExpr" $
  doc "Convert a for-in statement to an AST expression" $
  lambda "f" $ lets [
    "left">: project JS._ForInStatement JS._ForInStatement_left @@ var "f",
    "right">: project JS._ForInStatement JS._ForInStatement_right @@ var "f",
    "body">: project JS._ForInStatement JS._ForInStatement_body @@ var "f",
    "leftExpr">: cases JS._ForInLeft (var "left") Nothing [
      JS._ForInLeft_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
      JS._ForInLeft_pattern>>: lambda "p" $ patternToExpr @@ var "p"]] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "for",
      Serialization.parenthesize @@ (Serialization.spaceSep @@ list [
        var "leftExpr",
        Serialization.cst @@ string "in",
        expressionToExpr @@ var "right"]),
      statementToExpr @@ var "body"]

forOfStatementToExpr :: TBinding (JS.ForOfStatement -> Expr)
forOfStatementToExpr = define "forOfStatementToExpr" $
  doc "Convert a for-of statement to an AST expression" $
  lambda "f" $ lets [
    "await">: project JS._ForOfStatement JS._ForOfStatement_await @@ var "f",
    "left">: project JS._ForOfStatement JS._ForOfStatement_left @@ var "f",
    "right">: project JS._ForOfStatement JS._ForOfStatement_right @@ var "f",
    "body">: project JS._ForOfStatement JS._ForOfStatement_body @@ var "f",
    "forKw">: Logic.ifElse (var "await")
      (Serialization.cst @@ string "for await")
      (Serialization.cst @@ string "for"),
    "leftExpr">: cases JS._ForInLeft (var "left") Nothing [
      JS._ForInLeft_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
      JS._ForInLeft_pattern>>: lambda "p" $ patternToExpr @@ var "p"]] $
    Serialization.spaceSep @@ list [
      var "forKw",
      Serialization.parenthesize @@ (Serialization.spaceSep @@ list [
        var "leftExpr",
        Serialization.cst @@ string "of",
        expressionToExpr @@ var "right"]),
      statementToExpr @@ var "body"]

labeledStatementToExpr :: TBinding (JS.LabeledStatement -> Expr)
labeledStatementToExpr = define "labeledStatementToExpr" $
  doc "Convert a labeled statement to an AST expression" $
  lambda "l" $ lets [
    "label">: project JS._LabeledStatement JS._LabeledStatement_label @@ var "l",
    "body">: project JS._LabeledStatement JS._LabeledStatement_body @@ var "l"] $
    Serialization.spaceSep @@ list [
      Serialization.suffix @@ string ":" @@ (identifierToExpr @@ var "label"),
      statementToExpr @@ var "body"]


-- ============================================================================
-- Declaration Conversions
-- ============================================================================

functionDeclarationToExpr :: TBinding (JS.FunctionDeclaration -> Expr)
functionDeclarationToExpr = define "functionDeclarationToExpr" $
  doc "Convert a function declaration to an AST expression" $
  lambda "fn" $ lets [
    "id">: project JS._FunctionDeclaration JS._FunctionDeclaration_id @@ var "fn",
    "params">: project JS._FunctionDeclaration JS._FunctionDeclaration_params @@ var "fn",
    "body">: project JS._FunctionDeclaration JS._FunctionDeclaration_body @@ var "fn",
    "async">: project JS._FunctionDeclaration JS._FunctionDeclaration_async @@ var "fn",
    "generator">: project JS._FunctionDeclaration JS._FunctionDeclaration_generator @@ var "fn",
    "asyncKw">: Logic.ifElse (var "async") (list [Serialization.cst @@ string "async"]) (list ([] :: [TTerm Expr])),
    "funcKw">: Logic.ifElse (var "generator")
      (Serialization.cst @@ string "function*")
      (Serialization.cst @@ string "function"),
    "paramsExpr">: Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "params"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "asyncKw",
      list [var "funcKw", identifierToExpr @@ var "id", var "paramsExpr", blockStatementToExpr @@ var "body"]])

classDeclarationToExpr :: TBinding (JS.ClassDeclaration -> Expr)
classDeclarationToExpr = define "classDeclarationToExpr" $
  doc "Convert a class declaration to an AST expression" $
  lambda "cls" $ lets [
    "id">: project JS._ClassDeclaration JS._ClassDeclaration_id @@ var "cls",
    "superClass">: project JS._ClassDeclaration JS._ClassDeclaration_superClass @@ var "cls",
    "body">: project JS._ClassDeclaration JS._ClassDeclaration_body @@ var "cls",
    "extendsClause">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "s" $ list [Serialization.cst @@ string "extends", expressionToExpr @@ var "s"])
      (var "superClass"),
    "bodyExpr">: Serialization.curlyBracesList @@ nothing @@ Serialization.fullBlockStyle @@
      (Lists.map (methodDefinitionToExpr) (var "body"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [Serialization.cst @@ string "class", identifierToExpr @@ var "id"],
      var "extendsClause",
      list [var "bodyExpr"]])

methodDefinitionToExpr :: TBinding (JS.MethodDefinition -> Expr)
methodDefinitionToExpr = define "methodDefinitionToExpr" $
  doc "Convert a method definition to an AST expression" $
  lambda "method" $ lets [
    "key">: project JS._MethodDefinition JS._MethodDefinition_key @@ var "method",
    "value">: project JS._MethodDefinition JS._MethodDefinition_value @@ var "method",
    "kind">: project JS._MethodDefinition JS._MethodDefinition_kind @@ var "method",
    "computed">: project JS._MethodDefinition JS._MethodDefinition_computed @@ var "method",
    "static">: project JS._MethodDefinition JS._MethodDefinition_static @@ var "method",
    "staticKw">: Logic.ifElse (var "static") (list [Serialization.cst @@ string "static"]) (list ([] :: [TTerm Expr])),
    "kindKw">: cases JS._MethodKind (var "kind") Nothing [
      JS._MethodKind_constructor>>: constant $ list ([] :: [TTerm Expr]),
      JS._MethodKind_method>>: constant $ list ([] :: [TTerm Expr]),
      JS._MethodKind_get>>: constant $ list [Serialization.cst @@ string "get"],
      JS._MethodKind_set>>: constant $ list [Serialization.cst @@ string "set"]],
    "keyExpr">: Logic.ifElse (var "computed")
      (Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@ (expressionToExpr @@ var "key"))
      (expressionToExpr @@ var "key"),
    "params">: project JS._FunctionExpression JS._FunctionExpression_params @@ var "value",
    "body">: project JS._FunctionExpression JS._FunctionExpression_body @@ var "value",
    "paramsExpr">: Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "params"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "staticKw",
      var "kindKw",
      list [var "keyExpr", var "paramsExpr", blockStatementToExpr @@ var "body"]])


-- ============================================================================
-- Module Conversions
-- ============================================================================

programToExpr :: TBinding (JS.Program -> Expr)
programToExpr = define "programToExpr" $
  doc "Convert a JavaScript program to an AST expression" $
  lambda "prog" $ lets [
    "body">: project JS._Program JS._Program_body @@ var "prog",
    "warning">: list [Serialization.cst @@ (toLineComment @@ Constants.warningAutoGeneratedFile)],
    "items">: Lists.map (moduleItemToExpr) (var "body")] $
    Serialization.doubleNewlineSep @@ (Lists.concat $ list [var "warning", var "items"])

moduleItemToExpr :: TBinding (JS.ModuleItem -> Expr)
moduleItemToExpr = define "moduleItemToExpr" $
  doc "Convert a module item to an AST expression" $
  lambda "item" $
    cases JS._ModuleItem (var "item") Nothing [
      JS._ModuleItem_statement>>: lambda "s" $ statementToExpr @@ var "s",
      JS._ModuleItem_import>>: lambda "i" $ importDeclarationToExpr @@ var "i",
      JS._ModuleItem_export>>: lambda "e" $ exportDeclarationToExpr @@ var "e"]

importDeclarationToExpr :: TBinding (JS.ImportDeclaration -> Expr)
importDeclarationToExpr = define "importDeclarationToExpr" $
  doc "Convert an import declaration to an AST expression" $
  lambda "imp" $ lets [
    "specifiers">: project JS._ImportDeclaration JS._ImportDeclaration_specifiers @@ var "imp",
    "source">: project JS._ImportDeclaration JS._ImportDeclaration_source @@ var "imp",
    "sourceExpr">: stringLiteralToExpr @@ var "source",
    "specExprs">: Lists.map (importSpecifierToExpr) (var "specifiers")] $
    Logic.ifElse (Lists.null $ var "specifiers")
      -- import "module"
      (Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [Serialization.cst @@ string "import", var "sourceExpr"]))
      -- import { x, y } from "module"
      (Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "import",
          formatImportSpecifiers @@ var "specExprs",
          Serialization.cst @@ string "from",
          var "sourceExpr"]))

importSpecifierToExpr :: TBinding (JS.ImportClause -> Expr)
importSpecifierToExpr = define "importSpecifierToExpr" $
  doc "Convert an import specifier to an AST expression" $
  lambda "spec" $
    cases JS._ImportClause (var "spec") Nothing [
      JS._ImportClause_named>>: lambda "n" $ lets [
        "imported">: project JS._ImportSpecifier JS._ImportSpecifier_imported @@ var "n",
        "local">: project JS._ImportSpecifier JS._ImportSpecifier_local @@ var "n"] $
        Logic.ifElse (Equality.equal (unwrap JS._Identifier @@ var "imported") (unwrap JS._Identifier @@ var "local"))
          (identifierToExpr @@ var "local")
          (Serialization.spaceSep @@ list [
            identifierToExpr @@ var "imported",
            Serialization.cst @@ string "as",
            identifierToExpr @@ var "local"]),
      JS._ImportClause_default>>: lambda "d" $ identifierToExpr @@ (unwrap JS._ImportDefaultSpecifier @@ var "d"),
      JS._ImportClause_namespace>>: lambda "n" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "*",
          Serialization.cst @@ string "as",
          identifierToExpr @@ (unwrap JS._ImportNamespaceSpecifier @@ var "n")]]

formatImportSpecifiers :: TBinding ([Expr] -> Expr)
formatImportSpecifiers = define "formatImportSpecifiers" $
  doc "Format import specifiers, handling default vs named imports" $
  lambda "specs" $
    -- Simplified: just wrap named imports in braces
    Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "specs"

exportDeclarationToExpr :: TBinding (JS.ExportDeclaration -> Expr)
exportDeclarationToExpr = define "exportDeclarationToExpr" $
  doc "Convert an export declaration to an AST expression" $
  lambda "exp" $
    cases JS._ExportDeclaration (var "exp") Nothing [
      JS._ExportDeclaration_named>>: lambda "n" $ namedExportToExpr @@ var "n",
      JS._ExportDeclaration_default>>: lambda "e" $
        Serialization.suffix @@ string ";" @@
          (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "export",
            Serialization.cst @@ string "default",
            expressionToExpr @@ var "e"]),
      JS._ExportDeclaration_declaration>>: lambda "d" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "export",
          statementToExpr @@ var "d"],
      JS._ExportDeclaration_all>>: lambda "a" $ exportAllToExpr @@ var "a"]

namedExportToExpr :: TBinding (JS.NamedExport -> Expr)
namedExportToExpr = define "namedExportToExpr" $
  doc "Convert a named export to an AST expression" $
  lambda "n" $ lets [
    "specifiers">: project JS._NamedExport JS._NamedExport_specifiers @@ var "n",
    "source">: project JS._NamedExport JS._NamedExport_source @@ var "n",
    "specExprs">: Lists.map (exportSpecifierToExpr) (var "specifiers"),
    "fromClause">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "s" $ list [Serialization.cst @@ string "from", stringLiteralToExpr @@ var "s"])
      (var "source")] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ (Lists.concat $ list [
        list [Serialization.cst @@ string "export"],
        list [Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "specExprs"],
        var "fromClause"]))

exportSpecifierToExpr :: TBinding (JS.ExportSpecifier -> Expr)
exportSpecifierToExpr = define "exportSpecifierToExpr" $
  doc "Convert an export specifier to an AST expression" $
  lambda "spec" $ lets [
    "local">: project JS._ExportSpecifier JS._ExportSpecifier_local @@ var "spec",
    "exported">: project JS._ExportSpecifier JS._ExportSpecifier_exported @@ var "spec"] $
    Logic.ifElse (Equality.equal (unwrap JS._Identifier @@ var "local") (unwrap JS._Identifier @@ var "exported"))
      (identifierToExpr @@ var "local")
      (Serialization.spaceSep @@ list [
        identifierToExpr @@ var "local",
        Serialization.cst @@ string "as",
        identifierToExpr @@ var "exported"])

exportAllToExpr :: TBinding (JS.ExportAllDeclaration -> Expr)
exportAllToExpr = define "exportAllToExpr" $
  doc "Convert an export all declaration to an AST expression" $
  lambda "a" $ lets [
    "exported">: project JS._ExportAllDeclaration JS._ExportAllDeclaration_exported @@ var "a",
    "source">: project JS._ExportAllDeclaration JS._ExportAllDeclaration_source @@ var "a",
    "exportedClause">: Maybes.maybe
      (Serialization.cst @@ string "*")
      (lambda "e" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "*",
        Serialization.cst @@ string "as",
        identifierToExpr @@ var "e"])
      (var "exported")] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "export",
        var "exportedClause",
        Serialization.cst @@ string "from",
        stringLiteralToExpr @@ var "source"])


-- ============================================================================
-- Operators
-- ============================================================================

binaryOperatorToExpr :: TBinding (JS.BinaryOperator -> Op)
binaryOperatorToExpr = define "binaryOperatorToExpr" $
  doc "Convert a binary operator to an Op" $
  lambda "op" $
    cases JS._BinaryOperator (var "op") Nothing [
      JS._BinaryOperator_add>>: constant JavaScriptOperators.addOp,
      JS._BinaryOperator_subtract>>: constant JavaScriptOperators.subtractOp,
      JS._BinaryOperator_multiply>>: constant JavaScriptOperators.multiplyOp,
      JS._BinaryOperator_divide>>: constant JavaScriptOperators.divideOp,
      JS._BinaryOperator_modulo>>: constant JavaScriptOperators.moduloOp,
      JS._BinaryOperator_exponentiate>>: constant JavaScriptOperators.exponentiateOp,
      JS._BinaryOperator_equal>>: constant JavaScriptOperators.equalOp,
      JS._BinaryOperator_notEqual>>: constant JavaScriptOperators.notEqualOp,
      JS._BinaryOperator_strictEqual>>: constant JavaScriptOperators.strictEqualOp,
      JS._BinaryOperator_strictNotEqual>>: constant JavaScriptOperators.strictNotEqualOp,
      JS._BinaryOperator_lessThan>>: constant JavaScriptOperators.lessThanOp,
      JS._BinaryOperator_lessThanOrEqual>>: constant JavaScriptOperators.lessThanOrEqualOp,
      JS._BinaryOperator_greaterThan>>: constant JavaScriptOperators.greaterThanOp,
      JS._BinaryOperator_greaterThanOrEqual>>: constant JavaScriptOperators.greaterThanOrEqualOp,
      JS._BinaryOperator_and>>: constant JavaScriptOperators.logicalAndOp,
      JS._BinaryOperator_or>>: constant JavaScriptOperators.logicalOrOp,
      JS._BinaryOperator_nullishCoalescing>>: constant JavaScriptOperators.nullishCoalescingOp,
      JS._BinaryOperator_bitwiseAnd>>: constant JavaScriptOperators.bitwiseAndOp,
      JS._BinaryOperator_bitwiseOr>>: constant JavaScriptOperators.bitwiseOrOp,
      JS._BinaryOperator_bitwiseXor>>: constant JavaScriptOperators.bitwiseXorOp,
      JS._BinaryOperator_leftShift>>: constant JavaScriptOperators.leftShiftOp,
      JS._BinaryOperator_rightShift>>: constant JavaScriptOperators.rightShiftOp,
      JS._BinaryOperator_unsignedRightShift>>: constant JavaScriptOperators.unsignedRightShiftOp,
      JS._BinaryOperator_in>>: constant JavaScriptOperators.inOp,
      JS._BinaryOperator_instanceof>>: constant JavaScriptOperators.instanceOfOp]

unaryOperatorToString :: TBinding (JS.UnaryOperator -> String)
unaryOperatorToString = define "unaryOperatorToString" $
  doc "Convert a unary operator to a string" $
  lambda "op" $
    cases JS._UnaryOperator (var "op") Nothing [
      JS._UnaryOperator_negate>>: constant $ string "-",
      JS._UnaryOperator_plus>>: constant $ string "+",
      JS._UnaryOperator_not>>: constant $ string "!",
      JS._UnaryOperator_bitwiseNot>>: constant $ string "~",
      JS._UnaryOperator_typeof>>: constant $ string "typeof ",
      JS._UnaryOperator_void>>: constant $ string "void ",
      JS._UnaryOperator_delete>>: constant $ string "delete ",
      JS._UnaryOperator_increment>>: constant $ string "++",
      JS._UnaryOperator_decrement>>: constant $ string "--"]

assignmentOperatorToString :: TBinding (JS.AssignmentOperator -> String)
assignmentOperatorToString = define "assignmentOperatorToString" $
  doc "Convert an assignment operator to a string" $
  lambda "op" $
    cases JS._AssignmentOperator (var "op") Nothing [
      JS._AssignmentOperator_assign>>: constant $ string "=",
      JS._AssignmentOperator_addAssign>>: constant $ string "+=",
      JS._AssignmentOperator_subtractAssign>>: constant $ string "-=",
      JS._AssignmentOperator_multiplyAssign>>: constant $ string "*=",
      JS._AssignmentOperator_divideAssign>>: constant $ string "/=",
      JS._AssignmentOperator_moduloAssign>>: constant $ string "%=",
      JS._AssignmentOperator_exponentiateAssign>>: constant $ string "**=",
      JS._AssignmentOperator_leftShiftAssign>>: constant $ string "<<=",
      JS._AssignmentOperator_rightShiftAssign>>: constant $ string ">>=",
      JS._AssignmentOperator_unsignedRightShiftAssign>>: constant $ string ">>>=",
      JS._AssignmentOperator_bitwiseAndAssign>>: constant $ string "&=",
      JS._AssignmentOperator_bitwiseOrAssign>>: constant $ string "|=",
      JS._AssignmentOperator_bitwiseXorAssign>>: constant $ string "^=",
      JS._AssignmentOperator_andAssign>>: constant $ string "&&=",
      JS._AssignmentOperator_orAssign>>: constant $ string "||=",
      JS._AssignmentOperator_nullishAssign>>: constant $ string "??="]


-- ============================================================================
-- Comments
-- ============================================================================

documentationCommentToExpr :: TBinding (JS.DocumentationComment -> Expr)
documentationCommentToExpr = define "documentationCommentToExpr" $
  doc "Convert a documentation comment to an AST expression" $
  lambda "doc" $ lets [
    "description">: project JS._DocumentationComment JS._DocumentationComment_description @@ var "doc",
    "tags">: project JS._DocumentationComment JS._DocumentationComment_tags @@ var "doc"] $
    Serialization.cst @@ (toJavaScriptComments @@ var "description" @@ var "tags")

toJavaScriptComments :: TBinding (String -> [JS.DocumentationTag] -> String)
toJavaScriptComments = define "toJavaScriptComments" $
  doc "Format a description and tags as a JSDoc comment" $
  lambda "desc" $ lambda "tags" $ lets [
    "descLines">: Logic.ifElse (Equality.equal (var "desc") (string ""))
      (list ([] :: [TTerm String]))
      (Lists.map (lambda "line" $ Strings.cat2 (string " * ") (var "line")) (Strings.lines $ var "desc")),
    "tagLines">: Lists.map (documentationTagToLine) (var "tags"),
    "allLines">: Lists.concat $ list [var "descLines", var "tagLines"]] $
    Logic.ifElse (Lists.null $ var "allLines")
      (string "")
      (Strings.intercalate (string "\n") $
        Lists.concat $ list [
          list [string "/**"],
          var "allLines",
          list [string " */"]])

documentationTagToLine :: TBinding (JS.DocumentationTag -> String)
documentationTagToLine = define "documentationTagToLine" $
  doc "Convert a documentation tag to a JSDoc line" $
  lambda "tag" $ lets [
    "name">: project JS._DocumentationTag JS._DocumentationTag_name @@ var "tag",
    "mtype">: project JS._DocumentationTag JS._DocumentationTag_type @@ var "tag",
    "mparamName">: project JS._DocumentationTag JS._DocumentationTag_paramName @@ var "tag",
    "description">: project JS._DocumentationTag JS._DocumentationTag_description @@ var "tag",
    "typePart">: Maybes.maybe (string "") (lambda "t" $ Strings.cat $ list [string "{", typeExpressionToString @@ var "t", string "} "]) (var "mtype"),
    "paramPart">: Maybes.maybe (string "") (lambda "p" $ Strings.cat2 (unwrap JS._Identifier @@ var "p") (string " ")) (var "mparamName")] $
    Strings.cat $ list [string " * @", var "name", string " ", var "typePart", var "paramPart", var "description"]

typeExpressionToString :: TBinding (JS.TypeExpression -> String)
typeExpressionToString = define "typeExpressionToString" $
  doc "Convert a type expression to a string for JSDoc" $
  lambda "typ" $
    cases JS._TypeExpression (var "typ") Nothing [
      JS._TypeExpression_identifier>>: lambda "id" $ unwrap JS._Identifier @@ var "id",
      JS._TypeExpression_any>>: constant $ string "*",
      JS._TypeExpression_void>>: constant $ string "void",
      JS._TypeExpression_never>>: constant $ string "never",
      -- Simplified handling for other cases
      JS._TypeExpression_literal>>: lambda "l" $ string "literal",
      JS._TypeExpression_array>>: lambda "a" $ Strings.cat2 (typeExpressionToString @@ (unwrap JS._ArrayTypeExpression @@ var "a")) (string "[]"),
      JS._TypeExpression_function>>: constant $ string "Function",
      JS._TypeExpression_object>>: constant $ string "Object",
      JS._TypeExpression_union>>: lambda "u" $ Strings.intercalate (string "|") (Lists.map (typeExpressionToString) (var "u")),
      JS._TypeExpression_parameterized>>: lambda "p" $ lets [
        "base">: project JS._ParameterizedTypeExpression JS._ParameterizedTypeExpression_base @@ var "p",
        "args">: project JS._ParameterizedTypeExpression JS._ParameterizedTypeExpression_arguments @@ var "p"] $
        Strings.cat $ list [
          typeExpressionToString @@ var "base",
          string "<",
          Strings.intercalate (string ", ") (Lists.map (typeExpressionToString) (var "args")),
          string ">"],
      JS._TypeExpression_optional>>: lambda "o" $ Strings.cat2 (string "?") (typeExpressionToString @@ var "o")]

toLineComment :: TBinding (String -> String)
toLineComment = define "toLineComment" $
  doc "Convert a string to a JavaScript line comment" $
  lambda "s" $ Strings.intercalate (string "\n") $ Lists.map (lambda "line" $ Strings.cat2 (string "// ") (var "line")) (Strings.lines $ var "s")


-- ============================================================================
-- With Comments Variants
-- ============================================================================

moduleItemWithCommentsToExpr :: TBinding (JS.ModuleItemWithComments -> Expr)
moduleItemWithCommentsToExpr = define "moduleItemWithCommentsToExpr" $
  doc "Convert a module item with comments to an AST expression" $
  lambda "miwc" $ lets [
    "body">: project JS._ModuleItemWithComments JS._ModuleItemWithComments_body @@ var "miwc",
    "mc">: project JS._ModuleItemWithComments JS._ModuleItemWithComments_comments @@ var "miwc"] $
    Maybes.maybe
      (moduleItemToExpr @@ var "body")
      (lambda "c" $ Serialization.newlineSep @@ list [
        documentationCommentToExpr @@ var "c",
        moduleItemToExpr @@ var "body"])
      (var "mc")

functionDeclarationWithCommentsToExpr :: TBinding (JS.FunctionDeclarationWithComments -> Expr)
functionDeclarationWithCommentsToExpr = define "functionDeclarationWithCommentsToExpr" $
  doc "Convert a function declaration with comments to an AST expression" $
  lambda "fdwc" $ lets [
    "body">: project JS._FunctionDeclarationWithComments JS._FunctionDeclarationWithComments_body @@ var "fdwc",
    "mc">: project JS._FunctionDeclarationWithComments JS._FunctionDeclarationWithComments_comments @@ var "fdwc"] $
    Maybes.maybe
      (functionDeclarationToExpr @@ var "body")
      (lambda "c" $ Serialization.newlineSep @@ list [
        documentationCommentToExpr @@ var "c",
        functionDeclarationToExpr @@ var "body"])
      (var "mc")

classDeclarationWithCommentsToExpr :: TBinding (JS.ClassDeclarationWithComments -> Expr)
classDeclarationWithCommentsToExpr = define "classDeclarationWithCommentsToExpr" $
  doc "Convert a class declaration with comments to an AST expression" $
  lambda "cdwc" $ lets [
    "body">: project JS._ClassDeclarationWithComments JS._ClassDeclarationWithComments_body @@ var "cdwc",
    "mc">: project JS._ClassDeclarationWithComments JS._ClassDeclarationWithComments_comments @@ var "cdwc"] $
    Maybes.maybe
      (classDeclarationToExpr @@ var "body")
      (lambda "c" $ Serialization.newlineSep @@ list [
        documentationCommentToExpr @@ var "c",
        classDeclarationToExpr @@ var "body"])
      (var "mc")
