-- | Serialization functions for TypeScript AST to abstract syntax expressions.
--
-- This module provides functions to convert TypeScript AST types to Hydra's
-- abstract Expr type, which can then be rendered to concrete syntax.

module Hydra.Sources.TypeScript.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.TypeScript.Syntax as TS
import qualified Hydra.Sources.TypeScript.Syntax as TypeScriptSyntax
import qualified Hydra.Sources.TypeScript.Operators as TypeScriptOperators


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.typeScript.serde"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Constants.ns, Serialization.ns, TypeScriptOperators.ns] L.++ (TypeScriptSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Serialization functions for converting TypeScript AST to abstract expressions")}
  where
    definitions = [
      -- Core conversions
      toDefinition identifierToExpr,
      toDefinition literalToExpr,
      toDefinition stringLiteralToExpr,
      toDefinition escapeString,
      toDefinition templateLiteralToExpr,
      toDefinition numericLiteralToExpr,

      -- Expression conversions
      toDefinition expressionToExpr,
      toDefinition arrayExpressionToExpr,
      toDefinition arrayElementToExpr,
      toDefinition objectExpressionToExpr,
      toDefinition propertyToExpr,
      toDefinition functionExpressionToExpr,
      toDefinition arrowFunctionExpressionToExpr,
      toDefinition callExpressionToExpr,
      toDefinition memberExpressionToExpr,
      toDefinition conditionalExpressionToExpr,
      toDefinition binaryExpressionToExpr,
      toDefinition unaryExpressionToExpr,
      toDefinition assignmentExpressionToExpr,

      -- Pattern conversions
      toDefinition patternToExpr,
      toDefinition objectPatternToExpr,
      toDefinition objectPatternPropertyToExpr,
      toDefinition arrayPatternToExpr,
      toDefinition assignmentPatternToExpr,
      toDefinition typedPatternToExpr,
      toDefinition patternToString,
      toDefinition tsTypeExpressionToString,
      toDefinition isKernelTypeVarName,
      toDefinition allDigits,

      -- Statement conversions
      toDefinition statementToExpr,
      toDefinition blockStatementToExpr,
      toDefinition variableDeclarationToExpr,
      toDefinition variableDeclaratorToExpr,
      toDefinition variableKindToExpr,
      toDefinition ifStatementToExpr,
      toDefinition switchStatementToExpr,
      toDefinition switchCaseToExpr,
      toDefinition returnStatementToExpr,
      toDefinition throwStatementToExpr,
      toDefinition tryStatementToExpr,
      toDefinition catchClauseToExpr,
      toDefinition breakStatementToExpr,
      toDefinition continueStatementToExpr,
      toDefinition whileStatementToExpr,
      toDefinition doWhileStatementToExpr,
      toDefinition forStatementToExpr,
      toDefinition forInStatementToExpr,
      toDefinition forOfStatementToExpr,
      toDefinition labeledStatementToExpr,

      -- Declaration conversions
      toDefinition functionDeclarationToExpr,
      toDefinition classDeclarationToExpr,
      toDefinition methodDefinitionToExpr,

      -- Module conversions
      toDefinition programToExpr,
      toDefinition moduleItemToExpr,
      toDefinition importDeclarationToExpr,
      toDefinition importSpecifierToExpr,
      toDefinition formatImportSpecifiers,
      toDefinition exportDeclarationToExpr,
      toDefinition namedExportToExpr,
      toDefinition exportSpecifierToExpr,
      toDefinition exportAllToExpr,

      -- Operators
      toDefinition binaryOperatorToExpr,
      toDefinition unaryOperatorToString,
      toDefinition assignmentOperatorToString,

      -- Comments
      toDefinition documentationCommentToExpr,
      toDefinition toTypeScriptComments,
      toDefinition documentationTagToLine,
      toDefinition typeExpressionToString,
      toDefinition toLineComment,

      -- With comments variants
      toDefinition moduleItemWithCommentsToExpr,
      toDefinition functionDeclarationWithCommentsToExpr,
      toDefinition classDeclarationWithCommentsToExpr]


-- ============================================================================
-- Core Conversions
-- ============================================================================

-- | True iff every codepoint is an ASCII digit '0'..'9' (and the list is non-empty).
allDigits :: TypedTermDefinition ([Int] -> Bool)
allDigits = define "allDigits" $
  lambda "cps" $
    Logic.and
      (Equality.gt (Lists.length (var "cps")) (int32 0))
      (Lists.foldl (lambda "acc" $ lambda "c" $
        Logic.and (var "acc")
          (Logic.and
            (Equality.gte (var "c") (int32 48))
            (Equality.lte (var "c") (int32 57))))
        (boolean True)
        (var "cps"))

arrayElementToExpr :: TypedTermDefinition (TS.ArrayElement -> Expr)
arrayElementToExpr = define "arrayElementToExpr" $
  doc "Convert an array element to an AST expression" $
  lambda "elem" $
    cases TS._ArrayElement (var "elem") Nothing [
      TS._ArrayElement_expression>>: lambda "e" $ expressionToExpr @@ var "e",
      TS._ArrayElement_spread>>: lambda "s" $
        Serialization.prefix @@ string "..." @@ (expressionToExpr @@ (unwrap TS._SpreadElement @@ var "s")),
      TS._ArrayElement_hole>>: constant $ Serialization.cst @@ string ""]

arrayExpressionToExpr :: TypedTermDefinition (TS.ArrayExpression -> Expr)
arrayExpressionToExpr = define "arrayExpressionToExpr" $
  doc "Convert an array expression to an AST expression" $
  lambda "arr" $
    Serialization.bracketList @@ Serialization.inlineStyle @@
      (Lists.map (arrayElementToExpr) (var "arr"))

arrayPatternToExpr :: TypedTermDefinition (TS.ArrayPattern -> Expr)
arrayPatternToExpr = define "arrayPatternToExpr" $
  doc "Convert an array pattern to an AST expression" $
  lambda "arr" $
    Serialization.bracketList @@ Serialization.inlineStyle @@
      (Lists.map
        (lambda "maybeP" $ Optionals.cases (var "maybeP") (Serialization.cst @@ string "") (patternToExpr))
        (var "arr"))

arrowFunctionExpressionToExpr :: TypedTermDefinition (TS.ArrowFunctionExpression -> Expr)
arrowFunctionExpressionToExpr = define "arrowFunctionExpressionToExpr" $
  doc "Convert an arrow function expression to an AST expression" $
  lambda "arrow" $ lets [
    "params">: project TS._ArrowFunctionExpression TS._ArrowFunctionExpression_params @@ var "arrow",
    "body">: project TS._ArrowFunctionExpression TS._ArrowFunctionExpression_body @@ var "arrow",
    "async">: project TS._ArrowFunctionExpression TS._ArrowFunctionExpression_async @@ var "arrow",
    "asyncKw">: Logic.ifElse (var "async")
      (list [Serialization.cst @@ string "async"])
      (list ([] :: [TypedTerm Expr])),
    -- Always parenthesize. A bare untyped identifier could appear without
    -- parens (`x => x`), but a typed pattern (`(x: T) => x`) requires them.
    -- Defaulting to parens keeps the emitter simple.
    "paramsExpr">: Serialization.parenListAdaptive @@ (Lists.map (patternToExpr) (var "params")),
    "bodyExpr">: cases TS._ArrowFunctionBody (var "body") Nothing [
      -- An object-literal body needs explicit parens: `() => { ... }` would
      -- otherwise be parsed as an arrow with a block body. Same for sequence
      -- expressions (`a, b, c`) which would shadow the arrow.
      TS._ArrowFunctionBody_expression>>: lambda "e" $
        cases TS._Expression (var "e") (Just $ expressionToExpr @@ var "e") [
          TS._Expression_object>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "e"),
          TS._Expression_sequence>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "e")],
      TS._ArrowFunctionBody_block>>: lambda "b" $ blockStatementToExpr @@ var "b"]] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "asyncKw",
      list [Serialization.ifx @@ TypeScriptOperators.arrowOp @@ var "paramsExpr" @@ var "bodyExpr"]])

assignmentExpressionToExpr :: TypedTermDefinition (TS.AssignmentExpression -> Expr)
assignmentExpressionToExpr = define "assignmentExpressionToExpr" $
  doc "Convert an assignment expression to an AST expression" $
  lambda "assign" $ lets [
    "op">: project TS._AssignmentExpression TS._AssignmentExpression_operator @@ var "assign",
    "left">: project TS._AssignmentExpression TS._AssignmentExpression_left @@ var "assign",
    "right">: project TS._AssignmentExpression TS._AssignmentExpression_right @@ var "assign",
    "opStr">: assignmentOperatorToString @@ var "op"] $
    Serialization.spaceSep @@ list [
      patternToExpr @@ var "left",
      Serialization.cst @@ var "opStr",
      expressionToExpr @@ var "right"]


-- ============================================================================
-- Pattern Conversions
-- ============================================================================

assignmentOperatorToString :: TypedTermDefinition (TS.AssignmentOperator -> String)
assignmentOperatorToString = define "assignmentOperatorToString" $
  doc "Convert an assignment operator to a string" $
  lambda "op" $
    cases TS._AssignmentOperator (var "op") Nothing [
      TS._AssignmentOperator_assign>>: constant $ string "=",
      TS._AssignmentOperator_addAssign>>: constant $ string "+=",
      TS._AssignmentOperator_subtractAssign>>: constant $ string "-=",
      TS._AssignmentOperator_multiplyAssign>>: constant $ string "*=",
      TS._AssignmentOperator_divideAssign>>: constant $ string "/=",
      TS._AssignmentOperator_moduloAssign>>: constant $ string "%=",
      TS._AssignmentOperator_exponentiateAssign>>: constant $ string "**=",
      TS._AssignmentOperator_leftShiftAssign>>: constant $ string "<<=",
      TS._AssignmentOperator_rightShiftAssign>>: constant $ string ">>=",
      TS._AssignmentOperator_unsignedRightShiftAssign>>: constant $ string ">>>=",
      TS._AssignmentOperator_bitwiseAndAssign>>: constant $ string "&=",
      TS._AssignmentOperator_bitwiseOrAssign>>: constant $ string "|=",
      TS._AssignmentOperator_bitwiseXorAssign>>: constant $ string "^=",
      TS._AssignmentOperator_andAssign>>: constant $ string "&&=",
      TS._AssignmentOperator_orAssign>>: constant $ string "||=",
      TS._AssignmentOperator_nullishAssign>>: constant $ string "??="]


-- ============================================================================
-- Comments
-- ============================================================================

assignmentPatternToExpr :: TypedTermDefinition (TS.AssignmentPattern -> Expr)
assignmentPatternToExpr = define "assignmentPatternToExpr" $
  doc "Convert an assignment pattern to an AST expression" $
  lambda "assign" $ lets [
    "left">: project TS._AssignmentPattern TS._AssignmentPattern_left @@ var "assign",
    "right">: project TS._AssignmentPattern TS._AssignmentPattern_right @@ var "assign"] $
    Serialization.ifx @@ TypeScriptOperators.defineOp @@
      (patternToExpr @@ var "left") @@
      (expressionToExpr @@ var "right")


-- ============================================================================
-- Statement Conversions
-- ============================================================================

binaryExpressionToExpr :: TypedTermDefinition (TS.BinaryExpression -> Expr)
binaryExpressionToExpr = define "binaryExpressionToExpr" $
  doc "Convert a binary expression to an AST expression" $
  lambda "bin" $ lets [
    "op">: project TS._BinaryExpression TS._BinaryExpression_operator @@ var "bin",
    "left">: project TS._BinaryExpression TS._BinaryExpression_left @@ var "bin",
    "right">: project TS._BinaryExpression TS._BinaryExpression_right @@ var "bin"] $
    Serialization.ifx @@
      (binaryOperatorToExpr @@ var "op") @@
      (expressionToExpr @@ var "left") @@
      (expressionToExpr @@ var "right")

binaryOperatorToExpr :: TypedTermDefinition (TS.BinaryOperator -> Op)
binaryOperatorToExpr = define "binaryOperatorToExpr" $
  doc "Convert a binary operator to an Op" $
  lambda "op" $
    cases TS._BinaryOperator (var "op") Nothing [
      TS._BinaryOperator_add>>: constant TypeScriptOperators.addOp,
      TS._BinaryOperator_subtract>>: constant TypeScriptOperators.subtractOp,
      TS._BinaryOperator_multiply>>: constant TypeScriptOperators.multiplyOp,
      TS._BinaryOperator_divide>>: constant TypeScriptOperators.divideOp,
      TS._BinaryOperator_modulo>>: constant TypeScriptOperators.moduloOp,
      TS._BinaryOperator_exponentiate>>: constant TypeScriptOperators.exponentiateOp,
      TS._BinaryOperator_equal>>: constant TypeScriptOperators.equalOp,
      TS._BinaryOperator_notEqual>>: constant TypeScriptOperators.notEqualOp,
      TS._BinaryOperator_strictEqual>>: constant TypeScriptOperators.strictEqualOp,
      TS._BinaryOperator_strictNotEqual>>: constant TypeScriptOperators.strictNotEqualOp,
      TS._BinaryOperator_lessThan>>: constant TypeScriptOperators.lessThanOp,
      TS._BinaryOperator_lessThanOrEqual>>: constant TypeScriptOperators.lessThanOrEqualOp,
      TS._BinaryOperator_greaterThan>>: constant TypeScriptOperators.greaterThanOp,
      TS._BinaryOperator_greaterThanOrEqual>>: constant TypeScriptOperators.greaterThanOrEqualOp,
      TS._BinaryOperator_and>>: constant TypeScriptOperators.logicalAndOp,
      TS._BinaryOperator_or>>: constant TypeScriptOperators.logicalOrOp,
      TS._BinaryOperator_nullishCoalescing>>: constant TypeScriptOperators.nullishCoalescingOp,
      TS._BinaryOperator_bitwiseAnd>>: constant TypeScriptOperators.bitwiseAndOp,
      TS._BinaryOperator_bitwiseOr>>: constant TypeScriptOperators.bitwiseOrOp,
      TS._BinaryOperator_bitwiseXor>>: constant TypeScriptOperators.bitwiseXorOp,
      TS._BinaryOperator_leftShift>>: constant TypeScriptOperators.leftShiftOp,
      TS._BinaryOperator_rightShift>>: constant TypeScriptOperators.rightShiftOp,
      TS._BinaryOperator_unsignedRightShift>>: constant TypeScriptOperators.unsignedRightShiftOp,
      TS._BinaryOperator_in>>: constant TypeScriptOperators.inOp,
      TS._BinaryOperator_instanceof>>: constant TypeScriptOperators.instanceOfOp]

blockStatementToExpr :: TypedTermDefinition (TS.BlockStatement -> Expr)
blockStatementToExpr = define "blockStatementToExpr" $
  doc "Convert a block statement to an AST expression. Renders as `{ stmt1\\n stmt2\\n ... }` using curlyBlock + newlineSep: statements are separated by newlines, NOT by commas (which curlyBracesList's default would insert and which TypeScript rejects between block statements)." $
  lambda "block" $
    Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.newlineSep @@ (Lists.map (statementToExpr) (var "block")))

breakStatementToExpr :: TypedTermDefinition (TS.BreakStatement -> Expr)
breakStatementToExpr = define "breakStatementToExpr" $
  doc "Convert a break statement to an AST expression" $
  lambda "b" $
    Optionals.cases (var "b") (Serialization.cst @@ string "break;") (lambda "label" $ Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "break",
          identifierToExpr @@ var "label"]))

callExpressionToExpr :: TypedTermDefinition (TS.CallExpression -> Expr)
callExpressionToExpr = define "callExpressionToExpr" $
  doc "Convert a call expression to an AST expression" $
  lambda "call" $ lets [
    "callee">: project TS._CallExpression TS._CallExpression_callee @@ var "call",
    "args">: project TS._CallExpression TS._CallExpression_arguments @@ var "call",
    "optional">: project TS._CallExpression TS._CallExpression_optional @@ var "call",
    -- A bare arrow / conditional / binary / sequence / new can't appear in
    -- callee position without parens: `x => x.body(arg)` parses as
    -- `x => (x.body(arg))`, not `(x => x.body)(arg)`. Wrap those forms.
    "calleeExpr">: cases TS._Expression (var "callee") (Just $ expressionToExpr @@ var "callee") [
      TS._Expression_arrow>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_conditional>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_binary>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_unary>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_assignment>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_sequence>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_object>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee"),
      TS._Expression_function>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "callee")],
    "argsExpr">: Serialization.parenListAdaptive @@ (Lists.map (expressionToExpr) (var "args")),
    "optionalDot">: Logic.ifElse (var "optional") (string "?.") (string "")] $
    Serialization.spaceSep @@ list [
      var "calleeExpr",
      Serialization.cst @@ var "optionalDot",
      var "argsExpr"]

catchClauseToExpr :: TypedTermDefinition (TS.CatchClause -> Expr)
catchClauseToExpr = define "catchClauseToExpr" $
  doc "Convert a catch clause to an AST expression" $
  lambda "c" $ lets [
    "param">: project TS._CatchClause TS._CatchClause_param @@ var "c",
    "body">: project TS._CatchClause TS._CatchClause_body @@ var "c",
    "catchKw">: Optionals.cases (var "param") (Serialization.cst @@ string "catch") (lambda "p" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "catch",
        Serialization.parens @@ (patternToExpr @@ var "p")])] $
    Serialization.spaceSep @@ list [var "catchKw", blockStatementToExpr @@ var "body"]

classDeclarationToExpr :: TypedTermDefinition (TS.ClassDeclaration -> Expr)
classDeclarationToExpr = define "classDeclarationToExpr" $
  doc "Convert a class declaration to an AST expression" $
  lambda "cls" $ lets [
    "id">: project TS._ClassDeclaration TS._ClassDeclaration_id @@ var "cls",
    "superClass">: project TS._ClassDeclaration TS._ClassDeclaration_superClass @@ var "cls",
    "body">: project TS._ClassDeclaration TS._ClassDeclaration_body @@ var "cls",
    "extendsClause">: Optionals.cases (var "superClass") (list ([] :: [TypedTerm Expr])) (lambda "s" $ list [Serialization.cst @@ string "extends", expressionToExpr @@ var "s"]),
    "bodyExpr">: Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
      (Serialization.newlineSep @@ (Lists.map (methodDefinitionToExpr) (var "body")))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      list [Serialization.cst @@ string "class", identifierToExpr @@ var "id"],
      var "extendsClause",
      list [var "bodyExpr"]])

classDeclarationWithCommentsToExpr :: TypedTermDefinition (TS.ClassDeclarationWithComments -> Expr)
classDeclarationWithCommentsToExpr = define "classDeclarationWithCommentsToExpr" $
  doc "Convert a class declaration with comments to an AST expression" $
  lambda "cdwc" $ lets [
    "body">: project TS._ClassDeclarationWithComments TS._ClassDeclarationWithComments_body @@ var "cdwc",
    "mc">: project TS._ClassDeclarationWithComments TS._ClassDeclarationWithComments_comments @@ var "cdwc"] $
    Optionals.cases (var "mc") (classDeclarationToExpr @@ var "body") (lambda "c" $ Serialization.newlineSep @@ list [
        documentationCommentToExpr @@ var "c",
        classDeclarationToExpr @@ var "body"])

conditionalExpressionToExpr :: TypedTermDefinition (TS.ConditionalExpression -> Expr)
conditionalExpressionToExpr = define "conditionalExpressionToExpr" $
  doc "Convert a conditional expression to an AST expression" $
  lambda "cond" $ lets [
    "test">: project TS._ConditionalExpression TS._ConditionalExpression_test @@ var "cond",
    "consequent">: project TS._ConditionalExpression TS._ConditionalExpression_consequent @@ var "cond",
    "alternate">: project TS._ConditionalExpression TS._ConditionalExpression_alternate @@ var "cond",
    -- The consequent in `cond ? <obj> : ...` would be parsed as
    -- a block statement if it began with `{`. Parenthesize object literals
    -- (and sequences) on either side. Nested conditionals don't need parens
    -- because `?:` is right-associative.
    "consExpr">: cases TS._Expression (var "consequent")
      (Just $ expressionToExpr @@ var "consequent") [
        TS._Expression_object>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "consequent"),
        TS._Expression_sequence>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "consequent")],
    "altExpr">: cases TS._Expression (var "alternate")
      (Just $ expressionToExpr @@ var "alternate") [
        TS._Expression_object>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "alternate"),
        TS._Expression_sequence>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "alternate")]] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "test",
      Serialization.cst @@ string "?",
      var "consExpr",
      Serialization.cst @@ string ":",
      var "altExpr"]

continueStatementToExpr :: TypedTermDefinition (TS.ContinueStatement -> Expr)
continueStatementToExpr = define "continueStatementToExpr" $
  doc "Convert a continue statement to an AST expression" $
  lambda "c" $
    Optionals.cases (var "c") (Serialization.cst @@ string "continue;") (lambda "label" $ Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "continue",
          identifierToExpr @@ var "label"]))

doWhileStatementToExpr :: TypedTermDefinition (TS.DoWhileStatement -> Expr)
doWhileStatementToExpr = define "doWhileStatementToExpr" $
  doc "Convert a do-while statement to an AST expression" $
  lambda "d" $ lets [
    "body">: project TS._DoWhileStatement TS._DoWhileStatement_body @@ var "d",
    "test">: project TS._DoWhileStatement TS._DoWhileStatement_test @@ var "d"] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "do",
        statementToExpr @@ var "body",
        Serialization.cst @@ string "while",
        Serialization.parens @@ (expressionToExpr @@ var "test")])

documentationCommentToExpr :: TypedTermDefinition (TS.DocumentationComment -> Expr)
documentationCommentToExpr = define "documentationCommentToExpr" $
  doc "Convert a documentation comment to an AST expression" $
  lambda "doc" $ lets [
    "description">: project TS._DocumentationComment TS._DocumentationComment_description @@ var "doc",
    "tags">: project TS._DocumentationComment TS._DocumentationComment_tags @@ var "doc"] $
    Serialization.cst @@ (toTypeScriptComments @@ var "description" @@ var "tags")

documentationTagToLine :: TypedTermDefinition (TS.DocumentationTag -> String)
documentationTagToLine = define "documentationTagToLine" $
  doc ("Convert a documentation tag to a JSDoc line. Built by joining"
    <> " non-empty parts with spaces so that absent type/param/description"
    <> " components don't introduce trailing whitespace.") $
  lambda "tag" $ lets [
    "name">: project TS._DocumentationTag TS._DocumentationTag_name @@ var "tag",
    "mtype">: project TS._DocumentationTag TS._DocumentationTag_type @@ var "tag",
    "mparamName">: project TS._DocumentationTag TS._DocumentationTag_paramName @@ var "tag",
    "description">: project TS._DocumentationTag TS._DocumentationTag_description @@ var "tag",
    "typePart">: Optionals.cases (var "mtype") (string "") (lambda "t" $ Strings.cat $ list [string "{", typeExpressionToString @@ var "t", string "}"]),
    "paramPart">: Optionals.cases (var "mparamName") (string "") (lambda "p" $ unwrap TS._Identifier @@ var "p"),
    "parts">: list [
      Strings.cat2 (string "@") (var "name"),
      var "typePart",
      var "paramPart",
      var "description"],
    "nonEmpty">: Lists.filter (lambda "p" $ Logic.not $ Equality.equal (var "p") (string "")) (var "parts")] $
    Strings.cat2 (string " * ") (Strings.intercalate (string " ") (var "nonEmpty"))

escapeString :: TypedTermDefinition (String -> Bool -> String)
escapeString = define "escapeString" $
  doc "Escape special characters in a string for TypeScript" $
  lambda "s" $ lambda "singleQuote" $ lets [
    "replace">: lambda "old" $ lambda "new_" $ lambda "str" $
      Strings.intercalate (var "new_") (Strings.splitOn (var "old") (var "str")),
    "s1">: var "replace" @@ string "\\" @@ string "\\\\" @@ var "s",
    "s2">: var "replace" @@ string "\n" @@ string "\\n" @@ var "s1",
    "s3">: var "replace" @@ string "\r" @@ string "\\r" @@ var "s2",
    "s4">: var "replace" @@ string "\t" @@ string "\\t" @@ var "s3"] $
    Logic.ifElse (var "singleQuote")
      (var "replace" @@ string "'"  @@ string "\\'" @@ var "s4")
      (var "replace" @@ string "\"" @@ string "\\\"" @@ var "s4")

exportAllToExpr :: TypedTermDefinition (TS.ExportAllDeclaration -> Expr)
exportAllToExpr = define "exportAllToExpr" $
  doc "Convert an export all declaration to an AST expression" $
  lambda "a" $ lets [
    "exported">: project TS._ExportAllDeclaration TS._ExportAllDeclaration_exported @@ var "a",
    "source">: project TS._ExportAllDeclaration TS._ExportAllDeclaration_source @@ var "a",
    "exportedClause">: Optionals.cases (var "exported") (Serialization.cst @@ string "*") (lambda "e" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "*",
        Serialization.cst @@ string "as",
        identifierToExpr @@ var "e"])] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "export",
        var "exportedClause",
        Serialization.cst @@ string "from",
        stringLiteralToExpr @@ var "source"])


-- ============================================================================
-- Operators
-- ============================================================================

exportDeclarationToExpr :: TypedTermDefinition (TS.ExportDeclaration -> Expr)
exportDeclarationToExpr = define "exportDeclarationToExpr" $
  doc "Convert an export declaration to an AST expression" $
  lambda "exp" $
    cases TS._ExportDeclaration (var "exp") Nothing [
      TS._ExportDeclaration_named>>: lambda "n" $ namedExportToExpr @@ var "n",
      TS._ExportDeclaration_default>>: lambda "e" $
        Serialization.suffix @@ string ";" @@
          (Serialization.spaceSep @@ list [
            Serialization.cst @@ string "export",
            Serialization.cst @@ string "default",
            expressionToExpr @@ var "e"]),
      TS._ExportDeclaration_declaration>>: lambda "d" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "export",
          statementToExpr @@ var "d"],
      TS._ExportDeclaration_all>>: lambda "a" $ exportAllToExpr @@ var "a"]

exportSpecifierToExpr :: TypedTermDefinition (TS.ExportSpecifier -> Expr)
exportSpecifierToExpr = define "exportSpecifierToExpr" $
  doc "Convert an export specifier to an AST expression" $
  lambda "spec" $ lets [
    "local">: project TS._ExportSpecifier TS._ExportSpecifier_local @@ var "spec",
    "exported">: project TS._ExportSpecifier TS._ExportSpecifier_exported @@ var "spec"] $
    Logic.ifElse (Equality.equal (unwrap TS._Identifier @@ var "local") (unwrap TS._Identifier @@ var "exported"))
      (identifierToExpr @@ var "local")
      (Serialization.spaceSep @@ list [
        identifierToExpr @@ var "local",
        Serialization.cst @@ string "as",
        identifierToExpr @@ var "exported"])

expressionToExpr :: TypedTermDefinition (TS.Expression -> Expr)
expressionToExpr = define "expressionToExpr" $
  doc "Convert a TypeScript expression to an AST expression" $
  lambda "expr" $
    cases TS._Expression (var "expr") Nothing [
      TS._Expression_identifier>>: lambda "id" $ identifierToExpr @@ var "id",
      TS._Expression_literal>>: lambda "lit" $ literalToExpr @@ var "lit",
      TS._Expression_array>>: lambda "arr" $ arrayExpressionToExpr @@ var "arr",
      TS._Expression_object>>: lambda "obj" $ objectExpressionToExpr @@ var "obj",
      TS._Expression_function>>: lambda "fn" $ functionExpressionToExpr @@ var "fn",
      TS._Expression_arrow>>: lambda "arrow" $ arrowFunctionExpressionToExpr @@ var "arrow",
      TS._Expression_call>>: lambda "call" $ callExpressionToExpr @@ var "call",
      TS._Expression_member>>: lambda "mem" $ memberExpressionToExpr @@ var "mem",
      TS._Expression_conditional>>: lambda "cond" $ conditionalExpressionToExpr @@ var "cond",
      TS._Expression_binary>>: lambda "bin" $ binaryExpressionToExpr @@ var "bin",
      TS._Expression_unary>>: lambda "un" $ unaryExpressionToExpr @@ var "un",
      TS._Expression_assignment>>: lambda "assign" $ assignmentExpressionToExpr @@ var "assign",
      TS._Expression_sequence>>: lambda "exprs" $
        Serialization.parenListAdaptive @@ (Lists.map (expressionToExpr) (var "exprs")),
      TS._Expression_this>>: constant $ Serialization.cst @@ string "this",
      TS._Expression_new>>: lambda "call" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "new",
          callExpressionToExpr @@ var "call"],
      TS._Expression_yield>>: lambda "maybeExpr" $
        Optionals.cases (var "maybeExpr") (Serialization.cst @@ string "yield") (lambda "e" $ Serialization.spaceSep @@ list [
            Serialization.cst @@ string "yield",
            expressionToExpr @@ var "e"]),
      TS._Expression_await>>: lambda "e" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "await",
          expressionToExpr @@ var "e"],
      TS._Expression_spread>>: lambda "spread" $
        Serialization.prefix @@ string "..." @@ (expressionToExpr @@ (unwrap TS._SpreadElement @@ var "spread")),
      TS._Expression_parenthesized>>: lambda "e" $
        Serialization.parens @@ (expressionToExpr @@ var "e"),
      -- `<expr> as <type>` — always parenthesize the whole cast, because
      -- the precedence of `as` is lower than most surrounding contexts
      -- (call, member, …).
      TS._Expression_asExpression>>: lambda "ae" $ lets [
        "innerE">: project TS._AsExpression TS._AsExpression_expression @@ var "ae",
        "typ">: project TS._AsExpression TS._AsExpression_type @@ var "ae"] $
        Serialization.parens @@ (Serialization.spaceSep @@ list [
          expressionToExpr @@ var "innerE",
          Serialization.cst @@ string "as",
          Serialization.cst @@ (tsTypeExpressionToString @@ var "typ")])]

forInStatementToExpr :: TypedTermDefinition (TS.ForInStatement -> Expr)
forInStatementToExpr = define "forInStatementToExpr" $
  doc "Convert a for-in statement to an AST expression" $
  lambda "f" $ lets [
    "left">: project TS._ForInStatement TS._ForInStatement_left @@ var "f",
    "right">: project TS._ForInStatement TS._ForInStatement_right @@ var "f",
    "body">: project TS._ForInStatement TS._ForInStatement_body @@ var "f",
    "leftExpr">: cases TS._ForInLeft (var "left") Nothing [
      TS._ForInLeft_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
      TS._ForInLeft_pattern>>: lambda "p" $ patternToExpr @@ var "p"]] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "for",
      Serialization.parens @@ (Serialization.spaceSep @@ list [
        var "leftExpr",
        Serialization.cst @@ string "in",
        expressionToExpr @@ var "right"]),
      statementToExpr @@ var "body"]

forOfStatementToExpr :: TypedTermDefinition (TS.ForOfStatement -> Expr)
forOfStatementToExpr = define "forOfStatementToExpr" $
  doc "Convert a for-of statement to an AST expression" $
  lambda "f" $ lets [
    "await">: project TS._ForOfStatement TS._ForOfStatement_await @@ var "f",
    "left">: project TS._ForOfStatement TS._ForOfStatement_left @@ var "f",
    "right">: project TS._ForOfStatement TS._ForOfStatement_right @@ var "f",
    "body">: project TS._ForOfStatement TS._ForOfStatement_body @@ var "f",
    "forKw">: Logic.ifElse (var "await")
      (Serialization.cst @@ string "for await")
      (Serialization.cst @@ string "for"),
    "leftExpr">: cases TS._ForInLeft (var "left") Nothing [
      TS._ForInLeft_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
      TS._ForInLeft_pattern>>: lambda "p" $ patternToExpr @@ var "p"]] $
    Serialization.spaceSep @@ list [
      var "forKw",
      Serialization.parens @@ (Serialization.spaceSep @@ list [
        var "leftExpr",
        Serialization.cst @@ string "of",
        expressionToExpr @@ var "right"]),
      statementToExpr @@ var "body"]

forStatementToExpr :: TypedTermDefinition (TS.ForStatement -> Expr)
forStatementToExpr = define "forStatementToExpr" $
  doc "Convert a for statement to an AST expression" $
  lambda "f" $ lets [
    "init">: project TS._ForStatement TS._ForStatement_init @@ var "f",
    "test">: project TS._ForStatement TS._ForStatement_test @@ var "f",
    "update">: project TS._ForStatement TS._ForStatement_update @@ var "f",
    "body">: project TS._ForStatement TS._ForStatement_body @@ var "f",
    "initExpr">: Optionals.cases (var "init") (Serialization.cst @@ string "") (lambda "i" $ cases TS._ForInit (var "i") Nothing [
        TS._ForInit_variable>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
        TS._ForInit_expression>>: lambda "e" $ expressionToExpr @@ var "e"]),
    "testExpr">: Optionals.cases (var "test") (Serialization.cst @@ string "") (expressionToExpr),
    "updateExpr">: Optionals.cases (var "update") (Serialization.cst @@ string "") (expressionToExpr)] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "for",
      Serialization.parenListAdaptive @@ list [var "initExpr", var "testExpr", var "updateExpr"],
      statementToExpr @@ var "body"]

formatImportSpecifiers :: TypedTermDefinition ([Expr] -> Expr)
formatImportSpecifiers = define "formatImportSpecifiers" $
  doc "Format import specifiers, handling default vs named imports" $
  lambda "specs" $
    -- Simplified: just wrap named imports in braces
    Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "specs"

functionDeclarationToExpr :: TypedTermDefinition (TS.FunctionDeclaration -> Expr)
functionDeclarationToExpr = define "functionDeclarationToExpr" $
  doc "Convert a function declaration to an AST expression" $
  lambda "fn" $ lets [
    "id">: project TS._FunctionDeclaration TS._FunctionDeclaration_id @@ var "fn",
    "params">: project TS._FunctionDeclaration TS._FunctionDeclaration_params @@ var "fn",
    "body">: project TS._FunctionDeclaration TS._FunctionDeclaration_body @@ var "fn",
    "async">: project TS._FunctionDeclaration TS._FunctionDeclaration_async @@ var "fn",
    "generator">: project TS._FunctionDeclaration TS._FunctionDeclaration_generator @@ var "fn",
    "asyncKw">: Logic.ifElse (var "async") (list [Serialization.cst @@ string "async"]) (list ([] :: [TypedTerm Expr])),
    "funcKw">: Logic.ifElse (var "generator")
      (Serialization.cst @@ string "function*")
      (Serialization.cst @@ string "function"),
    "paramsExpr">: Serialization.parenListAdaptive @@ (Lists.map (patternToExpr) (var "params")),
    -- Inject `: any` between params and body so mutually-recursive nested
    -- functions don't trigger TS7024 ("function lacks return type and is
    -- referenced indirectly in its own return"). The AST has no
    -- return-type slot yet; this is a low-risk post-emission heuristic.
    "retAnnot">: Serialization.cst @@ string ": any"] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "asyncKw",
      list [var "funcKw", identifierToExpr @@ var "id", var "paramsExpr", var "retAnnot", blockStatementToExpr @@ var "body"]])

functionDeclarationWithCommentsToExpr :: TypedTermDefinition (TS.FunctionDeclarationWithComments -> Expr)
functionDeclarationWithCommentsToExpr = define "functionDeclarationWithCommentsToExpr" $
  doc "Convert a function declaration with comments to an AST expression" $
  lambda "fdwc" $ lets [
    "body">: project TS._FunctionDeclarationWithComments TS._FunctionDeclarationWithComments_body @@ var "fdwc",
    "mc">: project TS._FunctionDeclarationWithComments TS._FunctionDeclarationWithComments_comments @@ var "fdwc"] $
    Optionals.cases (var "mc") (functionDeclarationToExpr @@ var "body") (lambda "c" $ Serialization.newlineSep @@ list [
        documentationCommentToExpr @@ var "c",
        functionDeclarationToExpr @@ var "body"])

functionExpressionToExpr :: TypedTermDefinition (TS.FunctionExpression -> Expr)
functionExpressionToExpr = define "functionExpressionToExpr" $
  doc "Convert a function expression to an AST expression" $
  lambda "fn" $ lets [
    "mid">: project TS._FunctionExpression TS._FunctionExpression_id @@ var "fn",
    "params">: project TS._FunctionExpression TS._FunctionExpression_params @@ var "fn",
    "body">: project TS._FunctionExpression TS._FunctionExpression_body @@ var "fn",
    "async">: project TS._FunctionExpression TS._FunctionExpression_async @@ var "fn",
    "generator">: project TS._FunctionExpression TS._FunctionExpression_generator @@ var "fn",
    "asyncKw">: Logic.ifElse (var "async") (list [Serialization.cst @@ string "async"]) (list ([] :: [TypedTerm Expr])),
    "funcKw">: Logic.ifElse (var "generator")
      (Serialization.cst @@ string "function*")
      (Serialization.cst @@ string "function"),
    "nameExpr">: Optionals.cases (var "mid") (list ([] :: [TypedTerm Expr])) (lambda "id" $ list [identifierToExpr @@ var "id"]),
    "paramsExpr">: Serialization.parenListAdaptive @@ (Lists.map (patternToExpr) (var "params"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "asyncKw",
      list [var "funcKw"],
      var "nameExpr",
      list [var "paramsExpr", blockStatementToExpr @@ var "body"]])

identifierToExpr :: TypedTermDefinition (TS.Identifier -> Expr)
identifierToExpr = define "identifierToExpr" $
  doc "Convert an identifier to an AST expression" $
  lambda "id" $ Serialization.cst @@ (unwrap TS._Identifier @@ var "id")

ifStatementToExpr :: TypedTermDefinition (TS.IfStatement -> Expr)
ifStatementToExpr = define "ifStatementToExpr" $
  doc "Convert an if statement to an AST expression" $
  lambda "ifStmt" $ lets [
    "test">: project TS._IfStatement TS._IfStatement_test @@ var "ifStmt",
    "consequent">: project TS._IfStatement TS._IfStatement_consequent @@ var "ifStmt",
    "alternate">: project TS._IfStatement TS._IfStatement_alternate @@ var "ifStmt",
    "ifPart">: Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      Serialization.parens @@ (expressionToExpr @@ var "test"),
      statementToExpr @@ var "consequent"]] $
    Optionals.cases (var "alternate") (var "ifPart") (lambda "alt" $ Serialization.spaceSep @@ list [
        var "ifPart",
        Serialization.cst @@ string "else",
        statementToExpr @@ var "alt"])

importDeclarationToExpr :: TypedTermDefinition (TS.ImportDeclaration -> Expr)
importDeclarationToExpr = define "importDeclarationToExpr" $
  doc "Convert an import declaration to an AST expression" $
  lambda "imp" $ lets [
    "specifiers">: project TS._ImportDeclaration TS._ImportDeclaration_specifiers @@ var "imp",
    "source">: project TS._ImportDeclaration TS._ImportDeclaration_source @@ var "imp",
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

importSpecifierToExpr :: TypedTermDefinition (TS.ImportClause -> Expr)
importSpecifierToExpr = define "importSpecifierToExpr" $
  doc "Convert an import specifier to an AST expression" $
  lambda "spec" $
    cases TS._ImportClause (var "spec") Nothing [
      TS._ImportClause_named>>: lambda "n" $ lets [
        "imported">: project TS._ImportSpecifier TS._ImportSpecifier_imported @@ var "n",
        "local">: project TS._ImportSpecifier TS._ImportSpecifier_local @@ var "n"] $
        Logic.ifElse (Equality.equal (unwrap TS._Identifier @@ var "imported") (unwrap TS._Identifier @@ var "local"))
          (identifierToExpr @@ var "local")
          (Serialization.spaceSep @@ list [
            identifierToExpr @@ var "imported",
            Serialization.cst @@ string "as",
            identifierToExpr @@ var "local"]),
      TS._ImportClause_default>>: lambda "d" $ identifierToExpr @@ (unwrap TS._ImportDefaultSpecifier @@ var "d"),
      TS._ImportClause_namespace>>: lambda "n" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "*",
          Serialization.cst @@ string "as",
          identifierToExpr @@ (unwrap TS._ImportNamespaceSpecifier @@ var "n")]]

-- | Detect a kernel-synthesized type-variable name like `T0`, `T12`, etc.
-- These come from encoding `Type_variable "tN"` via `capitalize`. In inline
-- type annotations they're unbound — the function declaration has no
-- generic-binder clause to introduce them. The renderer substitutes `any`.
-- Implementation walks the codepoint list rather than using a substring
-- builtin (the kernel String DSL doesn't expose one).
isKernelTypeVarName :: TypedTermDefinition (String -> Bool)
isKernelTypeVarName = define "isKernelTypeVarName" $
  lambda "s" $ lets [
    "cps">: Strings.toList (var "s"),
    "len">: Lists.length (var "cps"),
    "first">: Optionals.fromOptional (int32 0) (Lists.maybeHead (var "cps")),
    "rest">: Logic.ifElse (Equality.gt (var "len") (int32 0))
      (Lists.drop (int32 1) (var "cps"))
      (list ([] :: [TypedTerm Int]))] $
    Logic.and
      (Equality.gt (var "len") (int32 1))
      (Logic.and
        (Equality.equal (var "first") (int32 84))     -- 'T' = 84
        (allDigits @@ (var "rest")))

labeledStatementToExpr :: TypedTermDefinition (TS.LabeledStatement -> Expr)
labeledStatementToExpr = define "labeledStatementToExpr" $
  doc "Convert a labeled statement to an AST expression" $
  lambda "l" $ lets [
    "label">: project TS._LabeledStatement TS._LabeledStatement_label @@ var "l",
    "body">: project TS._LabeledStatement TS._LabeledStatement_body @@ var "l"] $
    Serialization.spaceSep @@ list [
      Serialization.suffix @@ string ":" @@ (identifierToExpr @@ var "label"),
      statementToExpr @@ var "body"]


-- ============================================================================
-- Declaration Conversions
-- ============================================================================

literalToExpr :: TypedTermDefinition (TS.Literal -> Expr)
literalToExpr = define "literalToExpr" $
  doc "Convert a literal to an AST expression" $
  lambda "lit" $
    cases TS._Literal (var "lit") Nothing [
      TS._Literal_string>>: lambda "s" $ stringLiteralToExpr @@ var "s",
      TS._Literal_number>>: lambda "n" $ numericLiteralToExpr @@ var "n",
      TS._Literal_boolean>>: lambda "b" $
        Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false")),
      TS._Literal_null>>: constant $ Serialization.cst @@ string "null",
      TS._Literal_undefined>>: constant $ Serialization.cst @@ string "undefined",
      TS._Literal_bigInt>>: lambda "n" $
        Serialization.cst @@ (Strings.cat2 (Literals.showBigint $ var "n") (string "n")),
      TS._Literal_template>>: lambda "t" $ templateLiteralToExpr @@ var "t"]

memberExpressionToExpr :: TypedTermDefinition (TS.MemberExpression -> Expr)
memberExpressionToExpr = define "memberExpressionToExpr" $
  doc "Convert a member expression to an AST expression" $
  lambda "mem" $ lets [
    "obj">: project TS._MemberExpression TS._MemberExpression_object @@ var "mem",
    "prop">: project TS._MemberExpression TS._MemberExpression_property @@ var "mem",
    "computed">: project TS._MemberExpression TS._MemberExpression_computed @@ var "mem",
    "optional">: project TS._MemberExpression TS._MemberExpression_optional @@ var "mem",
    -- Same caveat as in callExpressionToExpr: an unparenthesized arrow /
    -- conditional / binary / object on the LHS of `.` would re-associate
    -- the dot to a sub-expression. Parenthesize them.
    "objExpr">: cases TS._Expression (var "obj") (Just $ expressionToExpr @@ var "obj") [
      TS._Expression_arrow>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_conditional>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_binary>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_unary>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_assignment>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_sequence>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_object>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj"),
      TS._Expression_function>>: constant $ Serialization.parens @@ (expressionToExpr @@ var "obj")],
    "propExpr">: expressionToExpr @@ var "prop"] $
    Logic.ifElse (var "computed")
      -- obj[prop] or obj?.[prop]
      (Serialization.spaceSep @@ list [
        var "objExpr",
        Logic.ifElse (var "optional") (Serialization.cst @@ string "?.") (Serialization.cst @@ string ""),
        Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@ var "propExpr"])
      -- obj.prop or obj?.prop
      (Serialization.ifx @@
        (Logic.ifElse (var "optional") TypeScriptOperators.optionalChainOp TypeScriptOperators.memberOp) @@
        var "objExpr" @@ var "propExpr")

methodDefinitionToExpr :: TypedTermDefinition (TS.MethodDefinition -> Expr)
methodDefinitionToExpr = define "methodDefinitionToExpr" $
  doc "Convert a method definition to an AST expression" $
  lambda "method" $ lets [
    "key">: project TS._MethodDefinition TS._MethodDefinition_key @@ var "method",
    "value">: project TS._MethodDefinition TS._MethodDefinition_value @@ var "method",
    "kind">: project TS._MethodDefinition TS._MethodDefinition_kind @@ var "method",
    "computed">: project TS._MethodDefinition TS._MethodDefinition_computed @@ var "method",
    "static">: project TS._MethodDefinition TS._MethodDefinition_static @@ var "method",
    "staticKw">: Logic.ifElse (var "static") (list [Serialization.cst @@ string "static"]) (list ([] :: [TypedTerm Expr])),
    "kindKw">: cases TS._MethodKind (var "kind") Nothing [
      TS._MethodKind_constructor>>: constant $ list ([] :: [TypedTerm Expr]),
      TS._MethodKind_method>>: constant $ list ([] :: [TypedTerm Expr]),
      TS._MethodKind_get>>: constant $ list [Serialization.cst @@ string "get"],
      TS._MethodKind_set>>: constant $ list [Serialization.cst @@ string "set"]],
    "keyExpr">: Logic.ifElse (var "computed")
      (Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@ (expressionToExpr @@ var "key"))
      (expressionToExpr @@ var "key"),
    "params">: project TS._FunctionExpression TS._FunctionExpression_params @@ var "value",
    "body">: project TS._FunctionExpression TS._FunctionExpression_body @@ var "value",
    "paramsExpr">: Serialization.parenListAdaptive @@ (Lists.map (patternToExpr) (var "params"))] $
    Serialization.spaceSep @@ (Lists.concat $ list [
      var "staticKw",
      var "kindKw",
      list [var "keyExpr", var "paramsExpr", blockStatementToExpr @@ var "body"]])


-- ============================================================================
-- Module Conversions
-- ============================================================================

moduleItemToExpr :: TypedTermDefinition (TS.ModuleItem -> Expr)
moduleItemToExpr = define "moduleItemToExpr" $
  doc "Convert a module item to an AST expression" $
  lambda "item" $
    cases TS._ModuleItem (var "item") Nothing [
      TS._ModuleItem_statement>>: lambda "s" $ statementToExpr @@ var "s",
      TS._ModuleItem_import>>: lambda "i" $ importDeclarationToExpr @@ var "i",
      TS._ModuleItem_export>>: lambda "e" $ exportDeclarationToExpr @@ var "e"]

moduleItemWithCommentsToExpr :: TypedTermDefinition (TS.ModuleItemWithComments -> Expr)
moduleItemWithCommentsToExpr = define "moduleItemWithCommentsToExpr" $
  doc "Convert a module item with comments to an AST expression" $
  lambda "miwc" $ lets [
    "body">: project TS._ModuleItemWithComments TS._ModuleItemWithComments_body @@ var "miwc",
    "mc">: project TS._ModuleItemWithComments TS._ModuleItemWithComments_comments @@ var "miwc"] $
    Optionals.cases (var "mc") (moduleItemToExpr @@ var "body") (lambda "c" $ Serialization.newlineSep @@ list [
        documentationCommentToExpr @@ var "c",
        moduleItemToExpr @@ var "body"])

namedExportToExpr :: TypedTermDefinition (TS.NamedExport -> Expr)
namedExportToExpr = define "namedExportToExpr" $
  doc "Convert a named export to an AST expression" $
  lambda "n" $ lets [
    "specifiers">: project TS._NamedExport TS._NamedExport_specifiers @@ var "n",
    "source">: project TS._NamedExport TS._NamedExport_source @@ var "n",
    "specExprs">: Lists.map (exportSpecifierToExpr) (var "specifiers"),
    "fromClause">: Optionals.cases (var "source") (list ([] :: [TypedTerm Expr])) (lambda "s" $ list [Serialization.cst @@ string "from", stringLiteralToExpr @@ var "s"])] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ (Lists.concat $ list [
        list [Serialization.cst @@ string "export"],
        list [Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@ var "specExprs"],
        var "fromClause"]))

numericLiteralToExpr :: TypedTermDefinition (TS.NumericLiteral -> Expr)
numericLiteralToExpr = define "numericLiteralToExpr" $
  doc "Convert a numeric literal to an AST expression" $
  lambda "n" $
    cases TS._NumericLiteral (var "n") Nothing [
      TS._NumericLiteral_integer>>: lambda "i" $
        Serialization.cst @@ (Literals.showInt64 $ var "i"),
      TS._NumericLiteral_float>>: lambda "f" $
        Serialization.cst @@ (Literals.showFloat64 $ var "f")]


-- ============================================================================
-- Expression Conversions
-- ============================================================================

objectExpressionToExpr :: TypedTermDefinition (TS.ObjectExpression -> Expr)
objectExpressionToExpr = define "objectExpressionToExpr" $
  doc "Convert an object expression to an AST expression" $
  lambda "obj" $
    Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      (Lists.map (propertyToExpr) (var "obj"))

objectPatternPropertyToExpr :: TypedTermDefinition (TS.ObjectPatternProperty -> Expr)
objectPatternPropertyToExpr = define "objectPatternPropertyToExpr" $
  doc "Convert an object pattern property to an AST expression" $
  lambda "prop" $
    cases TS._ObjectPatternProperty (var "prop") Nothing [
      TS._ObjectPatternProperty_property>>: lambda "p" $ propertyToExpr @@ var "p",
      TS._ObjectPatternProperty_rest>>: lambda "r" $
        Serialization.prefix @@ string "..." @@ (patternToExpr @@ (unwrap TS._RestElement @@ var "r"))]

objectPatternToExpr :: TypedTermDefinition (TS.ObjectPattern -> Expr)
objectPatternToExpr = define "objectPatternToExpr" $
  doc "Convert an object pattern to an AST expression" $
  lambda "obj" $ lets [
    "props">: project TS._ObjectPattern TS._ObjectPattern_properties @@ var "obj"] $
    Serialization.curlyBracesList @@ nothing @@ Serialization.inlineStyle @@
      (Lists.map (objectPatternPropertyToExpr) (var "props"))

patternToExpr :: TypedTermDefinition (TS.Pattern -> Expr)
patternToExpr = define "patternToExpr" $
  doc "Convert a pattern to an AST expression" $
  lambda "pat" $
    cases TS._Pattern (var "pat") Nothing [
      TS._Pattern_identifier>>: lambda "id" $ identifierToExpr @@ var "id",
      TS._Pattern_object>>: lambda "obj" $ objectPatternToExpr @@ var "obj",
      TS._Pattern_array>>: lambda "arr" $ arrayPatternToExpr @@ var "arr",
      TS._Pattern_assignment>>: lambda "assign" $ assignmentPatternToExpr @@ var "assign",
      TS._Pattern_rest>>: lambda "rest" $
        Serialization.prefix @@ string "..." @@ (patternToExpr @@ (unwrap TS._RestElement @@ var "rest")),
      TS._Pattern_typed>>: lambda "tp" $ typedPatternToExpr @@ var "tp"]

-- | Render a pattern as a string. Used by typedPatternToExpr; for parameter
-- contexts the inner pattern is almost always an identifier. Destructuring
-- patterns fall back to "_" since they are not currently produced by the
-- TypeScript coder.
patternToString :: TypedTermDefinition (TS.Pattern -> String)
patternToString = define "patternToString" $
  doc "Render a TS.Pattern as a plain string" $
  lambda "pat" $
    cases TS._Pattern (var "pat") (Just $ string "_") [
      TS._Pattern_identifier>>: lambda "id" $ unwrap TS._Identifier @@ var "id",
      TS._Pattern_rest>>: lambda "rest" $
        Strings.cat2 (string "...") (patternToString @@ (unwrap TS._RestElement @@ var "rest")),
      TS._Pattern_typed>>: lambda "tp2" $
        Strings.cat (list [
          patternToString @@ (project TS._TypedPattern TS._TypedPattern_pattern @@ var "tp2"),
          string ": ",
          tsTypeExpressionToString @@ (project TS._TypedPattern TS._TypedPattern_type @@ var "tp2")])]

programToExpr :: TypedTermDefinition (TS.Program -> Expr)
programToExpr = define "programToExpr" $
  doc "Convert a TypeScript program to an AST expression" $
  lambda "prog" $ lets [
    "body">: project TS._Program TS._Program_body @@ var "prog",
    "warning">: list [Serialization.cst @@ (toLineComment @@ Constants.warningAutoGeneratedFile)],
    "items">: Lists.map (moduleItemToExpr) (var "body")] $
    Serialization.doubleNewlineSep @@ (Lists.concat $ list [var "warning", var "items"])

propertyToExpr :: TypedTermDefinition (TS.Property -> Expr)
propertyToExpr = define "propertyToExpr" $
  doc "Convert an object property to an AST expression" $
  lambda "prop" $ lets [
    "key">: project TS._Property TS._Property_key @@ var "prop",
    "value">: project TS._Property TS._Property_value @@ var "prop",
    "shorthand">: project TS._Property TS._Property_shorthand @@ var "prop",
    "computed">: project TS._Property TS._Property_computed @@ var "prop",
    "keyExpr">: Logic.ifElse (var "computed")
      (Serialization.brackets @@ Serialization.squareBrackets @@ Serialization.inlineStyle @@ (expressionToExpr @@ var "key"))
      (expressionToExpr @@ var "key")] $
    Logic.ifElse (var "shorthand")
      (var "keyExpr")
      (Serialization.ifx @@ TypeScriptOperators.colonOp @@ var "keyExpr" @@ (expressionToExpr @@ var "value"))

returnStatementToExpr :: TypedTermDefinition (TS.ReturnStatement -> Expr)
returnStatementToExpr = define "returnStatementToExpr" $
  doc "Convert a return statement to an AST expression" $
  lambda "r" $
    Optionals.cases (var "r") (Serialization.cst @@ string "return;") (lambda "e" $ Serialization.suffix @@ string ";" @@
        (Serialization.spaceSep @@ list [
          Serialization.cst @@ string "return",
          expressionToExpr @@ var "e"]))

statementToExpr :: TypedTermDefinition (TS.Statement -> Expr)
statementToExpr = define "statementToExpr" $
  doc "Convert a statement to an AST expression" $
  lambda "stmt" $
    cases TS._Statement (var "stmt") Nothing [
      TS._Statement_expression>>: lambda "e" $
        Serialization.suffix @@ string ";" @@ (expressionToExpr @@ var "e"),
      TS._Statement_block>>: lambda "b" $ blockStatementToExpr @@ var "b",
      TS._Statement_empty>>: constant $ Serialization.cst @@ string ";",
      TS._Statement_debugger>>: constant $ Serialization.cst @@ string "debugger;",
      TS._Statement_return>>: lambda "r" $ returnStatementToExpr @@ var "r",
      TS._Statement_break>>: lambda "b" $ breakStatementToExpr @@ var "b",
      TS._Statement_continue>>: lambda "c" $ continueStatementToExpr @@ var "c",
      TS._Statement_if>>: lambda "i" $ ifStatementToExpr @@ var "i",
      TS._Statement_switch>>: lambda "s" $ switchStatementToExpr @@ var "s",
      TS._Statement_throw>>: lambda "t" $ throwStatementToExpr @@ var "t",
      TS._Statement_try>>: lambda "t" $ tryStatementToExpr @@ var "t",
      TS._Statement_while>>: lambda "w" $ whileStatementToExpr @@ var "w",
      TS._Statement_doWhile>>: lambda "d" $ doWhileStatementToExpr @@ var "d",
      TS._Statement_for>>: lambda "f" $ forStatementToExpr @@ var "f",
      TS._Statement_forIn>>: lambda "f" $ forInStatementToExpr @@ var "f",
      TS._Statement_forOf>>: lambda "f" $ forOfStatementToExpr @@ var "f",
      TS._Statement_variableDeclaration>>: lambda "v" $ variableDeclarationToExpr @@ var "v",
      TS._Statement_functionDeclaration>>: lambda "f" $ functionDeclarationToExpr @@ var "f",
      TS._Statement_classDeclaration>>: lambda "c" $ classDeclarationToExpr @@ var "c",
      TS._Statement_labeled>>: lambda "l" $ labeledStatementToExpr @@ var "l"]

stringLiteralToExpr :: TypedTermDefinition (TS.StringLiteral -> Expr)
stringLiteralToExpr = define "stringLiteralToExpr" $
  doc "Convert a string literal to an AST expression" $
  lambda "s" $ lets [
    "value">: project TS._StringLiteral TS._StringLiteral_value @@ var "s",
    "singleQuote">: project TS._StringLiteral TS._StringLiteral_singleQuote @@ var "s",
    "quote">: Logic.ifElse (var "singleQuote") (string "'") (string "\""),
    "escaped">: escapeString @@ var "value" @@ var "singleQuote"] $
    Serialization.cst @@ (Strings.cat $ list [var "quote", var "escaped", var "quote"])

switchCaseToExpr :: TypedTermDefinition (TS.SwitchCase -> Expr)
switchCaseToExpr = define "switchCaseToExpr" $
  doc "Convert a switch case to an AST expression" $
  lambda "c" $ lets [
    "test">: project TS._SwitchCase TS._SwitchCase_test @@ var "c",
    "consequent">: project TS._SwitchCase TS._SwitchCase_consequent @@ var "c",
    "caseLabel">: Optionals.cases (var "test") (Serialization.cst @@ string "default:") (lambda "t" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "case",
        expressionToExpr @@ var "t",
        Serialization.cst @@ string ":"])] $
    Serialization.newlineSep @@ (Lists.cons (var "caseLabel") (Lists.map (statementToExpr) (var "consequent")))

switchStatementToExpr :: TypedTermDefinition (TS.SwitchStatement -> Expr)
switchStatementToExpr = define "switchStatementToExpr" $
  doc "Convert a switch statement to an AST expression" $
  lambda "switchStmt" $ lets [
    "discriminant">: project TS._SwitchStatement TS._SwitchStatement_discriminant @@ var "switchStmt",
    "cases">: project TS._SwitchStatement TS._SwitchStatement_cases @@ var "switchStmt"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "switch",
      Serialization.parens @@ (expressionToExpr @@ var "discriminant"),
      Serialization.curlyBlock @@ Serialization.fullBlockStyle @@
        (Serialization.newlineSep @@ (Lists.map (switchCaseToExpr) (var "cases")))]

templateLiteralToExpr :: TypedTermDefinition (TS.TemplateLiteral -> Expr)
templateLiteralToExpr = define "templateLiteralToExpr" $
  doc "Convert a template literal to an AST expression" $
  lambda "t" $ lets [
    "quasis">: project TS._TemplateLiteral TS._TemplateLiteral_quasis @@ var "t",
    "exprs">: project TS._TemplateLiteral TS._TemplateLiteral_expressions @@ var "t"] $
    -- For now, just output the first quasi if no expressions
    -- Full implementation would interleave quasis and expressions
    Serialization.cst @@ (Strings.cat $ list [
      string "`",
      Strings.intercalate (string "") (Lists.map (lambda "q" $
        project TS._TemplateElement TS._TemplateElement_value @@ var "q") (var "quasis")),
      string "`"])

throwStatementToExpr :: TypedTermDefinition (TS.ThrowStatement -> Expr)
throwStatementToExpr = define "throwStatementToExpr" $
  doc "Convert a throw statement to an AST expression" $
  lambda "t" $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "throw",
        expressionToExpr @@ (unwrap TS._ThrowStatement @@ var "t")])

toLineComment :: TypedTermDefinition (String -> String)
toLineComment = define "toLineComment" $
  doc ("Convert a string to a TypeScript line comment. Empty source lines"
    <> " emit `//` (no trailing space).") $
  lambda "s" $ Strings.intercalate (string "\n") $ Lists.map
    (lambda "line" $ Logic.ifElse (Equality.equal (var "line") (string ""))
      (string "//")
      (Strings.cat2 (string "// ") (var "line")))
    (Strings.lines $ var "s")


-- ============================================================================
-- With Comments Variants
-- ============================================================================

toTypeScriptComments :: TypedTermDefinition (String -> [TS.DocumentationTag] -> String)
toTypeScriptComments = define "toTypeScriptComments" $
  doc ("Format a description and tags as a JSDoc comment. Empty doc lines"
    <> " emit ` *` (no trailing space) so blank lines don't carry trailing whitespace.") $
  lambda "desc" $ lambda "tags" $ lets [
    "descLines">: Logic.ifElse (Equality.equal (var "desc") (string ""))
      (list ([] :: [TypedTerm String]))
      (Lists.map (lambda "line" $ Logic.ifElse (Equality.equal (var "line") (string ""))
          (string " *")
          (Strings.cat2 (string " * ") (var "line")))
        (Strings.lines $ var "desc")),
    "tagLines">: Lists.map (documentationTagToLine) (var "tags"),
    "allLines">: Lists.concat $ list [var "descLines", var "tagLines"]] $
    Logic.ifElse (Lists.null $ var "allLines")
      (string "")
      (Strings.intercalate (string "\n") $
        Lists.concat $ list [
          list [string "/**"],
          var "allLines",
          list [string " */"]])

tryStatementToExpr :: TypedTermDefinition (TS.TryStatement -> Expr)
tryStatementToExpr = define "tryStatementToExpr" $
  doc "Convert a try statement to an AST expression" $
  lambda "t" $ lets [
    "block">: project TS._TryStatement TS._TryStatement_block @@ var "t",
    "handler">: project TS._TryStatement TS._TryStatement_handler @@ var "t",
    "finalizer">: project TS._TryStatement TS._TryStatement_finalizer @@ var "t",
    "tryPart">: Serialization.spaceSep @@ list [
      Serialization.cst @@ string "try",
      blockStatementToExpr @@ var "block"],
    "catchPart">: Optionals.cases (var "handler") (list ([] :: [TypedTerm Expr])) (lambda "c" $ list [catchClauseToExpr @@ var "c"]),
    "finallyPart">: Optionals.cases (var "finalizer") (list ([] :: [TypedTerm Expr])) (lambda "f" $ list [Serialization.spaceSep @@ list [
        Serialization.cst @@ string "finally",
        blockStatementToExpr @@ var "f"]])] $
    Serialization.spaceSep @@ (Lists.concat $ list [list [var "tryPart"], var "catchPart", var "finallyPart"])

-- | Render a TS.TypeExpression as a TypeScript type-syntax string. Used by
-- typedPatternToExpr and other contexts that need true TS type syntax
-- (distinct from the JSDoc-style `typeExpressionToString`).
tsTypeExpressionToString :: TypedTermDefinition (TS.TypeExpression -> String)
tsTypeExpressionToString = define "tsTypeExpressionToString" $
  doc "Render a TypeScript type expression as a string in TS syntax" $
  lambda "t" $ cases TS._TypeExpression (var "t") (Just $ string "unknown") [
    -- Render an identifier — but rewrite the synthetic kernel type-variable
    -- names (`T0`, `T1`, …) to `any`. The Hydra type-variable `tN` is
    -- capitalized to `TN` by encodeType, but inline annotations have no
    -- generic-binder syntax to bind them. Substituting at the render layer
    -- (rather than at encodeType) keeps type *definitions* — which DO bind
    -- generics — unaffected.
    TS._TypeExpression_identifier>>: lambda "i" $ lets [
      "raw">: unwrap TS._Identifier @@ var "i"] $
      Logic.ifElse (isKernelTypeVarName @@ var "raw")
        (string "any")
        (var "raw"),
    TS._TypeExpression_any>>: constant $ string "any",
    TS._TypeExpression_void>>: constant $ string "void",
    TS._TypeExpression_never>>: constant $ string "never",
    TS._TypeExpression_array>>: lambda "inner" $
      Strings.cat (list [
        string "ReadonlyArray<",
        tsTypeExpressionToString @@ (unwrap TS._ArrayTypeExpression @@ var "inner"),
        string ">"]),
    TS._TypeExpression_tuple>>: lambda "ts" $
      Strings.cat (list [
        string "readonly [",
        Strings.intercalate (string ", ") (Lists.map (asTerm tsTypeExpressionToString) (var "ts")),
        string "]"]),
    TS._TypeExpression_union>>: lambda "ts" $
      Strings.intercalate (string " | ")
        (Lists.map (asTerm tsTypeExpressionToString) (var "ts")),
    TS._TypeExpression_intersection>>: lambda "ts" $
      Strings.intercalate (string " & ")
        (Lists.map (asTerm tsTypeExpressionToString) (var "ts")),
    TS._TypeExpression_parameterized>>: lambda "p" $
      Strings.cat (list [
        tsTypeExpressionToString @@ (project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_base @@ var "p"),
        string "<",
        Strings.intercalate (string ", ")
          (Lists.map (asTerm tsTypeExpressionToString)
            (project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_arguments @@ var "p")),
        string ">"]),
    TS._TypeExpression_optional>>: lambda "inner" $
      Strings.cat (list [
        tsTypeExpressionToString @@ var "inner",
        string " | undefined"]),
    TS._TypeExpression_readonly>>: lambda "inner" $
      Strings.cat2 (string "readonly ") (tsTypeExpressionToString @@ var "inner"),
    TS._TypeExpression_unknown>>: constant $ string "unknown",
    -- Render a function type as `(...args: any[]) => any`. Why variadic
    -- with `any`?  Hydra encodes function types as curried (`A → B → C`),
    -- but the coder's flat-call ABI emits multi-arg closures (`(a, b) => c`)
    -- everywhere. A signature like `(_a0: any) => any` would reject every
    -- such closure at every callsite (TS2345). Using rest-args sidesteps
    -- arity mismatch without losing the "callable" shape: tsc still types
    -- the callback as a function, while permitting any invocation arity.
    -- Param/return types are erased to `any` because function-type
    -- expressions appearing inline often reference type variables not
    -- bound in the enclosing scope (TS2304 "Cannot find name 'T0'").
    TS._TypeExpression_function>>: constant $
      string "((...args: any[]) => any)"]

typeExpressionToString :: TypedTermDefinition (TS.TypeExpression -> String)
typeExpressionToString = define "typeExpressionToString" $
  doc "Convert a type expression to a string for JSDoc" $
  lambda "typ" $
    cases TS._TypeExpression (var "typ") Nothing [
      TS._TypeExpression_identifier>>: lambda "id" $ unwrap TS._Identifier @@ var "id",
      TS._TypeExpression_any>>: constant $ string "*",
      TS._TypeExpression_void>>: constant $ string "void",
      TS._TypeExpression_never>>: constant $ string "never",
      -- Simplified handling for other cases
      TS._TypeExpression_literal>>: lambda "l" $ string "literal",
      TS._TypeExpression_array>>: lambda "a" $ Strings.cat2 (typeExpressionToString @@ (unwrap TS._ArrayTypeExpression @@ var "a")) (string "[]"),
      -- Render a function type as `(_a0: T0, _a1: T1, ...) => R`. Names
      -- are synthetic (TS requires *some* name in a function-type signature)
      -- — they bind nothing; only the types matter. Previously this arm
      -- emitted the literal token "Function", which made every function-typed
      -- param a reference to the (nonexistent in strict mode) `Function`
      -- type, causing cascading downstream `unknown` errors.
      TS._TypeExpression_function>>: lambda "f" $ lets [
        "params">: project TS._FunctionTypeExpression TS._FunctionTypeExpression_parameters @@ var "f",
        "rt">: project TS._FunctionTypeExpression TS._FunctionTypeExpression_returnType @@ var "f",
        -- Pair each param with its index by a foldl-accumulating index.
        "rendered">:
          Pairs.second $ Lists.foldl
            (lambda "acc" $ lambda "p" $ lets [
              "i">: Pairs.first $ var "acc",
              "soFar">: Pairs.second $ var "acc",
              "this">: Strings.cat $ list [
                string "_a", Literals.showInt32 (var "i"),
                string ": ",
                typeExpressionToString @@ var "p"]] $
              pair
                (Math.add (var "i") (int32 1))
                (Lists.concat2 (var "soFar") (Lists.pure (var "this"))))
            (pair (int32 0) (list ([] :: [TypedTerm String])))
            (var "params")] $
        Strings.cat $ list [
          string "(",
          Strings.intercalate (string ", ") (var "rendered"),
          string ") => ",
          typeExpressionToString @@ var "rt"],
      TS._TypeExpression_object>>: constant $ string "Object",
      TS._TypeExpression_union>>: lambda "u" $ Strings.intercalate (string "|") (Lists.map (typeExpressionToString) (var "u")),
      TS._TypeExpression_parameterized>>: lambda "p" $ lets [
        "base">: project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_base @@ var "p",
        "args">: project TS._ParameterizedTypeExpression TS._ParameterizedTypeExpression_arguments @@ var "p"] $
        Strings.cat $ list [
          typeExpressionToString @@ var "base",
          string "<",
          Strings.intercalate (string ", ") (Lists.map (typeExpressionToString) (var "args")),
          string ">"],
      TS._TypeExpression_optional>>: lambda "o" $ Strings.cat2 (string "?") (typeExpressionToString @@ var "o")]

typedPatternToExpr :: TypedTermDefinition (TS.TypedPattern -> Expr)
typedPatternToExpr = define "typedPatternToExpr" $
  doc "Render `<pattern>: <type>` (TypeScript parameter / variable type annotation)" $
  lambda "tp" $ lets [
    "innerPat">: project TS._TypedPattern TS._TypedPattern_pattern @@ var "tp",
    "typ">: project TS._TypedPattern TS._TypedPattern_type @@ var "tp",
    "innerStr">: patternToString @@ var "innerPat",
    "typeStr">: tsTypeExpressionToString @@ var "typ"] $
    Serialization.cst @@ (Strings.cat (list [var "innerStr", string ": ", var "typeStr"]))

unaryExpressionToExpr :: TypedTermDefinition (TS.UnaryExpression -> Expr)
unaryExpressionToExpr = define "unaryExpressionToExpr" $
  doc "Convert a unary expression to an AST expression" $
  lambda "un" $ lets [
    "op">: project TS._UnaryExpression TS._UnaryExpression_operator @@ var "un",
    "arg">: project TS._UnaryExpression TS._UnaryExpression_argument @@ var "un",
    "prefix">: project TS._UnaryExpression TS._UnaryExpression_prefix @@ var "un",
    "opStr">: unaryOperatorToString @@ var "op",
    "argExpr">: expressionToExpr @@ var "arg"] $
    Logic.ifElse (var "prefix")
      (Serialization.prefix @@ var "opStr" @@ var "argExpr")
      (Serialization.suffix @@ var "opStr" @@ var "argExpr")

unaryOperatorToString :: TypedTermDefinition (TS.UnaryOperator -> String)
unaryOperatorToString = define "unaryOperatorToString" $
  doc "Convert a unary operator to a string" $
  lambda "op" $
    cases TS._UnaryOperator (var "op") Nothing [
      TS._UnaryOperator_negate>>: constant $ string "-",
      TS._UnaryOperator_plus>>: constant $ string "+",
      TS._UnaryOperator_not>>: constant $ string "!",
      TS._UnaryOperator_bitwiseNot>>: constant $ string "~",
      TS._UnaryOperator_typeof>>: constant $ string "typeof ",
      TS._UnaryOperator_void>>: constant $ string "void ",
      TS._UnaryOperator_delete>>: constant $ string "delete ",
      TS._UnaryOperator_increment>>: constant $ string "++",
      TS._UnaryOperator_decrement>>: constant $ string "--"]

variableDeclarationToExpr :: TypedTermDefinition (TS.VariableDeclaration -> Expr)
variableDeclarationToExpr = define "variableDeclarationToExpr" $
  doc "Convert a variable declaration to an AST expression" $
  lambda "decl" $ lets [
    "kind">: project TS._VariableDeclaration TS._VariableDeclaration_kind @@ var "decl",
    "declarations">: project TS._VariableDeclaration TS._VariableDeclaration_declarations @@ var "decl"] $
    Serialization.suffix @@ string ";" @@
      (Serialization.spaceSep @@ list [
        variableKindToExpr @@ var "kind",
        Serialization.commaSep @@ Serialization.inlineStyle @@ (Lists.map (variableDeclaratorToExpr) (var "declarations"))])

variableDeclaratorToExpr :: TypedTermDefinition (TS.VariableDeclarator -> Expr)
variableDeclaratorToExpr = define "variableDeclaratorToExpr" $
  doc "Convert a variable declarator to an AST expression" $
  lambda "decl" $ lets [
    "id">: project TS._VariableDeclarator TS._VariableDeclarator_id @@ var "decl",
    "init">: project TS._VariableDeclarator TS._VariableDeclarator_init @@ var "decl"] $
    Optionals.cases (var "init") (patternToExpr @@ var "id") (lambda "e" $ Serialization.ifx @@ TypeScriptOperators.defineOp @@
        (patternToExpr @@ var "id") @@
        (expressionToExpr @@ var "e"))

variableKindToExpr :: TypedTermDefinition (TS.VariableKind -> Expr)
variableKindToExpr = define "variableKindToExpr" $
  doc "Convert a variable kind to an AST expression" $
  lambda "kind" $
    cases TS._VariableKind (var "kind") Nothing [
      TS._VariableKind_var>>: constant $ Serialization.cst @@ string "var",
      TS._VariableKind_let>>: constant $ Serialization.cst @@ string "let",
      TS._VariableKind_const>>: constant $ Serialization.cst @@ string "const"]

whileStatementToExpr :: TypedTermDefinition (TS.WhileStatement -> Expr)
whileStatementToExpr = define "whileStatementToExpr" $
  doc "Convert a while statement to an AST expression" $
  lambda "w" $ lets [
    "test">: project TS._WhileStatement TS._WhileStatement_test @@ var "w",
    "body">: project TS._WhileStatement TS._WhileStatement_body @@ var "w"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "while",
      Serialization.parens @@ (expressionToExpr @@ var "test"),
      statementToExpr @@ var "body"]
