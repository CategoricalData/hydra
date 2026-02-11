-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting JavaScript AST to abstract expressions

module Hydra.Ext.JavaScript.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Constants as Constants
import qualified Hydra.Ext.JavaScript.Operators as Operators
import qualified Hydra.Ext.JavaScript.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert an identifier to an AST expression
identifierToExpr :: (Syntax.Identifier -> Ast.Expr)
identifierToExpr id = (Serialization.cst (Syntax.unIdentifier id))

-- | Convert a literal to an AST expression
literalToExpr :: (Syntax.Literal -> Ast.Expr)
literalToExpr lit = ((\x -> case x of
  Syntax.LiteralString v1 -> (stringLiteralToExpr v1)
  Syntax.LiteralNumber v1 -> (numericLiteralToExpr v1)
  Syntax.LiteralBoolean v1 -> (Serialization.cst (Logic.ifElse v1 "true" "false"))
  Syntax.LiteralNull -> (Serialization.cst "null")
  Syntax.LiteralUndefined -> (Serialization.cst "undefined")
  Syntax.LiteralBigInt v1 -> (Serialization.cst (Strings.cat2 (Literals.showBigint v1) "n"))
  Syntax.LiteralTemplate v1 -> (templateLiteralToExpr v1)) lit)

-- | Convert a string literal to an AST expression
stringLiteralToExpr :: (Syntax.StringLiteral -> Ast.Expr)
stringLiteralToExpr s =  
  let value = (Syntax.stringLiteralValue s) 
      singleQuote = (Syntax.stringLiteralSingleQuote s)
      quote = (Logic.ifElse singleQuote "'" "\"")
      escaped = (escapeString value singleQuote)
  in (Serialization.cst (Strings.cat [
    quote,
    escaped,
    quote]))

escapeString :: (t0 -> t1 -> t0)
escapeString s singleQuote = s

-- | Convert a template literal to an AST expression
templateLiteralToExpr :: (Syntax.TemplateLiteral -> Ast.Expr)
templateLiteralToExpr t =  
  let quasis = (Syntax.templateLiteralQuasis t) 
      exprs = (Syntax.templateLiteralExpressions t)
  in (Serialization.cst (Strings.cat [
    "`",
    (Strings.intercalate "" (Lists.map (\q -> Syntax.templateElementValue q) quasis)),
    "`"]))

-- | Convert a numeric literal to an AST expression
numericLiteralToExpr :: (Syntax.NumericLiteral -> Ast.Expr)
numericLiteralToExpr n = ((\x -> case x of
  Syntax.NumericLiteralInteger v1 -> (Serialization.cst (Literals.showInt64 v1))
  Syntax.NumericLiteralFloat v1 -> (Serialization.cst (Literals.showFloat64 v1))) n)

-- | Convert a JavaScript expression to an AST expression
expressionToExpr :: (Syntax.Expression -> Ast.Expr)
expressionToExpr expr = ((\x -> case x of
  Syntax.ExpressionIdentifier v1 -> (identifierToExpr v1)
  Syntax.ExpressionLiteral v1 -> (literalToExpr v1)
  Syntax.ExpressionArray v1 -> (arrayExpressionToExpr v1)
  Syntax.ExpressionObject v1 -> (objectExpressionToExpr v1)
  Syntax.ExpressionFunction v1 -> (functionExpressionToExpr v1)
  Syntax.ExpressionArrow v1 -> (arrowFunctionExpressionToExpr v1)
  Syntax.ExpressionCall v1 -> (callExpressionToExpr v1)
  Syntax.ExpressionMember v1 -> (memberExpressionToExpr v1)
  Syntax.ExpressionConditional v1 -> (conditionalExpressionToExpr v1)
  Syntax.ExpressionBinary v1 -> (binaryExpressionToExpr v1)
  Syntax.ExpressionUnary v1 -> (unaryExpressionToExpr v1)
  Syntax.ExpressionAssignment v1 -> (assignmentExpressionToExpr v1)
  Syntax.ExpressionSequence v1 -> (Serialization.parenList False (Lists.map expressionToExpr v1))
  Syntax.ExpressionThis -> (Serialization.cst "this")
  Syntax.ExpressionNew v1 -> (Serialization.spaceSep [
    Serialization.cst "new",
    (callExpressionToExpr v1)])
  Syntax.ExpressionYield v1 -> (Maybes.maybe (Serialization.cst "yield") (\e -> Serialization.spaceSep [
    Serialization.cst "yield",
    (expressionToExpr e)]) v1)
  Syntax.ExpressionAwait v1 -> (Serialization.spaceSep [
    Serialization.cst "await",
    (expressionToExpr v1)])
  Syntax.ExpressionSpread v1 -> (Serialization.prefix "..." (expressionToExpr (Syntax.unSpreadElement v1)))
  Syntax.ExpressionParenthesized v1 -> (Serialization.parenthesize (expressionToExpr v1))) expr)

-- | Convert an array expression to an AST expression
arrayExpressionToExpr :: ([Syntax.ArrayElement] -> Ast.Expr)
arrayExpressionToExpr arr = (Serialization.bracketList Serialization.inlineStyle (Lists.map arrayElementToExpr arr))

-- | Convert an array element to an AST expression
arrayElementToExpr :: (Syntax.ArrayElement -> Ast.Expr)
arrayElementToExpr elem = ((\x -> case x of
  Syntax.ArrayElementExpression v1 -> (expressionToExpr v1)
  Syntax.ArrayElementSpread v1 -> (Serialization.prefix "..." (expressionToExpr (Syntax.unSpreadElement v1)))
  Syntax.ArrayElementHole -> (Serialization.cst "")) elem)

-- | Convert an object expression to an AST expression
objectExpressionToExpr :: ([Syntax.Property] -> Ast.Expr)
objectExpressionToExpr obj = (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map propertyToExpr obj))

-- | Convert an object property to an AST expression
propertyToExpr :: (Syntax.Property -> Ast.Expr)
propertyToExpr prop =  
  let key = (Syntax.propertyKey prop) 
      value = (Syntax.propertyValue prop)
      shorthand = (Syntax.propertyShorthand prop)
      computed = (Syntax.propertyComputed prop)
      keyExpr = (Logic.ifElse computed (Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (expressionToExpr key)) (expressionToExpr key))
  in (Logic.ifElse shorthand keyExpr (Serialization.ifx Operators.colonOp keyExpr (expressionToExpr value)))

-- | Convert a function expression to an AST expression
functionExpressionToExpr :: (Syntax.FunctionExpression -> Ast.Expr)
functionExpressionToExpr fn =  
  let mid = (Syntax.functionExpressionId fn) 
      params = (Syntax.functionExpressionParams fn)
      body = (Syntax.functionExpressionBody fn)
      async = (Syntax.functionExpressionAsync fn)
      generator = (Syntax.functionExpressionGenerator fn)
      asyncKw = (Logic.ifElse async [
              Serialization.cst "async"] [])
      funcKw = (Logic.ifElse generator (Serialization.cst "function*") (Serialization.cst "function"))
      nameExpr = (Maybes.maybe [] (\id -> [
              identifierToExpr id]) mid)
      paramsExpr = (Serialization.parenList False (Lists.map patternToExpr params))
  in (Serialization.spaceSep (Lists.concat [
    asyncKw,
    [
      funcKw],
    nameExpr,
    [
      paramsExpr,
      (blockStatementToExpr body)]]))

-- | Convert an arrow function expression to an AST expression
arrowFunctionExpressionToExpr :: (Syntax.ArrowFunctionExpression -> Ast.Expr)
arrowFunctionExpressionToExpr arrow =  
  let params = (Syntax.arrowFunctionExpressionParams arrow) 
      body = (Syntax.arrowFunctionExpressionBody arrow)
      async = (Syntax.arrowFunctionExpressionAsync arrow)
      asyncKw = (Logic.ifElse async [
              Serialization.cst "async"] [])
      paramsExpr = (Logic.ifElse (Equality.equal (Lists.length params) 1) (patternToExpr (Lists.head params)) (Serialization.parenList False (Lists.map patternToExpr params)))
      bodyExpr = ((\x -> case x of
              Syntax.ArrowFunctionBodyExpression v1 -> (expressionToExpr v1)
              Syntax.ArrowFunctionBodyBlock v1 -> (blockStatementToExpr v1)) body)
  in (Serialization.spaceSep (Lists.concat [
    asyncKw,
    [
      Serialization.ifx Operators.arrowOp paramsExpr bodyExpr]]))

-- | Convert a call expression to an AST expression
callExpressionToExpr :: (Syntax.CallExpression -> Ast.Expr)
callExpressionToExpr call =  
  let callee = (Syntax.callExpressionCallee call) 
      args = (Syntax.callExpressionArguments call)
      optional = (Syntax.callExpressionOptional call)
      calleeExpr = (expressionToExpr callee)
      argsExpr = (Serialization.parenList False (Lists.map expressionToExpr args))
      optionalDot = (Logic.ifElse optional "?." "")
  in (Serialization.spaceSep [
    calleeExpr,
    (Serialization.cst optionalDot),
    argsExpr])

-- | Convert a member expression to an AST expression
memberExpressionToExpr :: (Syntax.MemberExpression -> Ast.Expr)
memberExpressionToExpr mem =  
  let obj = (Syntax.memberExpressionObject mem) 
      prop = (Syntax.memberExpressionProperty mem)
      computed = (Syntax.memberExpressionComputed mem)
      optional = (Syntax.memberExpressionOptional mem)
      objExpr = (expressionToExpr obj)
      propExpr = (expressionToExpr prop)
  in (Logic.ifElse computed (Serialization.spaceSep [
    objExpr,
    (Logic.ifElse optional (Serialization.cst "?.") (Serialization.cst "")),
    (Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle propExpr)]) (Serialization.ifx (Logic.ifElse optional Operators.optionalChainOp Operators.memberOp) objExpr propExpr))

-- | Convert a conditional expression to an AST expression
conditionalExpressionToExpr :: (Syntax.ConditionalExpression -> Ast.Expr)
conditionalExpressionToExpr cond =  
  let test = (Syntax.conditionalExpressionTest cond) 
      consequent = (Syntax.conditionalExpressionConsequent cond)
      alternate = (Syntax.conditionalExpressionAlternate cond)
  in (Serialization.spaceSep [
    expressionToExpr test,
    (Serialization.cst "?"),
    (expressionToExpr consequent),
    (Serialization.cst ":"),
    (expressionToExpr alternate)])

-- | Convert a binary expression to an AST expression
binaryExpressionToExpr :: (Syntax.BinaryExpression -> Ast.Expr)
binaryExpressionToExpr bin =  
  let op = (Syntax.binaryExpressionOperator bin) 
      left = (Syntax.binaryExpressionLeft bin)
      right = (Syntax.binaryExpressionRight bin)
  in (Serialization.ifx (binaryOperatorToExpr op) (expressionToExpr left) (expressionToExpr right))

-- | Convert a unary expression to an AST expression
unaryExpressionToExpr :: (Syntax.UnaryExpression -> Ast.Expr)
unaryExpressionToExpr un =  
  let op = (Syntax.unaryExpressionOperator un) 
      arg = (Syntax.unaryExpressionArgument un)
      prefix = (Syntax.unaryExpressionPrefix un)
      opStr = (unaryOperatorToString op)
      argExpr = (expressionToExpr arg)
  in (Logic.ifElse prefix (Serialization.prefix opStr argExpr) (Serialization.suffix opStr argExpr))

-- | Convert an assignment expression to an AST expression
assignmentExpressionToExpr :: (Syntax.AssignmentExpression -> Ast.Expr)
assignmentExpressionToExpr assign =  
  let op = (Syntax.assignmentExpressionOperator assign) 
      left = (Syntax.assignmentExpressionLeft assign)
      right = (Syntax.assignmentExpressionRight assign)
      opStr = (assignmentOperatorToString op)
  in (Serialization.spaceSep [
    patternToExpr left,
    (Serialization.cst opStr),
    (expressionToExpr right)])

-- | Convert a pattern to an AST expression
patternToExpr :: (Syntax.Pattern -> Ast.Expr)
patternToExpr pat = ((\x -> case x of
  Syntax.PatternIdentifier v1 -> (identifierToExpr v1)
  Syntax.PatternObject v1 -> (objectPatternToExpr v1)
  Syntax.PatternArray v1 -> (arrayPatternToExpr v1)
  Syntax.PatternAssignment v1 -> (assignmentPatternToExpr v1)
  Syntax.PatternRest v1 -> (Serialization.prefix "..." (patternToExpr (Syntax.unRestElement v1)))) pat)

-- | Convert an object pattern to an AST expression
objectPatternToExpr :: (Syntax.ObjectPattern -> Ast.Expr)
objectPatternToExpr obj =  
  let props = (Syntax.objectPatternProperties obj)
  in (Serialization.curlyBracesList Nothing Serialization.inlineStyle (Lists.map objectPatternPropertyToExpr props))

-- | Convert an object pattern property to an AST expression
objectPatternPropertyToExpr :: (Syntax.ObjectPatternProperty -> Ast.Expr)
objectPatternPropertyToExpr prop = ((\x -> case x of
  Syntax.ObjectPatternPropertyProperty v1 -> (propertyToExpr v1)
  Syntax.ObjectPatternPropertyRest v1 -> (Serialization.prefix "..." (patternToExpr (Syntax.unRestElement v1)))) prop)

-- | Convert an array pattern to an AST expression
arrayPatternToExpr :: ([Maybe Syntax.Pattern] -> Ast.Expr)
arrayPatternToExpr arr = (Serialization.bracketList Serialization.inlineStyle (Lists.map (\maybeP -> Maybes.maybe (Serialization.cst "") patternToExpr maybeP) arr))

-- | Convert an assignment pattern to an AST expression
assignmentPatternToExpr :: (Syntax.AssignmentPattern -> Ast.Expr)
assignmentPatternToExpr assign =  
  let left = (Syntax.assignmentPatternLeft assign) 
      right = (Syntax.assignmentPatternRight assign)
  in (Serialization.ifx Operators.defineOp (patternToExpr left) (expressionToExpr right))

-- | Convert a statement to an AST expression
statementToExpr :: (Syntax.Statement -> Ast.Expr)
statementToExpr stmt = ((\x -> case x of
  Syntax.StatementExpression v1 -> (Serialization.suffix ";" (expressionToExpr v1))
  Syntax.StatementBlock v1 -> (blockStatementToExpr v1)
  Syntax.StatementEmpty -> (Serialization.cst ";")
  Syntax.StatementDebugger -> (Serialization.cst "debugger;")
  Syntax.StatementReturn v1 -> (returnStatementToExpr v1)
  Syntax.StatementBreak v1 -> (breakStatementToExpr v1)
  Syntax.StatementContinue v1 -> (continueStatementToExpr v1)
  Syntax.StatementIf v1 -> (ifStatementToExpr v1)
  Syntax.StatementSwitch v1 -> (switchStatementToExpr v1)
  Syntax.StatementThrow v1 -> (throwStatementToExpr v1)
  Syntax.StatementTry v1 -> (tryStatementToExpr v1)
  Syntax.StatementWhile v1 -> (whileStatementToExpr v1)
  Syntax.StatementDoWhile v1 -> (doWhileStatementToExpr v1)
  Syntax.StatementFor v1 -> (forStatementToExpr v1)
  Syntax.StatementForIn v1 -> (forInStatementToExpr v1)
  Syntax.StatementForOf v1 -> (forOfStatementToExpr v1)
  Syntax.StatementVariableDeclaration v1 -> (variableDeclarationToExpr v1)
  Syntax.StatementFunctionDeclaration v1 -> (functionDeclarationToExpr v1)
  Syntax.StatementClassDeclaration v1 -> (classDeclarationToExpr v1)
  Syntax.StatementLabeled v1 -> (labeledStatementToExpr v1)) stmt)

-- | Convert a block statement to an AST expression
blockStatementToExpr :: ([Syntax.Statement] -> Ast.Expr)
blockStatementToExpr block = (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map statementToExpr block))

-- | Convert a variable declaration to an AST expression
variableDeclarationToExpr :: (Syntax.VariableDeclaration -> Ast.Expr)
variableDeclarationToExpr decl =  
  let kind = (Syntax.variableDeclarationKind decl) 
      declarations = (Syntax.variableDeclarationDeclarations decl)
  in (Serialization.suffix ";" (Serialization.spaceSep [
    variableKindToExpr kind,
    (Serialization.commaSep Serialization.inlineStyle (Lists.map variableDeclaratorToExpr declarations))]))

-- | Convert a variable declarator to an AST expression
variableDeclaratorToExpr :: (Syntax.VariableDeclarator -> Ast.Expr)
variableDeclaratorToExpr decl =  
  let id = (Syntax.variableDeclaratorId decl) 
      init = (Syntax.variableDeclaratorInit decl)
  in (Maybes.maybe (patternToExpr id) (\e -> Serialization.ifx Operators.defineOp (patternToExpr id) (expressionToExpr e)) init)

-- | Convert a variable kind to an AST expression
variableKindToExpr :: (Syntax.VariableKind -> Ast.Expr)
variableKindToExpr kind = ((\x -> case x of
  Syntax.VariableKindVar -> (Serialization.cst "var")
  Syntax.VariableKindLet -> (Serialization.cst "let")
  Syntax.VariableKindConst -> (Serialization.cst "const")) kind)

-- | Convert an if statement to an AST expression
ifStatementToExpr :: (Syntax.IfStatement -> Ast.Expr)
ifStatementToExpr ifStmt =  
  let test = (Syntax.ifStatementTest ifStmt) 
      consequent = (Syntax.ifStatementConsequent ifStmt)
      alternate = (Syntax.ifStatementAlternate ifStmt)
      ifPart = (Serialization.spaceSep [
              Serialization.cst "if",
              (Serialization.parenthesize (expressionToExpr test)),
              (statementToExpr consequent)])
  in (Maybes.maybe ifPart (\alt -> Serialization.spaceSep [
    ifPart,
    (Serialization.cst "else"),
    (statementToExpr alt)]) alternate)

-- | Convert a switch statement to an AST expression
switchStatementToExpr :: (Syntax.SwitchStatement -> Ast.Expr)
switchStatementToExpr switchStmt =  
  let discriminant = (Syntax.switchStatementDiscriminant switchStmt) 
      cases = (Syntax.switchStatementCases switchStmt)
  in (Serialization.spaceSep [
    Serialization.cst "switch",
    (Serialization.parenthesize (expressionToExpr discriminant)),
    (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map switchCaseToExpr cases))])

-- | Convert a switch case to an AST expression
switchCaseToExpr :: (Syntax.SwitchCase -> Ast.Expr)
switchCaseToExpr c =  
  let test = (Syntax.switchCaseTest c) 
      consequent = (Syntax.switchCaseConsequent c)
      caseLabel = (Maybes.maybe (Serialization.cst "default:") (\t -> Serialization.spaceSep [
              Serialization.cst "case",
              (expressionToExpr t),
              (Serialization.cst ":")]) test)
  in (Serialization.newlineSep (Lists.cons caseLabel (Lists.map statementToExpr consequent)))

-- | Convert a return statement to an AST expression
returnStatementToExpr :: (Maybe Syntax.Expression -> Ast.Expr)
returnStatementToExpr r = (Maybes.maybe (Serialization.cst "return;") (\e -> Serialization.suffix ";" (Serialization.spaceSep [
  Serialization.cst "return",
  (expressionToExpr e)])) r)

-- | Convert a throw statement to an AST expression
throwStatementToExpr :: (Syntax.ThrowStatement -> Ast.Expr)
throwStatementToExpr t = (Serialization.suffix ";" (Serialization.spaceSep [
  Serialization.cst "throw",
  (expressionToExpr (Syntax.unThrowStatement t))]))

-- | Convert a try statement to an AST expression
tryStatementToExpr :: (Syntax.TryStatement -> Ast.Expr)
tryStatementToExpr t =  
  let block = (Syntax.tryStatementBlock t) 
      handler = (Syntax.tryStatementHandler t)
      finalizer = (Syntax.tryStatementFinalizer t)
      tryPart = (Serialization.spaceSep [
              Serialization.cst "try",
              (blockStatementToExpr block)])
      catchPart = (Maybes.maybe [] (\c -> [
              catchClauseToExpr c]) handler)
      finallyPart = (Maybes.maybe [] (\f -> [
              Serialization.spaceSep [
                Serialization.cst "finally",
                (blockStatementToExpr f)]]) finalizer)
  in (Serialization.spaceSep (Lists.concat [
    [
      tryPart],
    catchPart,
    finallyPart]))

-- | Convert a catch clause to an AST expression
catchClauseToExpr :: (Syntax.CatchClause -> Ast.Expr)
catchClauseToExpr c =  
  let param = (Syntax.catchClauseParam c) 
      body = (Syntax.catchClauseBody c)
      catchKw = (Maybes.maybe (Serialization.cst "catch") (\p -> Serialization.spaceSep [
              Serialization.cst "catch",
              (Serialization.parenthesize (patternToExpr p))]) param)
  in (Serialization.spaceSep [
    catchKw,
    (blockStatementToExpr body)])

-- | Convert a break statement to an AST expression
breakStatementToExpr :: (Maybe Syntax.Identifier -> Ast.Expr)
breakStatementToExpr b = (Maybes.maybe (Serialization.cst "break;") (\label -> Serialization.suffix ";" (Serialization.spaceSep [
  Serialization.cst "break",
  (identifierToExpr label)])) b)

-- | Convert a continue statement to an AST expression
continueStatementToExpr :: (Maybe Syntax.Identifier -> Ast.Expr)
continueStatementToExpr c = (Maybes.maybe (Serialization.cst "continue;") (\label -> Serialization.suffix ";" (Serialization.spaceSep [
  Serialization.cst "continue",
  (identifierToExpr label)])) c)

-- | Convert a while statement to an AST expression
whileStatementToExpr :: (Syntax.WhileStatement -> Ast.Expr)
whileStatementToExpr w =  
  let test = (Syntax.whileStatementTest w) 
      body = (Syntax.whileStatementBody w)
  in (Serialization.spaceSep [
    Serialization.cst "while",
    (Serialization.parenthesize (expressionToExpr test)),
    (statementToExpr body)])

-- | Convert a do-while statement to an AST expression
doWhileStatementToExpr :: (Syntax.DoWhileStatement -> Ast.Expr)
doWhileStatementToExpr d =  
  let body = (Syntax.doWhileStatementBody d) 
      test = (Syntax.doWhileStatementTest d)
  in (Serialization.suffix ";" (Serialization.spaceSep [
    Serialization.cst "do",
    (statementToExpr body),
    (Serialization.cst "while"),
    (Serialization.parenthesize (expressionToExpr test))]))

-- | Convert a for statement to an AST expression
forStatementToExpr :: (Syntax.ForStatement -> Ast.Expr)
forStatementToExpr f =  
  let init = (Syntax.forStatementInit f) 
      test = (Syntax.forStatementTest f)
      update = (Syntax.forStatementUpdate f)
      body = (Syntax.forStatementBody f)
      initExpr = (Maybes.maybe (Serialization.cst "") (\i -> (\x -> case x of
              Syntax.ForInitVariable v1 -> (variableDeclarationToExpr v1)
              Syntax.ForInitExpression v1 -> (expressionToExpr v1)) i) init)
      testExpr = (Maybes.maybe (Serialization.cst "") expressionToExpr test)
      updateExpr = (Maybes.maybe (Serialization.cst "") expressionToExpr update)
  in (Serialization.spaceSep [
    Serialization.cst "for",
    (Serialization.parenList False [
      initExpr,
      testExpr,
      updateExpr]),
    (statementToExpr body)])

-- | Convert a for-in statement to an AST expression
forInStatementToExpr :: (Syntax.ForInStatement -> Ast.Expr)
forInStatementToExpr f =  
  let left = (Syntax.forInStatementLeft f) 
      right = (Syntax.forInStatementRight f)
      body = (Syntax.forInStatementBody f)
      leftExpr = ((\x -> case x of
              Syntax.ForInLeftVariable v1 -> (variableDeclarationToExpr v1)
              Syntax.ForInLeftPattern v1 -> (patternToExpr v1)) left)
  in (Serialization.spaceSep [
    Serialization.cst "for",
    (Serialization.parenthesize (Serialization.spaceSep [
      leftExpr,
      (Serialization.cst "in"),
      (expressionToExpr right)])),
    (statementToExpr body)])

-- | Convert a for-of statement to an AST expression
forOfStatementToExpr :: (Syntax.ForOfStatement -> Ast.Expr)
forOfStatementToExpr f =  
  let await = (Syntax.forOfStatementAwait f) 
      left = (Syntax.forOfStatementLeft f)
      right = (Syntax.forOfStatementRight f)
      body = (Syntax.forOfStatementBody f)
      forKw = (Logic.ifElse await (Serialization.cst "for await") (Serialization.cst "for"))
      leftExpr = ((\x -> case x of
              Syntax.ForInLeftVariable v1 -> (variableDeclarationToExpr v1)
              Syntax.ForInLeftPattern v1 -> (patternToExpr v1)) left)
  in (Serialization.spaceSep [
    forKw,
    (Serialization.parenthesize (Serialization.spaceSep [
      leftExpr,
      (Serialization.cst "of"),
      (expressionToExpr right)])),
    (statementToExpr body)])

-- | Convert a labeled statement to an AST expression
labeledStatementToExpr :: (Syntax.LabeledStatement -> Ast.Expr)
labeledStatementToExpr l =  
  let label = (Syntax.labeledStatementLabel l) 
      body = (Syntax.labeledStatementBody l)
  in (Serialization.spaceSep [
    Serialization.suffix ":" (identifierToExpr label),
    (statementToExpr body)])

-- | Convert a function declaration to an AST expression
functionDeclarationToExpr :: (Syntax.FunctionDeclaration -> Ast.Expr)
functionDeclarationToExpr fn =  
  let id = (Syntax.functionDeclarationId fn) 
      params = (Syntax.functionDeclarationParams fn)
      body = (Syntax.functionDeclarationBody fn)
      async = (Syntax.functionDeclarationAsync fn)
      generator = (Syntax.functionDeclarationGenerator fn)
      asyncKw = (Logic.ifElse async [
              Serialization.cst "async"] [])
      funcKw = (Logic.ifElse generator (Serialization.cst "function*") (Serialization.cst "function"))
      paramsExpr = (Serialization.parenList False (Lists.map patternToExpr params))
  in (Serialization.spaceSep (Lists.concat [
    asyncKw,
    [
      funcKw,
      (identifierToExpr id),
      paramsExpr,
      (blockStatementToExpr body)]]))

-- | Convert a class declaration to an AST expression
classDeclarationToExpr :: (Syntax.ClassDeclaration -> Ast.Expr)
classDeclarationToExpr cls =  
  let id = (Syntax.classDeclarationId cls) 
      superClass = (Syntax.classDeclarationSuperClass cls)
      body = (Syntax.classDeclarationBody cls)
      extendsClause = (Maybes.maybe [] (\s -> [
              Serialization.cst "extends",
              (expressionToExpr s)]) superClass)
      bodyExpr = (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle (Lists.map methodDefinitionToExpr body))
  in (Serialization.spaceSep (Lists.concat [
    [
      Serialization.cst "class",
      (identifierToExpr id)],
    extendsClause,
    [
      bodyExpr]]))

-- | Convert a method definition to an AST expression
methodDefinitionToExpr :: (Syntax.MethodDefinition -> Ast.Expr)
methodDefinitionToExpr method =  
  let key = (Syntax.methodDefinitionKey method) 
      value = (Syntax.methodDefinitionValue method)
      kind = (Syntax.methodDefinitionKind method)
      computed = (Syntax.methodDefinitionComputed method)
      static = (Syntax.methodDefinitionStatic method)
      staticKw = (Logic.ifElse static [
              Serialization.cst "static"] [])
      kindKw = ((\x -> case x of
              Syntax.MethodKindConstructor -> []
              Syntax.MethodKindMethod -> []
              Syntax.MethodKindGet -> [
                Serialization.cst "get"]
              Syntax.MethodKindSet -> [
                Serialization.cst "set"]) kind)
      keyExpr = (Logic.ifElse computed (Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (expressionToExpr key)) (expressionToExpr key))
      params = (Syntax.functionExpressionParams value)
      body = (Syntax.functionExpressionBody value)
      paramsExpr = (Serialization.parenList False (Lists.map patternToExpr params))
  in (Serialization.spaceSep (Lists.concat [
    staticKw,
    kindKw,
    [
      keyExpr,
      paramsExpr,
      (blockStatementToExpr body)]]))

-- | Convert a JavaScript program to an AST expression
programToExpr :: (Syntax.Program -> Ast.Expr)
programToExpr prog =  
  let body = (Syntax.programBody prog) 
      warning = [
              Serialization.cst (toLineComment Constants.warningAutoGeneratedFile)]
      items = (Lists.map moduleItemToExpr body)
  in (Serialization.doubleNewlineSep (Lists.concat [
    warning,
    items]))

-- | Convert a module item to an AST expression
moduleItemToExpr :: (Syntax.ModuleItem -> Ast.Expr)
moduleItemToExpr item = ((\x -> case x of
  Syntax.ModuleItemStatement v1 -> (statementToExpr v1)
  Syntax.ModuleItemImport v1 -> (importDeclarationToExpr v1)
  Syntax.ModuleItemExport v1 -> (exportDeclarationToExpr v1)) item)

-- | Convert an import declaration to an AST expression
importDeclarationToExpr :: (Syntax.ImportDeclaration -> Ast.Expr)
importDeclarationToExpr imp =  
  let specifiers = (Syntax.importDeclarationSpecifiers imp) 
      source = (Syntax.importDeclarationSource imp)
      sourceExpr = (stringLiteralToExpr source)
      specExprs = (Lists.map importSpecifierToExpr specifiers)
  in (Logic.ifElse (Lists.null specifiers) (Serialization.suffix ";" (Serialization.spaceSep [
    Serialization.cst "import",
    sourceExpr])) (Serialization.suffix ";" (Serialization.spaceSep [
    Serialization.cst "import",
    (formatImportSpecifiers specExprs),
    (Serialization.cst "from"),
    sourceExpr])))

-- | Convert an import specifier to an AST expression
importSpecifierToExpr :: (Syntax.ImportClause -> Ast.Expr)
importSpecifierToExpr spec = ((\x -> case x of
  Syntax.ImportClauseNamed v1 ->  
    let imported = (Syntax.importSpecifierImported v1) 
        local = (Syntax.importSpecifierLocal v1)
    in (Logic.ifElse (Equality.equal (Syntax.unIdentifier imported) (Syntax.unIdentifier local)) (identifierToExpr local) (Serialization.spaceSep [
      identifierToExpr imported,
      (Serialization.cst "as"),
      (identifierToExpr local)]))
  Syntax.ImportClauseDefault v1 -> (identifierToExpr (Syntax.unImportDefaultSpecifier v1))
  Syntax.ImportClauseNamespace v1 -> (Serialization.spaceSep [
    Serialization.cst "*",
    (Serialization.cst "as"),
    (identifierToExpr (Syntax.unImportNamespaceSpecifier v1))])) spec)

-- | Format import specifiers, handling default vs named imports
formatImportSpecifiers :: ([Ast.Expr] -> Ast.Expr)
formatImportSpecifiers specs = (Serialization.curlyBracesList Nothing Serialization.inlineStyle specs)

-- | Convert an export declaration to an AST expression
exportDeclarationToExpr :: (Syntax.ExportDeclaration -> Ast.Expr)
exportDeclarationToExpr exp = ((\x -> case x of
  Syntax.ExportDeclarationNamed v1 -> (namedExportToExpr v1)
  Syntax.ExportDeclarationDefault v1 -> (Serialization.suffix ";" (Serialization.spaceSep [
    Serialization.cst "export",
    (Serialization.cst "default"),
    (expressionToExpr v1)]))
  Syntax.ExportDeclarationDeclaration v1 -> (Serialization.spaceSep [
    Serialization.cst "export",
    (statementToExpr v1)])
  Syntax.ExportDeclarationAll v1 -> (exportAllToExpr v1)) exp)

-- | Convert a named export to an AST expression
namedExportToExpr :: (Syntax.NamedExport -> Ast.Expr)
namedExportToExpr n =  
  let specifiers = (Syntax.namedExportSpecifiers n) 
      source = (Syntax.namedExportSource n)
      specExprs = (Lists.map exportSpecifierToExpr specifiers)
      fromClause = (Maybes.maybe [] (\s -> [
              Serialization.cst "from",
              (stringLiteralToExpr s)]) source)
  in (Serialization.suffix ";" (Serialization.spaceSep (Lists.concat [
    [
      Serialization.cst "export"],
    [
      Serialization.curlyBracesList Nothing Serialization.inlineStyle specExprs],
    fromClause])))

-- | Convert an export specifier to an AST expression
exportSpecifierToExpr :: (Syntax.ExportSpecifier -> Ast.Expr)
exportSpecifierToExpr spec =  
  let local = (Syntax.exportSpecifierLocal spec) 
      exported = (Syntax.exportSpecifierExported spec)
  in (Logic.ifElse (Equality.equal (Syntax.unIdentifier local) (Syntax.unIdentifier exported)) (identifierToExpr local) (Serialization.spaceSep [
    identifierToExpr local,
    (Serialization.cst "as"),
    (identifierToExpr exported)]))

-- | Convert an export all declaration to an AST expression
exportAllToExpr :: (Syntax.ExportAllDeclaration -> Ast.Expr)
exportAllToExpr a =  
  let exported = (Syntax.exportAllDeclarationExported a) 
      source = (Syntax.exportAllDeclarationSource a)
      exportedClause = (Maybes.maybe (Serialization.cst "*") (\e -> Serialization.spaceSep [
              Serialization.cst "*",
              (Serialization.cst "as"),
              (identifierToExpr e)]) exported)
  in (Serialization.suffix ";" (Serialization.spaceSep [
    Serialization.cst "export",
    exportedClause,
    (Serialization.cst "from"),
    (stringLiteralToExpr source)]))

-- | Convert a binary operator to an Op
binaryOperatorToExpr :: (Syntax.BinaryOperator -> Ast.Op)
binaryOperatorToExpr op = ((\x -> case x of
  Syntax.BinaryOperatorAdd -> Operators.addOp
  Syntax.BinaryOperatorSubtract -> Operators.subtractOp
  Syntax.BinaryOperatorMultiply -> Operators.multiplyOp
  Syntax.BinaryOperatorDivide -> Operators.divideOp
  Syntax.BinaryOperatorModulo -> Operators.moduloOp
  Syntax.BinaryOperatorExponentiate -> Operators.exponentiateOp
  Syntax.BinaryOperatorEqual -> Operators.equalOp
  Syntax.BinaryOperatorNotEqual -> Operators.notEqualOp
  Syntax.BinaryOperatorStrictEqual -> Operators.strictEqualOp
  Syntax.BinaryOperatorStrictNotEqual -> Operators.strictNotEqualOp
  Syntax.BinaryOperatorLessThan -> Operators.lessThanOp
  Syntax.BinaryOperatorLessThanOrEqual -> Operators.lessThanOrEqualOp
  Syntax.BinaryOperatorGreaterThan -> Operators.greaterThanOp
  Syntax.BinaryOperatorGreaterThanOrEqual -> Operators.greaterThanOrEqualOp
  Syntax.BinaryOperatorAnd -> Operators.logicalAndOp
  Syntax.BinaryOperatorOr -> Operators.logicalOrOp
  Syntax.BinaryOperatorNullishCoalescing -> Operators.nullishCoalescingOp
  Syntax.BinaryOperatorBitwiseAnd -> Operators.bitwiseAndOp
  Syntax.BinaryOperatorBitwiseOr -> Operators.bitwiseOrOp
  Syntax.BinaryOperatorBitwiseXor -> Operators.bitwiseXorOp
  Syntax.BinaryOperatorLeftShift -> Operators.leftShiftOp
  Syntax.BinaryOperatorRightShift -> Operators.rightShiftOp
  Syntax.BinaryOperatorUnsignedRightShift -> Operators.unsignedRightShiftOp
  Syntax.BinaryOperatorIn -> Operators.inOp
  Syntax.BinaryOperatorInstanceof -> Operators.instanceOfOp) op)

-- | Convert a unary operator to a string
unaryOperatorToString :: (Syntax.UnaryOperator -> String)
unaryOperatorToString op = ((\x -> case x of
  Syntax.UnaryOperatorNegate -> "-"
  Syntax.UnaryOperatorPlus -> "+"
  Syntax.UnaryOperatorNot -> "!"
  Syntax.UnaryOperatorBitwiseNot -> "~"
  Syntax.UnaryOperatorTypeof -> "typeof "
  Syntax.UnaryOperatorVoid -> "void "
  Syntax.UnaryOperatorDelete -> "delete "
  Syntax.UnaryOperatorIncrement -> "++"
  Syntax.UnaryOperatorDecrement -> "--") op)

-- | Convert an assignment operator to a string
assignmentOperatorToString :: (Syntax.AssignmentOperator -> String)
assignmentOperatorToString op = ((\x -> case x of
  Syntax.AssignmentOperatorAssign -> "="
  Syntax.AssignmentOperatorAddAssign -> "+="
  Syntax.AssignmentOperatorSubtractAssign -> "-="
  Syntax.AssignmentOperatorMultiplyAssign -> "*="
  Syntax.AssignmentOperatorDivideAssign -> "/="
  Syntax.AssignmentOperatorModuloAssign -> "%="
  Syntax.AssignmentOperatorExponentiateAssign -> "**="
  Syntax.AssignmentOperatorLeftShiftAssign -> "<<="
  Syntax.AssignmentOperatorRightShiftAssign -> ">>="
  Syntax.AssignmentOperatorUnsignedRightShiftAssign -> ">>>="
  Syntax.AssignmentOperatorBitwiseAndAssign -> "&="
  Syntax.AssignmentOperatorBitwiseOrAssign -> "|="
  Syntax.AssignmentOperatorBitwiseXorAssign -> "^="
  Syntax.AssignmentOperatorAndAssign -> "&&="
  Syntax.AssignmentOperatorOrAssign -> "||="
  Syntax.AssignmentOperatorNullishAssign -> "??=") op)

-- | Convert a documentation comment to an AST expression
documentationCommentToExpr :: (Syntax.DocumentationComment -> Ast.Expr)
documentationCommentToExpr doc =  
  let description = (Syntax.documentationCommentDescription doc) 
      tags = (Syntax.documentationCommentTags doc)
  in (Serialization.cst (toJavaScriptComments description tags))

-- | Format a description and tags as a JSDoc comment
toJavaScriptComments :: (String -> [Syntax.DocumentationTag] -> String)
toJavaScriptComments desc tags =  
  let descLines = (Logic.ifElse (Equality.equal desc "") [] (Lists.map (\line -> Strings.cat2 " * " line) (Strings.lines desc))) 
      tagLines = (Lists.map documentationTagToLine tags)
      allLines = (Lists.concat [
              descLines,
              tagLines])
  in (Logic.ifElse (Lists.null allLines) "" (Strings.intercalate "\n" (Lists.concat [
    [
      "/**"],
    allLines,
    [
      " */"]])))

-- | Convert a documentation tag to a JSDoc line
documentationTagToLine :: (Syntax.DocumentationTag -> String)
documentationTagToLine tag =  
  let name = (Syntax.documentationTagName tag) 
      mtype = (Syntax.documentationTagType tag)
      mparamName = (Syntax.documentationTagParamName tag)
      description = (Syntax.documentationTagDescription tag)
      typePart = (Maybes.maybe "" (\t -> Strings.cat [
              "{",
              (typeExpressionToString t),
              "} "]) mtype)
      paramPart = (Maybes.maybe "" (\p -> Strings.cat2 (Syntax.unIdentifier p) " ") mparamName)
  in (Strings.cat [
    " * @",
    name,
    " ",
    typePart,
    paramPart,
    description])

-- | Convert a type expression to a string for JSDoc
typeExpressionToString :: (Syntax.TypeExpression -> String)
typeExpressionToString typ = ((\x -> case x of
  Syntax.TypeExpressionIdentifier v1 -> (Syntax.unIdentifier v1)
  Syntax.TypeExpressionAny -> "*"
  Syntax.TypeExpressionVoid -> "void"
  Syntax.TypeExpressionNever -> "never"
  Syntax.TypeExpressionLiteral _ -> "literal"
  Syntax.TypeExpressionArray v1 -> (Strings.cat2 (typeExpressionToString (Syntax.unArrayTypeExpression v1)) "[]")
  Syntax.TypeExpressionFunction _ -> "Function"
  Syntax.TypeExpressionObject _ -> "Object"
  Syntax.TypeExpressionUnion v1 -> (Strings.intercalate "|" (Lists.map typeExpressionToString v1))
  Syntax.TypeExpressionParameterized v1 ->  
    let base = (Syntax.parameterizedTypeExpressionBase v1) 
        args = (Syntax.parameterizedTypeExpressionArguments v1)
    in (Strings.cat [
      typeExpressionToString base,
      "<",
      (Strings.intercalate ", " (Lists.map typeExpressionToString args)),
      ">"])
  Syntax.TypeExpressionOptional v1 -> (Strings.cat2 "?" (typeExpressionToString v1))) typ)

-- | Convert a string to a JavaScript line comment
toLineComment :: (String -> String)
toLineComment s = (Strings.intercalate "\n" (Lists.map (\line -> Strings.cat2 "// " line) (Strings.lines s)))

-- | Convert a module item with comments to an AST expression
moduleItemWithCommentsToExpr :: (Syntax.ModuleItemWithComments -> Ast.Expr)
moduleItemWithCommentsToExpr miwc =  
  let body = (Syntax.moduleItemWithCommentsBody miwc) 
      mc = (Syntax.moduleItemWithCommentsComments miwc)
  in (Maybes.maybe (moduleItemToExpr body) (\c -> Serialization.newlineSep [
    documentationCommentToExpr c,
    (moduleItemToExpr body)]) mc)

-- | Convert a function declaration with comments to an AST expression
functionDeclarationWithCommentsToExpr :: (Syntax.FunctionDeclarationWithComments -> Ast.Expr)
functionDeclarationWithCommentsToExpr fdwc =  
  let body = (Syntax.functionDeclarationWithCommentsBody fdwc) 
      mc = (Syntax.functionDeclarationWithCommentsComments fdwc)
  in (Maybes.maybe (functionDeclarationToExpr body) (\c -> Serialization.newlineSep [
    documentationCommentToExpr c,
    (functionDeclarationToExpr body)]) mc)

-- | Convert a class declaration with comments to an AST expression
classDeclarationWithCommentsToExpr :: (Syntax.ClassDeclarationWithComments -> Ast.Expr)
classDeclarationWithCommentsToExpr cdwc =  
  let body = (Syntax.classDeclarationWithCommentsBody cdwc) 
      mc = (Syntax.classDeclarationWithCommentsComments cdwc)
  in (Maybes.maybe (classDeclarationToExpr body) (\c -> Serialization.newlineSep [
    documentationCommentToExpr c,
    (classDeclarationToExpr body)]) mc)
