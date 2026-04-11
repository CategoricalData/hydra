-- | A DSL for constructing JavaScript syntax trees in Haskell.
module Hydra.Ext.Dsl.JavaScript.Syntax where

import Hydra.Ext.JavaScript.Syntax
import qualified Data.Int as I


-- * Identifiers

-- | Create an identifier
-- Example: ident "foo"
ident :: String -> Identifier
ident = Identifier

-- | Create a qualified name from a list of strings
-- Example: qualifiedName ["module", "submodule", "name"]
qualifiedName :: [String] -> QualifiedName
qualifiedName = fmap Identifier


-- * Literals

-- | Create a string literal with double quotes
-- Example: stringLit "hello"
stringLit :: String -> Literal
stringLit s = LiteralString $ StringLiteral s False

-- | Create a string literal with single quotes
-- Example: stringLitSingle "hello"
stringLitSingle :: String -> Literal
stringLitSingle s = LiteralString $ StringLiteral s True

-- | Create an integer literal
-- Example: intLit 42
intLit :: I.Int64 -> Literal
intLit = LiteralNumber . NumericLiteralInteger

-- | Create a floating-point literal
-- Example: floatLit 3.14
floatLit :: Double -> Literal
floatLit = LiteralNumber . NumericLiteralFloat

-- | Create a boolean literal
-- Example: boolLit True
boolLit :: Bool -> Literal
boolLit = LiteralBoolean

-- | The null literal
nullLit :: Literal
nullLit = LiteralNull

-- | The undefined literal
undefinedLit :: Literal
undefinedLit = LiteralUndefined

-- | Create a BigInt literal
-- Example: bigIntLit 123
bigIntLit :: Integer -> Literal
bigIntLit = LiteralBigInt


-- * Expressions

-- | Create an identifier expression
-- Example: identExpr "foo"
identExpr :: String -> Expression
identExpr = ExpressionIdentifier . ident

-- | Create a literal expression
-- Example: litExpr (stringLit "hello")
litExpr :: Literal -> Expression
litExpr = ExpressionLiteral

-- | Create a string expression
-- Example: stringExpr "hello"
stringExpr :: String -> Expression
stringExpr = litExpr . stringLit

-- | Create an integer expression
-- Example: intExpr 42
intExpr :: I.Int64 -> Expression
intExpr = litExpr . intLit

-- | Create a floating-point expression
-- Example: floatExpr 3.14
floatExpr :: Double -> Expression
floatExpr = litExpr . floatLit

-- | Create a boolean expression
-- Example: boolExpr True
boolExpr :: Bool -> Expression
boolExpr = litExpr . boolLit

-- | The null expression
nullExpr :: Expression
nullExpr = litExpr nullLit

-- | The undefined expression
undefinedExpr :: Expression
undefinedExpr = litExpr undefinedLit

-- | Create an array expression
-- Example: arrayExpr [intExpr 1, intExpr 2, intExpr 3]
arrayExpr :: [Expression] -> Expression
arrayExpr = ExpressionArray . fmap ArrayElementExpression

-- | Create an object expression from key-value pairs
-- Example: objectExpr [("name", stringExpr "John"), ("age", intExpr 30)]
objectExpr :: [(String, Expression)] -> Expression
objectExpr pairs = ExpressionObject $ fmap toProperty pairs
  where
    toProperty (k, v) = Property (identExpr k) v PropertyKindInit False False

-- | Create a function call expression
-- Example: callExpr (identExpr "foo") [intExpr 1, intExpr 2]
callExpr :: Expression -> [Expression] -> Expression
callExpr callee args = ExpressionCall $ CallExpression callee args False

-- | Create an optional chaining call expression
-- Example: optionalCallExpr (identExpr "foo") [intExpr 1]
optionalCallExpr :: Expression -> [Expression] -> Expression
optionalCallExpr callee args = ExpressionCall $ CallExpression callee args True

-- | Create a member access expression (obj.prop)
-- Example: memberExpr (identExpr "obj") "prop"
memberExpr :: Expression -> String -> Expression
memberExpr obj prop = ExpressionMember $ MemberExpression obj (identExpr prop) False False

-- | Create a computed member access expression (obj[expr])
-- Example: computedMemberExpr (identExpr "arr") (intExpr 0)
computedMemberExpr :: Expression -> Expression -> Expression
computedMemberExpr obj prop = ExpressionMember $ MemberExpression obj prop True False

-- | Create an optional chaining member expression (obj?.prop)
-- Example: optionalMemberExpr (identExpr "obj") "prop"
optionalMemberExpr :: Expression -> String -> Expression
optionalMemberExpr obj prop = ExpressionMember $ MemberExpression obj (identExpr prop) False True

-- | Create a conditional (ternary) expression
-- Example: condExpr (identExpr "x") (intExpr 1) (intExpr 0)
condExpr :: Expression -> Expression -> Expression -> Expression
condExpr test consequent alternate = ExpressionConditional $
  ConditionalExpression test consequent alternate

-- | Create a binary expression
-- Example: binaryExpr BinaryOperatorAdd (intExpr 1) (intExpr 2)
binaryExpr :: BinaryOperator -> Expression -> Expression -> Expression
binaryExpr op left right = ExpressionBinary $ BinaryExpression op left right

-- | Create an addition expression
-- Example: addExpr (intExpr 1) (intExpr 2)
addExpr :: Expression -> Expression -> Expression
addExpr = binaryExpr BinaryOperatorAdd

-- | Create a subtraction expression
-- Example: subExpr (intExpr 5) (intExpr 3)
subExpr :: Expression -> Expression -> Expression
subExpr = binaryExpr BinaryOperatorSubtract

-- | Create a multiplication expression
-- Example: mulExpr (intExpr 3) (intExpr 4)
mulExpr :: Expression -> Expression -> Expression
mulExpr = binaryExpr BinaryOperatorMultiply

-- | Create a division expression
-- Example: divExpr (intExpr 10) (intExpr 2)
divExpr :: Expression -> Expression -> Expression
divExpr = binaryExpr BinaryOperatorDivide

-- | Create a strict equality expression (===)
-- Example: eqExpr (identExpr "x") (intExpr 1)
eqExpr :: Expression -> Expression -> Expression
eqExpr = binaryExpr BinaryOperatorStrictEqual

-- | Create a strict inequality expression (!==)
-- Example: neqExpr (identExpr "x") (intExpr 1)
neqExpr :: Expression -> Expression -> Expression
neqExpr = binaryExpr BinaryOperatorStrictNotEqual

-- | Create a logical AND expression
-- Example: andExpr (identExpr "a") (identExpr "b")
andExpr :: Expression -> Expression -> Expression
andExpr = binaryExpr BinaryOperatorAnd

-- | Create a logical OR expression
-- Example: orExpr (identExpr "a") (identExpr "b")
orExpr :: Expression -> Expression -> Expression
orExpr = binaryExpr BinaryOperatorOr

-- | Create a unary expression
-- Example: unaryExpr UnaryOperatorNot (identExpr "x")
unaryExpr :: UnaryOperator -> Expression -> Expression
unaryExpr op arg = ExpressionUnary $ UnaryExpression op arg True

-- | Create a logical NOT expression
-- Example: notExpr (identExpr "x")
notExpr :: Expression -> Expression
notExpr = unaryExpr UnaryOperatorNot

-- | Create a negation expression
-- Example: negateExpr (identExpr "x")
negateExpr :: Expression -> Expression
negateExpr = unaryExpr UnaryOperatorNegate

-- | Create an assignment expression
-- Example: assignExpr (identPattern "x") (intExpr 42)
assignExpr :: Pattern -> Expression -> Expression
assignExpr left right = ExpressionAssignment $
  AssignmentExpression AssignmentOperatorAssign left right

-- | The 'this' expression
thisExpr :: Expression
thisExpr = ExpressionThis

-- | Create a 'new' expression
-- Example: newExpr (identExpr "Date") []
newExpr :: Expression -> [Expression] -> Expression
newExpr callee args = ExpressionNew $ CallExpression callee args False

-- | Create an await expression
-- Example: awaitExpr (callExpr (identExpr "fetch") [stringExpr "url"])
awaitExpr :: Expression -> Expression
awaitExpr = ExpressionAwait

-- | Create a yield expression
-- Example: yieldExpr (Just (intExpr 1))
yieldExpr :: Maybe Expression -> Expression
yieldExpr = ExpressionYield

-- | Create a spread expression
-- Example: spreadExpr (identExpr "arr")
spreadExpr :: Expression -> Expression
spreadExpr = ExpressionSpread . SpreadElement

-- | Create a parenthesized expression
-- Example: parenExpr (addExpr (intExpr 1) (intExpr 2))
parenExpr :: Expression -> Expression
parenExpr = ExpressionParenthesized


-- * Arrow Functions

-- | Create an arrow function with an expression body
-- Example: arrowExpr ["x", "y"] (addExpr (identExpr "x") (identExpr "y"))
arrowExpr :: [String] -> Expression -> Expression
arrowExpr params body = ExpressionArrow $ ArrowFunctionExpression
  (fmap identPattern params)
  (ArrowFunctionBodyExpression body)
  False

-- | Create an async arrow function with an expression body
-- Example: asyncArrowExpr ["x"] (awaitExpr (callExpr (identExpr "fetch") [identExpr "x"]))
asyncArrowExpr :: [String] -> Expression -> Expression
asyncArrowExpr params body = ExpressionArrow $ ArrowFunctionExpression
  (fmap identPattern params)
  (ArrowFunctionBodyExpression body)
  True

-- | Create an arrow function with a block body
-- Example: arrowBlockExpr ["x"] [returnStmt (Just (identExpr "x"))]
arrowBlockExpr :: [String] -> [Statement] -> Expression
arrowBlockExpr params body = ExpressionArrow $ ArrowFunctionExpression
  (fmap identPattern params)
  (ArrowFunctionBodyBlock body)
  False


-- * Patterns

-- | Create an identifier pattern
-- Example: identPattern "x"
identPattern :: String -> Pattern
identPattern = PatternIdentifier . ident

-- | Create an object destructuring pattern
-- Example: objectPattern [identPattern "a", identPattern "b"]
objectPattern :: [Pattern] -> Pattern
objectPattern ps = PatternObject $ ObjectPattern $ fmap toProperty ps
  where
    toProperty p = case p of
      PatternIdentifier i -> ObjectPatternPropertyProperty $
        Property (ExpressionIdentifier i) (ExpressionIdentifier i) PropertyKindInit False True
      _ -> ObjectPatternPropertyProperty $
        Property nullExpr (patternToExpr p) PropertyKindInit False False
    patternToExpr (PatternIdentifier i) = ExpressionIdentifier i
    patternToExpr _ = nullExpr

-- | Create an array destructuring pattern
-- Example: arrayPattern [Just (identPattern "a"), Nothing, Just (identPattern "b")]
arrayPattern :: [Maybe Pattern] -> Pattern
arrayPattern = PatternArray

-- | Create a rest pattern
-- Example: restPattern "args"
restPattern :: String -> Pattern
restPattern = PatternRest . RestElement . identPattern

-- | Create a pattern with a default value
-- Example: defaultPattern "x" (intExpr 0)
defaultPattern :: String -> Expression -> Pattern
defaultPattern name def = PatternAssignment $
  AssignmentPattern (identPattern name) def


-- * Statements

-- | Create an expression statement
-- Example: exprStmt (callExpr (identExpr "console.log") [stringExpr "hello"])
exprStmt :: Expression -> Statement
exprStmt = StatementExpression

-- | Create a block statement
-- Example: blockStmt [exprStmt (intExpr 1), exprStmt (intExpr 2)]
blockStmt :: [Statement] -> Statement
blockStmt = StatementBlock

-- | Create a return statement
-- Example: returnStmt (Just (intExpr 42))
returnStmt :: Maybe Expression -> Statement
returnStmt = StatementReturn

-- | Create a throw statement
-- Example: throwStmt (newExpr (identExpr "Error") [stringExpr "oops"])
throwStmt :: Expression -> Statement
throwStmt = StatementThrow . ThrowStatement

-- | Create a break statement
-- Example: breakStmt Nothing
breakStmt :: Maybe String -> Statement
breakStmt = StatementBreak . fmap ident

-- | Create a continue statement
-- Example: continueStmt Nothing
continueStmt :: Maybe String -> Statement
continueStmt = StatementContinue . fmap ident

-- | Create an if statement
-- Example: ifStmt (identExpr "x") (returnStmt (Just (intExpr 1))) Nothing
ifStmt :: Expression -> Statement -> Maybe Statement -> Statement
ifStmt test consequent alternate = StatementIf $ IfStatement test consequent alternate

-- | Create a while statement
-- Example: whileStmt (boolExpr True) (blockStmt [breakStmt Nothing])
whileStmt :: Expression -> Statement -> Statement
whileStmt test body = StatementWhile $ WhileStatement test body

-- | Create a for statement
-- Example: forStmt (Just (ForInitVariable (constDecl "i" (intExpr 0)))) (Just (binaryExpr BinaryOperatorLessThan (identExpr "i") (intExpr 10))) (Just (unaryExpr UnaryOperatorIncrement (identExpr "i"))) (blockStmt [])
forStmt :: Maybe ForInit -> Maybe Expression -> Maybe Expression -> Statement -> Statement
forStmt init test update body = StatementFor $ ForStatement init test update body

-- | Create a for-of statement
-- Example: forOfStmt False (ForInLeftVariable (constDecl "item" Nothing)) (identExpr "items") (blockStmt [])
forOfStmt :: Bool -> ForInLeft -> Expression -> Statement -> Statement
forOfStmt await left right body = StatementForOf $ ForOfStatement await left right body

-- | Create a try statement
-- Example: tryStmt [exprStmt (callExpr (identExpr "riskyOp") [])] (Just (catchClause (Just (identPattern "e")) [throwStmt (identExpr "e")])) Nothing
tryStmt :: [Statement] -> Maybe CatchClause -> Maybe [Statement] -> Statement
tryStmt block handler finalizer = StatementTry $ TryStatement block handler finalizer

-- | Create a catch clause
-- Example: catchClause (Just (identPattern "e")) [throwStmt (identExpr "e")]
catchClause :: Maybe Pattern -> [Statement] -> CatchClause
catchClause = CatchClause

-- | Create a switch statement
-- Example: switchStmt (identExpr "x") [switchCase (Just (intExpr 1)) [returnStmt (Just (stringExpr "one"))], switchCase Nothing [returnStmt (Just (stringExpr "default"))]]
switchStmt :: Expression -> [SwitchCase] -> Statement
switchStmt discriminant cases = StatementSwitch $ SwitchStatement discriminant cases

-- | Create a switch case
-- Example: switchCase (Just (intExpr 1)) [returnStmt (Just (stringExpr "one"))]
switchCase :: Maybe Expression -> [Statement] -> SwitchCase
switchCase = SwitchCase

-- | An empty statement
emptyStmt :: Statement
emptyStmt = StatementEmpty

-- | A debugger statement
debuggerStmt :: Statement
debuggerStmt = StatementDebugger


-- * Variable Declarations

-- | Create a const declaration
-- Example: constDecl "x" (Just (intExpr 42))
constDecl :: String -> Maybe Expression -> VariableDeclaration
constDecl name init = VariableDeclaration VariableKindConst
  [VariableDeclarator (identPattern name) init]

-- | Create a let declaration
-- Example: letDecl "x" (Just (intExpr 42))
letDecl :: String -> Maybe Expression -> VariableDeclaration
letDecl name init = VariableDeclaration VariableKindLet
  [VariableDeclarator (identPattern name) init]

-- | Create a var declaration
-- Example: varDecl "x" (Just (intExpr 42))
varDecl :: String -> Maybe Expression -> VariableDeclaration
varDecl name init = VariableDeclaration VariableKindVar
  [VariableDeclarator (identPattern name) init]

-- | Create a variable declaration statement
-- Example: varDeclStmt (constDecl "x" (Just (intExpr 42)))
varDeclStmt :: VariableDeclaration -> Statement
varDeclStmt = StatementVariableDeclaration


-- * Function Declarations

-- | Create a function declaration
-- Example: funcDecl "add" ["x", "y"] [returnStmt (Just (addExpr (identExpr "x") (identExpr "y")))]
funcDecl :: String -> [String] -> [Statement] -> FunctionDeclaration
funcDecl name params body = FunctionDeclaration
  (ident name)
  (fmap identPattern params)
  body
  False
  False

-- | Create an async function declaration
-- Example: asyncFuncDecl "fetchData" ["url"] [returnStmt (Just (awaitExpr (callExpr (identExpr "fetch") [identExpr "url"])))]
asyncFuncDecl :: String -> [String] -> [Statement] -> FunctionDeclaration
asyncFuncDecl name params body = FunctionDeclaration
  (ident name)
  (fmap identPattern params)
  body
  True
  False

-- | Create a function declaration statement
-- Example: funcDeclStmt (funcDecl "add" ["x", "y"] [returnStmt (Just (addExpr (identExpr "x") (identExpr "y")))])
funcDeclStmt :: FunctionDeclaration -> Statement
funcDeclStmt = StatementFunctionDeclaration


-- * Class Declarations

-- | Create a class declaration
-- Example: classDecl "Person" Nothing [methodDef "greet" [] [returnStmt (Just (stringExpr "hello"))]]
classDecl :: String -> Maybe Expression -> [MethodDefinition] -> ClassDeclaration
classDecl name superClass body = ClassDeclaration (ident name) superClass body

-- | Create a method definition
-- Example: methodDef "greet" [] [returnStmt (Just (stringExpr "hello"))]
methodDef :: String -> [String] -> [Statement] -> MethodDefinition
methodDef name params body = MethodDefinition
  (identExpr name)
  (FunctionExpression Nothing (fmap identPattern params) body False False)
  MethodKindMethod
  False
  False

-- | Create a constructor definition
-- Example: constructorDef ["name"] [exprStmt (assignExpr (PatternIdentifier (ident "this.name")) (identExpr "name"))]
constructorDef :: [String] -> [Statement] -> MethodDefinition
constructorDef params body = MethodDefinition
  (identExpr "constructor")
  (FunctionExpression Nothing (fmap identPattern params) body False False)
  MethodKindConstructor
  False
  False

-- | Create a static method definition
-- Example: staticMethodDef "create" [] [returnStmt (Just (newExpr (identExpr "Person") []))]
staticMethodDef :: String -> [String] -> [Statement] -> MethodDefinition
staticMethodDef name params body = MethodDefinition
  (identExpr name)
  (FunctionExpression Nothing (fmap identPattern params) body False False)
  MethodKindMethod
  False
  True

-- | Create a class declaration statement
-- Example: classDeclStmt (classDecl "Person" Nothing [])
classDeclStmt :: ClassDeclaration -> Statement
classDeclStmt = StatementClassDeclaration


-- * Module Items

-- | Create a statement module item
-- Example: stmtItem (varDeclStmt (constDecl "x" (Just (intExpr 42))))
stmtItem :: Statement -> ModuleItem
stmtItem = ModuleItemStatement

-- | Create an import declaration
-- Example: importDecl [importNamed "foo" "foo"] "./module.js"
importDecl :: [ImportClause] -> String -> ImportDeclaration
importDecl specifiers source = ImportDeclaration specifiers (StringLiteral source False)

-- | Create a named import
-- Example: importNamed "foo" "bar"  -- import { foo as bar }
importNamed :: String -> String -> ImportClause
importNamed imported local = ImportClauseNamed $
  ImportSpecifier (ident imported) (ident local)

-- | Create a default import
-- Example: importDefault "foo"  -- import foo from ...
importDefault :: String -> ImportClause
importDefault = ImportClauseDefault . ImportDefaultSpecifier . ident

-- | Create a namespace import
-- Example: importNamespace "foo"  -- import * as foo from ...
importNamespace :: String -> ImportClause
importNamespace = ImportClauseNamespace . ImportNamespaceSpecifier . ident

-- | Create an import module item
-- Example: importItem (importDecl [importDefault "React"] "react")
importItem :: ImportDeclaration -> ModuleItem
importItem = ModuleItemImport

-- | Create a named export declaration
-- Example: namedExportDecl [("foo", "foo")] Nothing
namedExportDecl :: [(String, String)] -> Maybe String -> ExportDeclaration
namedExportDecl specifiers source = ExportDeclarationNamed $ NamedExport
  (fmap (\(local, exported) -> ExportSpecifier (ident local) (ident exported)) specifiers)
  (fmap (\s -> StringLiteral s False) source)

-- | Create a default export declaration
-- Example: defaultExportDecl (identExpr "foo")
defaultExportDecl :: Expression -> ExportDeclaration
defaultExportDecl = ExportDeclarationDefault

-- | Create an export declaration for a statement
-- Example: exportDeclStmt (varDeclStmt (constDecl "x" (Just (intExpr 42))))
exportDeclStmt :: Statement -> ExportDeclaration
exportDeclStmt = ExportDeclarationDeclaration

-- | Create an export module item
-- Example: exportItem (defaultExportDecl (identExpr "foo"))
exportItem :: ExportDeclaration -> ModuleItem
exportItem = ModuleItemExport


-- * Programs

-- | Create a module program
-- Example: moduleProgram [importItem (importDecl [importDefault "foo"] "./foo.js"), stmtItem (varDeclStmt (constDecl "x" (Just (intExpr 42))))]
moduleProgram :: [ModuleItem] -> Program
moduleProgram body = Program body SourceTypeModule

-- | Create a script program
-- Example: scriptProgram [stmtItem (varDeclStmt (constDecl "x" (Just (intExpr 42))))]
scriptProgram :: [ModuleItem] -> Program
scriptProgram body = Program body SourceTypeScript


-- * Type Expressions

-- | Create an identifier type expression
-- Example: identType "string"
identType :: String -> TypeExpression
identType = TypeExpressionIdentifier . ident

-- | Create an array type expression
-- Example: arrayType (identType "number")
arrayType :: TypeExpression -> TypeExpression
arrayType = TypeExpressionArray . ArrayTypeExpression

-- | Create a union type expression
-- Example: unionType [identType "string", identType "number"]
unionType :: [TypeExpression] -> TypeExpression
unionType = TypeExpressionUnion

-- | Create an optional type expression
-- Example: optionalType (identType "string")
optionalType :: TypeExpression -> TypeExpression
optionalType = TypeExpressionOptional

-- | Create a parameterized type expression
-- Example: paramType (identType "Array") [identType "string"]
paramType :: TypeExpression -> [TypeExpression] -> TypeExpression
paramType base args = TypeExpressionParameterized $
  ParameterizedTypeExpression base args

-- | The 'any' type
anyType :: TypeExpression
anyType = TypeExpressionAny

-- | The 'void' type
voidType :: TypeExpression
voidType = TypeExpressionVoid

-- | The 'never' type
neverType :: TypeExpression
neverType = TypeExpressionNever


-- * Comments and Documentation

-- | Create a line comment
-- Example: lineComment "This is a comment"
lineComment :: String -> Comment
lineComment = CommentLine

-- | Create a block comment
-- Example: blockComment "This is a\nmultiline comment"
blockComment :: String -> Comment
blockComment = CommentBlock

-- | Create a documentation comment
-- Example: docComment "Description of the function" [paramTag "x" (Just (identType "number")) "The input value"]
docComment :: String -> [DocumentationTag] -> Comment
docComment desc tags = CommentDocumentation $ DocumentationComment desc tags

-- | Create a @param documentation tag
-- Example: paramTag "x" (Just (identType "number")) "The input value"
paramTag :: String -> Maybe TypeExpression -> String -> DocumentationTag
paramTag name typ desc = DocumentationTag "param" typ (Just $ ident name) desc

-- | Create a @returns documentation tag
-- Example: returnsTag (Just (identType "number")) "The result"
returnsTag :: Maybe TypeExpression -> String -> DocumentationTag
returnsTag typ desc = DocumentationTag "returns" typ Nothing desc

-- | Create a @type documentation tag
-- Example: typeTag (identType "number")
typeTag :: TypeExpression -> DocumentationTag
typeTag typ = DocumentationTag "type" (Just typ) Nothing ""
