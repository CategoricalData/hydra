module Hydra.Sources.Go.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel                    hiding (literalType)
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.go.syntax"

define :: String -> Type -> TypeDefinition
define = datatype ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A Go syntax model, based on the Go Language Specification retrieved on 2025-02-05"
      ++ " from https://go.dev/ref/spec"))}
  where
    -- Alphabetical order by local type name, per the definition-ordering style guide
    -- (Validate.Packaging.checkDefinitionOrdering has no section-boundary awareness,
    -- so the grammar-mirror grouping formerly used here cannot be preserved in this
    -- list; the grammar-section comments remain in place below, next to the
    -- definitions themselves, as documentation).
    definitions = [
      addOp,
      aliasDecl,
      annotatedDeclaration,
      arguments,
      arrayType,
      assignOp,
      assignment,
      basicLit,
      binaryExpr,
      binaryOp,
      block,
      breakStmt,
      callExpr,
      channelDirection,
      channelType,
      commCase,
      commClause,
      compositeLit,
      constDecl,
      constSpec,
      continueStmt,
      conversion,
      declaration,
      deferStmt,
      element,
      elementList,
      elseClause,
      embeddedField,
      emptyStmt,
      exprCaseClause,
      exprSwitchStmt,
      expression,
      expressionStmt,
      fallthroughStmt,
      fieldDecl,
      floatLit,
      forClause,
      forClauseOrRange,
      forStmt,
      fullSlice,
      functionBody,
      functionDecl,
      functionLit,
      functionType,
      goStmt,
      gotoStmt,
      identifier,
      ifStmt,
      imaginaryLit,
      importAlias,
      importDecl,
      importPath,
      importSpec,
      incDecStmt,
      index,
      indexExpr,
      intLit,
      interfaceElem,
      interfaceType,
      interpretedStringLit,
      key,
      keyedElement,
      labeledStmt,
      literal,
      literalType,
      literalValue,
      mapType,
      methodDecl,
      methodElem,
      methodExpr,
      goModule,
      mulOp,
      namedField,
      operand,
      operandName,
      packageClause,
      parameterDecl,
      parameters,
      pointerType,
      primaryExpr,
      qualifiedIdent,
      rangeClause,
      rangeVars,
      rawStringLit,
      receiveCase,
      receiver,
      relOp,
      result,
      returnStmt,
      runeLit,
      selectStmt,
      selector,
      selectorExpr,
      sendStmt,
      shortVarDecl,
      signature,
      simpleSlice,
      simpleStmt,
      slice_,
      sliceExpr,
      sliceType,
      sourceFile_,
      statement,
      stringLit,
      structType,
      switchStmt,
      tag,
      topLevelDecl,
      type_,
      typeAssertion,
      typeAssertionExpr,
      typeCaseClause,
      typeConstraint,
      typeDecl,
      typeDef,
      typeElem,
      typeLit,
      typeName,
      typeParamDecl,
      typeParameters,
      typeSpec,
      typeSwitchGuard,
      typeSwitchStmt,
      typeTerm,
      unaryExpr,
      unaryOp,
      unaryOperation,
      varDecl,
      varSpec]

-- ============================================================================
-- Constructs (Hydra-specific, not from grammar)
-- ============================================================================

-- add_op     = "+" | "-" | "|" | "^" .
addOp :: TypeDefinition
addOp = define "AddOp" $
  doc "A Go additive operator: +, -, |, or ^" $
  T.enum [
  "add",
  "subtract",
  "bitwiseOr",
  "bitwiseXor"]

-- AliasDecl = identifier "=" Type .
aliasDecl :: TypeDefinition
aliasDecl = define "AliasDecl" $
  doc "A Go type alias declaration: identifier = Type" $
  T.record [
  "name">: go "Identifier",
  "type">: go "Type"]

annotatedDeclaration :: TypeDefinition
annotatedDeclaration = define "AnnotatedDeclaration" $
  doc "A top-level Go declaration together with its preceding comment" $
  T.record [
  "comment">: T.string,
  "declaration">: go "TopLevelDecl"]

-- Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
arguments :: TypeDefinition
arguments = define "Arguments" $
  doc "The argument list of a Go call expression, with an optional leading type argument and an optional trailing ellipsis for spread calls" $
  T.record [
  "typeArg">: T.optional $ go "Type",
  "expressions">: T.list $ go "Expression",
  "ellipsis">: T.boolean]

-- ArrayType   = "[" ArrayLength "]" ElementType .
-- ArrayLength = Expression .
-- ElementType = Type .
arrayType :: TypeDefinition
arrayType = define "ArrayType" $
  doc "A Go array type: a fixed length and an element type" $
  T.record [
  "length">: go "Expression",
  "element">: go "Type"]

-- assign_op = [ add_op | mul_op ] "=" .
assignOp :: TypeDefinition
assignOp = define "AssignOp" $
  doc "A Go assignment operator: plain =, or an additive/multiplicative compound assignment" $
  T.union [
  "simple">: T.unit,     -- =
  "add">: go "AddOp",    -- +=, -=, |=, ^=
  "mul">: go "MulOp"]    -- *=, /=, %=, <<=, >>=, &=, &^=

-- Assignment = ExpressionList assign_op ExpressionList .
assignment :: TypeDefinition
assignment = define "Assignment" $
  doc "A Go assignment statement: a left-hand expression list, an assignment operator, and a right-hand expression list" $
  T.record [
  "lhs">: nonemptyList $ go "Expression",
  "op">: go "AssignOp",
  "rhs">: nonemptyList $ go "Expression"]

-- BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
basicLit :: TypeDefinition
basicLit = define "BasicLit" $
  doc "A Go basic literal: an integer, floating-point, imaginary, rune, or string literal" $
  T.union [
  "int">: go "IntLit",
  "float">: go "FloatLit",
  "imaginary">: go "ImaginaryLit",
  "rune">: go "RuneLit",
  "string">: go "StringLit"]

-- binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
binaryExpr :: TypeDefinition
binaryExpr = define "BinaryExpr" $
  doc "A Go binary expression: a left operand, a binary operator, and a right operand" $
  T.record [
  "left">: go "Expression",
  "op">: go "BinaryOp",
  "right">: go "Expression"]

binaryOp :: TypeDefinition
binaryOp = define "BinaryOp" $
  doc "A Go binary operator, covering logical, relational, additive, and multiplicative operators" $
  T.enum [
  -- Logical
  "or",          -- ||
  "and",         -- &&
  -- Relational
  "equal",       -- ==
  "notEqual",    -- !=
  "less",        -- <
  "lessEqual",   -- <=
  "greater",     -- >
  "greaterEqual", -- >=
  -- Additive
  "add",         -- +
  "subtract",    -- -
  "bitwiseOr",   -- |
  "bitwiseXor",  -- ^
  -- Multiplicative
  "multiply",    -- *
  "divide",      -- /
  "remainder",   -- %
  "leftShift",   -- <<
  "rightShift",  -- >>
  "bitwiseAnd",  -- &
  "bitClear"]    -- &^

-- Block = "{" StatementList "}" .
-- StatementList = { Statement ";" } .
block :: TypeDefinition
block = define "Block" $
  doc "A Go block: a brace-delimited list of statements" $
  T.wrap $ T.list $ go "Statement"

-- ============================================================================
-- Operators (miscellaneous)
-- ============================================================================

-- BreakStmt = "break" [ Label ] .
breakStmt :: TypeDefinition
breakStmt = define "BreakStmt" $
  doc "A Go break statement, with an optional target label" $
  T.wrap $ T.optional $ go "Identifier"

-- Hydra: Call expression (f(args))
callExpr :: TypeDefinition
callExpr = define "CallExpr" $
  doc "A Go call expression: a function together with its arguments" $
  T.record [
  "function">: go "PrimaryExpr",
  "arguments">: go "Arguments"]

channelDirection :: TypeDefinition
channelDirection = define "ChannelDirection" $
  doc "The direction of a Go channel type: bidirectional, send-only, or receive-only" $
  T.enum [
  "bidirectional", -- chan
  "send",          -- chan<-
  "receive"]       -- <-chan

-- ============================================================================
-- Expressions
-- ============================================================================

-- ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
channelType :: TypeDefinition
channelType = define "ChannelType" $
  doc "A Go channel type: a direction and an element type" $
  T.record [
  "direction">: go "ChannelDirection",
  "element">: go "Type"]

-- CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
-- RecvStmt   = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
-- RecvExpr   = Expression .
commCase :: TypeDefinition
commCase = define "CommCase" $
  doc "A Go communication case in a select statement: a send, a receive, or the default case" $
  T.union [
  "send">: go "SendStmt",
  "receive">: go "ReceiveCase",
  "default">: T.unit]

-- CommClause = CommCase ":" StatementList .
commClause :: TypeDefinition
commClause = define "CommClause" $
  doc "A Go communication clause: a comm case together with its statement list" $
  T.record [
  "case">: go "CommCase",
  "statements">: T.list $ go "Statement"]

-- CompositeLit  = LiteralType LiteralValue .
compositeLit :: TypeDefinition
compositeLit = define "CompositeLit" $
  doc "A Go composite literal: a literal type together with its literal value" $
  T.record [
  "type">: go "LiteralType",
  "value">: go "LiteralValue"]

-- ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
constDecl :: TypeDefinition
constDecl = define "ConstDecl" $
  doc "A Go constant declaration: one or more constant specs" $
  T.wrap $ nonemptyList $ go "ConstSpec"

-- ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
constSpec :: TypeDefinition
constSpec = define "ConstSpec" $
  doc "A Go constant spec: a list of names, an optional type, and a list of value expressions" $
  T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: T.optional $ go "Type",
  "values">: T.list $ go "Expression"]

-- ContinueStmt = "continue" [ Label ] .
continueStmt :: TypeDefinition
continueStmt = define "ContinueStmt" $
  doc "A Go continue statement, with an optional target label" $
  T.wrap $ T.optional $ go "Identifier"

-- Conversion = Type "(" Expression [ "," ] ")" .
conversion :: TypeDefinition
conversion = define "Conversion" $
  doc "A Go type conversion expression: a target type applied to an expression" $
  T.record [
  "type">: go "Type",
  "expression">: go "Expression"]

-- ============================================================================
-- Statements
-- ============================================================================

-- Declaration   = ConstDecl | TypeDecl | VarDecl .
declaration :: TypeDefinition
declaration = define "Declaration" $
  doc "A Go declaration: a constant, type, or variable declaration" $
  T.union [
  "const">: go "ConstDecl",
  "type">: go "TypeDecl",
  "var">: go "VarDecl"]

-- DeferStmt = "defer" Expression .
deferStmt :: TypeDefinition
deferStmt = define "DeferStmt" $
  doc "A Go defer statement, wrapping the deferred expression" $
  T.wrap $ go "Expression"

-- Element       = Expression | LiteralValue .
element :: TypeDefinition
element = define "Element" $
  doc "A Go composite-literal element: an expression or a nested literal value" $
  T.union [
  "expression">: go "Expression",
  "literal">: go "LiteralValue"]

-- ElementList   = KeyedElement { "," KeyedElement } .
elementList :: TypeDefinition
elementList = define "ElementList" $
  doc "A non-empty list of keyed elements in a Go composite literal" $
  T.wrap $ nonemptyList $ go "KeyedElement"

-- Hydra: Else clause (either another if or a block)
elseClause :: TypeDefinition
elseClause = define "ElseClause" $
  doc "The else clause of a Go if statement: another if statement or a block" $
  T.union [
  "if">: go "IfStmt",
  "block">: go "Block"]

-- Hydra: Embedded field (anonymous field)
embeddedField :: TypeDefinition
embeddedField = define "EmbeddedField" $
  doc "A Go embedded (anonymous) struct field: a type name, an optional pointer marker, and an optional tag" $
  T.record [
  "pointer">: T.boolean,
  "type">: go "TypeName",
  "tag">: T.optional $ go "Tag"]

-- EmptyStmt = .
emptyStmt :: TypeDefinition
emptyStmt = define "EmptyStmt" $
  doc "A Go empty statement" $
  T.wrap T.unit

-- ExprCaseClause = ExprSwitchCase ":" StatementList .
-- ExprSwitchCase = "case" ExpressionList | "default" .
exprCaseClause :: TypeDefinition
exprCaseClause = define "ExprCaseClause" $
  doc "A case clause of a Go expression switch: an optional expression list (absent for default) and a statement list" $
  T.record [
  "case">: T.optional $ nonemptyList $ go "Expression", -- Nothing for default
  "statements">: T.list $ go "Statement"]

-- ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
exprSwitchStmt :: TypeDefinition
exprSwitchStmt = define "ExprSwitchStmt" $
  doc "A Go expression switch statement: an optional init statement, an optional tag expression, and its case clauses" $
  T.record [
  "init">: T.optional $ go "SimpleStmt",
  "expression">: T.optional $ go "Expression",
  "cases">: T.list $ go "ExprCaseClause"]

-- Expression = UnaryExpr | Expression binary_op Expression .
expression :: TypeDefinition
expression = define "Expression" $
  doc "A Go expression: a unary expression, or a binary expression combining two expressions" $
  T.union [
  "unary">: go "UnaryExpr",
  "binary">: go "BinaryExpr"]

-- ExpressionStmt = Expression .
expressionStmt :: TypeDefinition
expressionStmt = define "ExpressionStmt" $
  doc "A Go expression statement, wrapping a single expression" $
  T.wrap $ go "Expression"

-- FallthroughStmt = "fallthrough" .
fallthroughStmt :: TypeDefinition
fallthroughStmt = define "FallthroughStmt" $
  doc "A Go fallthrough statement" $
  T.wrap T.unit

-- FieldDecl     = (IdentifierList Type | EmbeddedField) [ Tag ] .
-- EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
fieldDecl :: TypeDefinition
fieldDecl = define "FieldDecl" $
  doc "A Go struct field declaration: a named field or an embedded field" $
  T.union [
  "named">: go "NamedField",
  "embedded">: go "EmbeddedField"]

-- float_lit         = decimal_float_lit | hex_float_lit .
-- decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
--                     decimal_digits decimal_exponent |
--                     "." decimal_digits [ decimal_exponent ] .
-- hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
floatLit :: TypeDefinition
floatLit = define "FloatLit" $
  doc "A Go floating-point literal, in decimal or hexadecimal form" $
  T.wrap T.float64

-- ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
-- InitStmt = SimpleStmt .
-- PostStmt = SimpleStmt .
forClause :: TypeDefinition
forClause = define "ForClause" $
  doc "A Go C-style for-loop clause: an optional init statement, an optional condition, and an optional post statement" $
  T.record [
  "init">: T.optional $ go "SimpleStmt",
  "condition">: T.optional $ go "Expression",
  "post">: T.optional $ go "SimpleStmt"]

-- Hydra: Either a for clause, range clause, or simple condition
forClauseOrRange :: TypeDefinition
forClauseOrRange = define "ForClauseOrRange" $
  doc "The header of a Go for statement: a bare condition, a for clause, or a range clause" $
  T.union [
  "condition">: go "Expression",
  "clause">: go "ForClause",
  "range">: go "RangeClause"]

-- ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
-- Condition = Expression .
forStmt :: TypeDefinition
forStmt = define "ForStmt" $
  doc "A Go for statement: an optional loop header and a body block" $
  T.record [
  "clause">: T.optional $ go "ForClauseOrRange",
  "body">: go "Block"]

-- Hydra: Full slice [lo:hi:max]
fullSlice :: TypeDefinition
fullSlice = define "FullSlice" $
  doc "A Go three-index slice expression, with low, high, and max bounds" $
  T.record [
  "low">: T.optional $ go "Expression",
  "high">: go "Expression",
  "max">: go "Expression"]

-- FunctionBody = Block .
functionBody :: TypeDefinition
functionBody = define "FunctionBody" $
  doc "The body of a Go function: a block" $
  T.wrap $ go "Block"

-- FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
-- FunctionName = identifier .
functionDecl :: TypeDefinition
functionDecl = define "FunctionDecl" $
  doc "A Go top-level function declaration: a name, optional type parameters, a signature, and an optional body" $
  T.record [
  "name">: go "Identifier",
  "typeParams">: T.optional $ go "TypeParameters",
  "signature">: go "Signature",
  "body">: T.optional $ go "FunctionBody"]

-- FunctionLit = "func" Signature FunctionBody .
functionLit :: TypeDefinition
functionLit = define "FunctionLit" $
  doc "A Go function literal: a signature together with its body" $
  T.record [
  "signature">: go "Signature",
  "body">: go "FunctionBody"]

-- FunctionType   = "func" Signature .
functionType :: TypeDefinition
functionType = define "FunctionType" $
  doc "A Go function type: a signature" $
  T.wrap $ go "Signature"

go :: String -> Type
go = typeref ns

goModule :: TypeDefinition
goModule = define "Module" $
  doc "A Go source file: a package clause, imports, and top-level declarations" $
  T.record [
  "package">: go "PackageClause",
  "imports">: T.list $ go "ImportDecl",
  "declarations">: T.list $ go "TopLevelDecl"]

-- ============================================================================
-- Terminals
-- ============================================================================

-- GoStmt = "go" Expression .
goStmt :: TypeDefinition
goStmt = define "GoStmt" $
  doc "A Go go statement, launching the given expression as a goroutine" $
  T.wrap $ go "Expression"

-- GotoStmt = "goto" Label .
gotoStmt :: TypeDefinition
gotoStmt = define "GotoStmt" $
  doc "A Go goto statement, naming its target label" $
  T.wrap $ go "Identifier"

-- identifier = letter { letter | unicode_digit } .
identifier :: TypeDefinition
identifier = define "Identifier" $
  doc "A Go identifier: a letter followed by letters and unicode digits" $
  T.wrap T.string

-- IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
ifStmt :: TypeDefinition
ifStmt = define "IfStmt" $
  doc "A Go if statement: an optional init statement, a condition, a then block, and an optional else clause" $
  T.record [
  "init">: T.optional $ go "SimpleStmt",
  "condition">: go "Expression",
  "then">: go "Block",
  "else">: T.optional $ go "ElseClause"]

-- imaginary_lit = (decimal_digits | int_lit | float_lit) "i" .
imaginaryLit :: TypeDefinition
imaginaryLit = define "ImaginaryLit" $
  doc "A Go imaginary literal: a decimal or floating-point literal suffixed with 'i'" $
  T.wrap T.float64

-- Hydra: ImportAlias represents the optional alias in an import
-- Either "." for dot import, or an identifier for named alias
importAlias :: TypeDefinition
importAlias = define "ImportAlias" $
  doc "The optional alias of a Go import spec: a dot for a dot import, or an identifier for a named alias" $
  T.union [
  "dot">: T.unit,
  "name">: go "Identifier"]

-- ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
importDecl :: TypeDefinition
importDecl = define "ImportDecl" $
  doc "A Go import declaration: one or more import specs" $
  T.wrap $ nonemptyList $ go "ImportSpec"

-- ImportPath       = string_lit .
importPath :: TypeDefinition
importPath = define "ImportPath" $
  doc "The path string of a Go import spec" $
  T.wrap $ go "StringLit"

-- ============================================================================
-- Declarations
-- ============================================================================

-- ImportSpec       = [ "." | PackageName ] ImportPath .
importSpec :: TypeDefinition
importSpec = define "ImportSpec" $
  doc "A Go import spec: an optional dot or package-name alias together with an import path" $
  T.record [
  "alias">: T.optional $ go "ImportAlias",
  "path">: go "ImportPath"]

-- IncDecStmt = Expression ( "++" | "--" ) .
incDecStmt :: TypeDefinition
incDecStmt = define "IncDecStmt" $
  doc "A Go increment or decrement statement: an expression followed by ++ or --" $
  T.record [
  "expression">: go "Expression",
  "increment">: T.boolean] -- true for ++, false for --

-- Index          = "[" Expression [ "," ] "]" .
index :: TypeDefinition
index = define "Index" $
  doc "A Go index expression's bracketed index" $
  T.wrap $ nonemptyList $ go "Expression"

-- Hydra: Index expression (x[i])
indexExpr :: TypeDefinition
indexExpr = define "IndexExpr" $
  doc "A Go index expression: a primary expression subscripted by an index" $
  T.record [
  "expr">: go "PrimaryExpr",
  "index">: go "Expression"]

-- int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
-- decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
-- binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
-- octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
-- hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .
intLit :: TypeDefinition
intLit = define "IntLit" $
  doc "A Go integer literal, in decimal, binary, octal, or hexadecimal form" $
  T.wrap T.bigint

-- InterfaceElem  = MethodElem | TypeElem .
interfaceElem :: TypeDefinition
interfaceElem = define "InterfaceElem" $
  doc "A Go interface element: a method element or a type element" $
  T.union [
  "method">: go "MethodElem",
  "type">: go "TypeElem"]

-- InterfaceType  = "interface" "{" { InterfaceElem ";" } "}" .
interfaceType :: TypeDefinition
interfaceType = define "InterfaceType" $
  doc "A Go interface type: a brace-delimited list of interface elements" $
  T.wrap $ T.list $ go "InterfaceElem"

-- interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
interpretedStringLit :: TypeDefinition
interpretedStringLit = define "InterpretedStringLit" $
  doc "A Go interpreted (double-quoted) string literal" $
  T.wrap T.string

-- ============================================================================
-- Source file structure
-- ============================================================================

-- Key           = FieldName | Expression | LiteralValue .
-- FieldName     = identifier .
key :: TypeDefinition
key = define "Key" $
  doc "The key of a Go keyed element: a field name, an expression, or a literal value" $
  T.union [
  "field">: go "Identifier",
  "expression">: go "Expression",
  "literal">: go "LiteralValue"]

-- KeyedElement  = [ Key ":" ] Element .
keyedElement :: TypeDefinition
keyedElement = define "KeyedElement" $
  doc "A Go composite-literal element with an optional key" $
  T.record [
  "key">: T.optional $ go "Key",
  "element">: go "Element"]

-- LabeledStmt = Label ":" Statement .
-- Label       = identifier .
labeledStmt :: TypeDefinition
labeledStmt = define "LabeledStmt" $
  doc "A Go labeled statement: a label together with the statement it labels" $
  T.record [
  "label">: go "Identifier",
  "statement">: go "Statement"]

-- Literal     = BasicLit | CompositeLit | FunctionLit .
literal :: TypeDefinition
literal = define "Literal" $
  doc "A Go literal: a basic literal, a composite literal, or a function literal" $
  T.union [
  "basic">: go "BasicLit",
  "composite">: go "CompositeLit",
  "function">: go "FunctionLit"]

-- LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
--                 SliceType | MapType | TypeName [ TypeArgs ] .
literalType :: TypeDefinition
literalType = define "LiteralType" $
  doc "The type of a Go composite literal: a struct, array, elided-length array, slice, map, or named type" $
  T.union [
  "struct">: go "StructType",
  "array">: go "ArrayType",
  "inferredArray">: go "Type", -- [...]T
  "slice">: go "SliceType",
  "map">: go "MapType",
  "name">: go "TypeName"]

-- LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
literalValue :: TypeDefinition
literalValue = define "LiteralValue" $
  doc "A Go composite literal's brace-delimited value: an optional element list" $
  T.wrap $ T.list $ go "KeyedElement"

-- MapType     = "map" "[" KeyType "]" ElementType .
-- KeyType     = Type .
mapType :: TypeDefinition
mapType = define "MapType" $
  doc "A Go map type: a key type and an element type" $
  T.record [
  "key">: go "Type",
  "value">: go "Type"]

-- MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
-- MethodName = identifier .
methodDecl :: TypeDefinition
methodDecl = define "MethodDecl" $
  doc "A Go method declaration: a receiver, a method name, a signature, and an optional body" $
  T.record [
  "receiver">: go "Receiver",
  "name">: go "Identifier",
  "signature">: go "Signature",
  "body">: T.optional $ go "FunctionBody"]

-- MethodElem     = MethodName Signature .
methodElem :: TypeDefinition
methodElem = define "MethodElem" $
  doc "A Go interface method element: a method name together with its signature" $
  T.record [
  "name">: go "Identifier",
  "signature">: go "Signature"]

-- MethodExpr    = ReceiverType "." MethodName .
-- ReceiverType  = Type .
methodExpr :: TypeDefinition
methodExpr = define "MethodExpr" $
  doc "A Go method expression: a receiver type together with a method name" $
  T.record [
  "receiver">: go "Type",
  "method">: go "Identifier"]

-- mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
mulOp :: TypeDefinition
mulOp = define "MulOp" $
  doc "A Go multiplicative operator: *, /, %, <<, >>, &, or &^" $
  T.enum [
  "multiply",
  "divide",
  "remainder",
  "leftShift",
  "rightShift",
  "bitwiseAnd",
  "bitClear"]

-- Hydra: Named field with names, type, and optional tag
namedField :: TypeDefinition
namedField = define "NamedField" $
  doc "A Go named struct field: a list of names, a type, and an optional tag" $
  T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: go "Type",
  "tag">: T.optional $ go "Tag"]

-- Operand     = Literal | OperandName [ TypeArgs ] | "(" Expression ")" .
-- OperandName = identifier | QualifiedIdent .
operand :: TypeDefinition
operand = define "Operand" $
  doc "A Go operand: a literal, an operand name with optional type arguments, or a parenthesized expression" $
  T.union [
  "literal">: go "Literal",
  -- Note: field named "named" rather than "name" to avoid a constructor-name
  -- collision with the separately defined OperandName record type below (see
  -- the analogous Type/TypeName fix above for the same pattern).
  "named">: go "OperandName",
  "paren">: go "Expression"]

-- Hydra: Operand name with optional type arguments
operandName :: TypeDefinition
operandName = define "OperandName" $
  doc "A Go operand name: a plain identifier or a package-qualified identifier" $
  T.record [
  "name">: go "QualifiedIdent",
  "typeArgs">: T.list $ go "Type"]

-- PackageClause  = "package" PackageName .
-- PackageName    = identifier .
packageClause :: TypeDefinition
packageClause = define "PackageClause" $
  doc "A Go package clause, naming the package" $
  T.wrap $ go "Identifier"

-- ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
parameterDecl :: TypeDefinition
parameterDecl = define "ParameterDecl" $
  doc "A Go parameter declaration: an optional identifier list, an optional variadic marker, and a type" $
  T.record [
  "names">: T.list $ go "Identifier",
  "variadic">: T.boolean,
  "type">: go "Type"]

-- Parameters     = "(" [ ParameterList [ "," ] ] ")" .
-- ParameterList  = ParameterDecl { "," ParameterDecl } .
parameters :: TypeDefinition
parameters = define "Parameters" $
  doc "A Go parameter list, parenthesized and comma-separated" $
  T.wrap $ T.list $ go "ParameterDecl"

-- PointerType = "*" BaseType .
-- BaseType    = Type .
pointerType :: TypeDefinition
pointerType = define "PointerType" $
  doc "A Go pointer type: a pointer to a base type" $
  T.wrap $ go "Type"

-- PrimaryExpr =
--     Operand |
--     Conversion |
--     MethodExpr |
--     PrimaryExpr Selector |
--     PrimaryExpr Index |
--     PrimaryExpr Slice |
--     PrimaryExpr TypeAssertion |
--     PrimaryExpr Arguments .
primaryExpr :: TypeDefinition
primaryExpr = define "PrimaryExpr" $
  doc "A Go primary expression: an operand, conversion, or method expression, possibly followed by a selector, index, slice, type assertion, or call" $
  T.union [
  "operand">: go "Operand",
  "conversion">: go "Conversion",
  "methodExpr">: go "MethodExpr",
  "selector">: go "SelectorExpr",
  "index">: go "IndexExpr",
  "slice">: go "SliceExpr",
  "typeAssertion">: go "TypeAssertionExpr",
  "call">: go "CallExpr"]

-- QualifiedIdent = PackageName "." identifier .
qualifiedIdent :: TypeDefinition
qualifiedIdent = define "QualifiedIdent" $
  doc "A Go qualified identifier: a package name together with an identifier" $
  T.record [
  "package">: T.optional $ go "Identifier",
  "name">: go "Identifier"]

-- RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
rangeClause :: TypeDefinition
rangeClause = define "RangeClause" $
  doc "A Go range clause: an optional assignment or short variable declaration over a ranged expression" $
  T.record [
  "vars">: T.optional $ go "RangeVars",
  "expression">: go "Expression"]

-- Hydra: Range variables (either assignment or short declaration)
rangeVars :: TypeDefinition
rangeVars = define "RangeVars" $
  doc "The loop variables of a Go range clause: an assignment or a short variable declaration" $
  T.union [
  "assign">: nonemptyList $ go "Expression",
  "declare">: nonemptyList $ go "Identifier"]

-- raw_string_lit         = "`" { unicode_char | newline } "`" .
rawStringLit :: TypeDefinition
rawStringLit = define "RawStringLit" $
  doc "A Go raw (back-quoted) string literal" $
  T.wrap T.string

-- Hydra: Receive case with optional assignment
receiveCase :: TypeDefinition
receiveCase = define "ReceiveCase" $
  doc "A Go receive case in a select statement, with an optional assignment or declaration" $
  T.record [
  "vars">: T.optional $ go "RangeVars", -- reuse RangeVars type
  "expression">: go "Expression"]

-- Receiver       = Parameters .
-- (Note: Go spec says the receiver must be a single non-variadic parameter)
receiver :: TypeDefinition
receiver = define "Receiver" $
  doc "The receiver of a Go method declaration: a single non-variadic parameter" $
  T.record [
  "name">: T.optional $ go "Identifier",
  "type">: go "Type"]

-- ============================================================================
-- Types
-- ============================================================================

-- rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
relOp :: TypeDefinition
relOp = define "RelOp" $
  doc "A Go relational operator: ==, !=, <, <=, >, or >=" $
  T.enum [
  "equal",
  "notEqual",
  "less",
  "lessEqual",
  "greater",
  "greaterEqual"]

-- Result         = Parameters | Type .
result :: TypeDefinition
result = define "Result" $
  doc "The result of a Go function signature: a parameter list or a bare type" $
  T.union [
  "parameters">: go "Parameters",
  "type">: go "Type"]

-- ReturnStmt = "return" [ ExpressionList ] .
returnStmt :: TypeDefinition
returnStmt = define "ReturnStmt" $
  doc "A Go return statement, with an optional expression list" $
  T.wrap $ T.list $ go "Expression"

-- rune_lit         = "'" ( unicode_value | byte_value ) "'" .
runeLit :: TypeDefinition
runeLit = define "RuneLit" $
  doc "A Go rune literal" $
  T.wrap T.int32 -- rune is int32

-- SelectStmt = "select" "{" { CommClause } "}" .
selectStmt :: TypeDefinition
selectStmt = define "SelectStmt" $
  doc "A Go select statement: a brace-delimited list of communication clauses" $
  T.wrap $ T.list $ go "CommClause"

-- Selector       = "." identifier .
selector :: TypeDefinition
selector = define "Selector" $
  doc "A Go selector: a dot followed by an identifier" $
  T.wrap $ go "Identifier"

-- Hydra: Selector expression (x.y)
selectorExpr :: TypeDefinition
selectorExpr = define "SelectorExpr" $
  doc "A Go selector expression: a primary expression together with a field or method selector" $
  T.record [
  "expr">: go "PrimaryExpr",
  "selector">: go "Identifier"]

-- SendStmt = Channel "<-" Expression .
-- Channel  = Expression .
sendStmt :: TypeDefinition
sendStmt = define "SendStmt" $
  doc "A Go send statement: a channel expression together with the value sent on it" $
  T.record [
  "channel">: go "Expression",
  "value">: go "Expression"]

-- ShortVarDecl = IdentifierList ":=" ExpressionList .
shortVarDecl :: TypeDefinition
shortVarDecl = define "ShortVarDecl" $
  doc "A Go short variable declaration: an identifier list assigned via :=" $
  T.record [
  "names">: nonemptyList $ go "Identifier",
  "values">: nonemptyList $ go "Expression"]

-- Signature      = Parameters [ Result ] .
signature :: TypeDefinition
signature = define "Signature" $
  doc "A Go function signature: a parameter list and an optional result" $
  T.record [
  "parameters">: go "Parameters",
  "result">: T.optional $ go "Result"]

-- Hydra: Simple slice [lo:hi]
simpleSlice :: TypeDefinition
simpleSlice = define "SimpleSlice" $
  doc "A Go two-index slice expression, with optional low and high bounds" $
  T.record [
  "low">: T.optional $ go "Expression",
  "high">: T.optional $ go "Expression"]

-- SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
simpleStmt :: TypeDefinition
simpleStmt = define "SimpleStmt" $
  doc "A Go simple statement: an empty, expression, send, increment/decrement, assignment, or short variable declaration statement" $
  T.union [
  "empty">: go "EmptyStmt",
  "expression">: go "ExpressionStmt",
  "send">: go "SendStmt",
  "incDec">: go "IncDecStmt",
  "assignment">: go "Assignment",
  "shortVarDecl">: go "ShortVarDecl"]

-- Hydra: Slice expression (x[lo:hi] or x[lo:hi:max])
sliceExpr :: TypeDefinition
sliceExpr = define "SliceExpr" $
  doc "A Go slice expression: a primary expression together with a simple or full slice" $
  T.record [
  "expr">: go "PrimaryExpr",
  "slice">: go "Slice"]

-- SliceType = "[" "]" ElementType .
sliceType :: TypeDefinition
sliceType = define "SliceType" $
  doc "A Go slice type: an element type" $
  T.wrap $ go "Type"

-- Slice          = "[" [ Expression ] ":" [ Expression ] "]" |
--                  "[" [ Expression ] ":" Expression ":" Expression "]" .
slice_ :: TypeDefinition
slice_ = define "Slice" $
  doc "A Go slice expression's bracketed bounds, either two-index or three-index" $
  T.union [
  "simple">: go "SimpleSlice",
  "full">: go "FullSlice"]

-- SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
sourceFile_ :: TypeDefinition
sourceFile_ = define "SourceFile" $
  doc "A Go source file: a package clause, imports, and top-level declarations" $
  T.record [
  "package">: go "PackageClause",
  "imports">: T.list $ go "ImportDecl",
  "declarations">: T.list $ go "TopLevelDecl"]

-- Statement =
--     Declaration | LabeledStmt | SimpleStmt |
--     GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
--     FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
--     DeferStmt .
statement :: TypeDefinition
statement = define "Statement" $
  doc "A Go statement, covering every statement form in the grammar" $
  T.union [
  "declaration">: go "Declaration",
  "labeled">: go "LabeledStmt",
  "simple">: go "SimpleStmt",
  "go">: go "GoStmt",
  "return">: go "ReturnStmt",
  "break">: go "BreakStmt",
  "continue">: go "ContinueStmt",
  "goto">: go "GotoStmt",
  "fallthrough">: go "FallthroughStmt",
  "block">: go "Block",
  "if">: go "IfStmt",
  "switch">: go "SwitchStmt",
  "select">: go "SelectStmt",
  "for">: go "ForStmt",
  "defer">: go "DeferStmt"]

-- string_lit             = raw_string_lit | interpreted_string_lit .
stringLit :: TypeDefinition
stringLit = define "StringLit" $
  doc "A Go string literal: a raw or interpreted string literal" $
  T.union [
  "raw">: go "RawStringLit",
  "interpreted">: go "InterpretedStringLit"]

-- StructType    = "struct" "{" { FieldDecl ";" } "}" .
structType :: TypeDefinition
structType = define "StructType" $
  doc "A Go struct type: a brace-delimited list of field declarations" $
  T.wrap $ T.list $ go "FieldDecl"

-- SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
switchStmt :: TypeDefinition
switchStmt = define "SwitchStmt" $
  doc "A Go switch statement: an expression switch or a type switch" $
  T.union [
  "expression">: go "ExprSwitchStmt",
  "type">: go "TypeSwitchStmt"]

-- Tag = string_lit .
tag :: TypeDefinition
tag = define "Tag" $
  doc "A Go struct field tag: a string literal" $
  T.wrap $ go "StringLit"

-- TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
topLevelDecl :: TypeDefinition
topLevelDecl = define "TopLevelDecl" $
  doc "A Go top-level declaration: a plain declaration, a function declaration, or a method declaration" $
  T.union [
  "declaration">: go "Declaration",
  "function">: go "FunctionDecl",
  "method">: go "MethodDecl"]

-- TypeAssertion = "." "(" Type ")" .
typeAssertion :: TypeDefinition
typeAssertion = define "TypeAssertion" $
  doc "A Go type assertion's parenthesized target type" $
  T.wrap $ go "Type"

-- Hydra: Type assertion expression (x.(T))
typeAssertionExpr :: TypeDefinition
typeAssertionExpr = define "TypeAssertionExpr" $
  doc "A Go type assertion expression: a primary expression together with its asserted type" $
  T.record [
  "expr">: go "PrimaryExpr",
  "type">: go "Type"]

-- TypeCaseClause  = TypeSwitchCase ":" StatementList .
-- TypeSwitchCase  = "case" TypeList | "default" .
-- TypeList        = Type { "," Type } .
typeCaseClause :: TypeDefinition
typeCaseClause = define "TypeCaseClause" $
  doc "A case clause of a Go type switch: an optional type list (absent for default) and a statement list" $
  T.record [
  "case">: T.optional $ nonemptyList $ go "Type", -- Nothing for default
  "statements">: T.list $ go "Statement"]

-- TypeConstraint = TypeElem .
typeConstraint :: TypeDefinition
typeConstraint = define "TypeConstraint" $
  doc "A Go type parameter constraint: a type element" $
  T.wrap $ go "TypeElem"

-- TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
typeDecl :: TypeDefinition
typeDecl = define "TypeDecl" $
  doc "A Go type declaration: one or more type specs" $
  T.wrap $ nonemptyList $ go "TypeSpec"

-- TypeDef = identifier [ TypeParameters ] Type .
typeDef :: TypeDefinition
typeDef = define "TypeDef" $
  doc "A Go type definition: an identifier, optional type parameters, and an underlying type" $
  T.record [
  "name">: go "Identifier",
  "typeParams">: T.optional $ go "TypeParameters",
  "type">: go "Type"]

-- TypeElem       = TypeTerm { "|" TypeTerm } .
typeElem :: TypeDefinition
typeElem = define "TypeElem" $
  doc "A Go type element: one or more type terms joined by union operators" $
  T.wrap $ nonemptyList $ go "TypeTerm"

-- TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
--             SliceType | MapType | ChannelType .
typeLit :: TypeDefinition
typeLit = define "TypeLit" $
  doc "A Go type literal: an array, struct, pointer, function, interface, slice, map, or channel type" $
  T.union [
  "array">: go "ArrayType",
  "struct">: go "StructType",
  "pointer">: go "PointerType",
  "function">: go "FunctionType",
  "interface">: go "InterfaceType",
  "slice">: go "SliceType",
  "map">: go "MapType",
  "channel">: go "ChannelType"]

-- TypeName  = identifier | QualifiedIdent .
typeName :: TypeDefinition
typeName = define "TypeName" $
  doc "A Go type name: a plain identifier or a package-qualified identifier" $
  T.record [
  "name">: go "QualifiedIdent",
  "typeArgs">: T.list $ go "Type"]

-- TypeParamDecl = IdentifierList TypeConstraint .
typeParamDecl :: TypeDefinition
typeParamDecl = define "TypeParamDecl" $
  doc "A Go type parameter declaration: an identifier list together with a type constraint" $
  T.record [
  "names">: nonemptyList $ go "Identifier",
  "constraint">: go "TypeConstraint"]

-- TypeParameters  = "[" TypeParamList [ "," ] "]" .
-- TypeParamList   = TypeParamDecl { "," TypeParamDecl } .
typeParameters :: TypeDefinition
typeParameters = define "TypeParameters" $
  doc "A Go type parameter list, bracketed and comma-separated" $
  T.wrap $ nonemptyList $ go "TypeParamDecl"

-- TypeSpec = AliasDecl | TypeDef .
typeSpec :: TypeDefinition
typeSpec = define "TypeSpec" $
  doc "A Go type spec: an alias declaration or a type definition" $
  T.union [
  "alias">: go "AliasDecl",
  "definition">: go "TypeDef"]

-- TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
typeSwitchGuard :: TypeDefinition
typeSwitchGuard = define "TypeSwitchGuard" $
  doc "The guard of a Go type switch: an optional bound identifier together with the asserted primary expression" $
  T.record [
  "name">: T.optional $ go "Identifier",
  "expression">: go "PrimaryExpr"]

-- TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
typeSwitchStmt :: TypeDefinition
typeSwitchStmt = define "TypeSwitchStmt" $
  doc "A Go type switch statement: an optional init statement, a type switch guard, and its case clauses" $
  T.record [
  "init">: T.optional $ go "SimpleStmt",
  "guard">: go "TypeSwitchGuard",
  "cases">: T.list $ go "TypeCaseClause"]

-- TypeTerm       = Type | UnderlyingType .
-- UnderlyingType = "~" Type .
typeTerm :: TypeDefinition
typeTerm = define "TypeTerm" $
  doc "A Go type element term: a type, or its underlying-type form" $
  T.record [
  "underlying">: T.boolean, -- true if ~Type
  "type">: go "Type"]

-- Type      = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
type_ :: TypeDefinition
type_ = define "Type" $
  doc "A Go type: a type name with optional type arguments, a type literal, or a parenthesized type" $
  T.union [
  -- Note: field named "named" rather than "name" to avoid a constructor-name
  -- collision: the synthesized variant constructor for a "name" field on the
  -- "Type" union would be "TypeName", which conflicts with the separately
  -- defined TypeName record type above.
  "named">: go "TypeName",
  "literal">: go "TypeLit",
  "paren">: go "Type"]

-- UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
unaryExpr :: TypeDefinition
unaryExpr = define "UnaryExpr" $
  doc "A Go unary expression: a primary expression or a unary operator applied to a unary expression" $
  T.union [
  "primary">: go "PrimaryExpr",
  "op">: go "UnaryOperation"]

-- unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
unaryOp :: TypeDefinition
unaryOp = define "UnaryOp" $
  doc "A Go unary operator: +, -, !, ^, *, &, or <-" $
  T.enum [
  "plus",       -- +
  "minus",      -- -
  "not",        -- !
  "xor",        -- ^
  "deref",      -- *
  "addressOf",  -- &
  "receive"]    -- <-

-- Hydra: Unary operation with operator and operand
unaryOperation :: TypeDefinition
unaryOperation = define "UnaryOperation" $
  doc "A Go unary expression's operator together with its operand" $
  T.record [
  "op">: go "UnaryOp",
  "operand">: go "UnaryExpr"]

-- VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
varDecl :: TypeDefinition
varDecl = define "VarDecl" $
  doc "A Go variable declaration: one or more var specs" $
  T.wrap $ nonemptyList $ go "VarSpec"

-- VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
varSpec :: TypeDefinition
varSpec = define "VarSpec" $
  doc "A Go var spec: an identifier list with a type, an initializer expression list, or both" $
  T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: T.optional $ go "Type",
  "values">: T.list $ go "Expression"]
