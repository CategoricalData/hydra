module Hydra.Sources.Go.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel                    hiding (literalType)
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
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
    definitions = constructs ++ terminals ++ sourceFile ++ declarations ++ types
      ++ expressions ++ statements ++ miscellaneous

    -- These definitions are not based on the grammar, but are convenient for working with Go sources in Hydra.
    constructs = [
      annotatedDeclaration,
      goModule]

    -- Terminal tokens
    terminals = [
      identifier,
      intLit,
      floatLit,
      imaginaryLit,
      runeLit,
      stringLit,
      rawStringLit,
      interpretedStringLit]

    -- Source file structure
    sourceFile = [
      sourceFile_,
      packageClause,
      importDecl,
      importSpec,
      importAlias,
      importPath]

    -- Declarations
    declarations = [
      declaration,
      topLevelDecl,
      constDecl,
      constSpec,
      varDecl,
      varSpec,
      shortVarDecl,
      typeDecl,
      typeSpec,
      aliasDecl,
      typeDef,
      typeParameters,
      typeParamDecl,
      typeConstraint,
      functionDecl,
      functionBody,
      methodDecl,
      receiver]

    -- Types
    types = [
      type_,
      typeName,
      qualifiedIdent,
      typeLit,
      arrayType,
      sliceType,
      structType,
      fieldDecl,
      namedField,
      embeddedField,
      tag,
      pointerType,
      functionType,
      signature,
      result,
      parameters,
      parameterDecl,
      interfaceType,
      interfaceElem,
      methodElem,
      typeElem,
      typeTerm,
      mapType,
      channelType,
      channelDirection]

    -- Expressions
    expressions = [
      expression,
      unaryExpr,
      unaryOperation,
      binaryExpr,
      binaryOp,
      primaryExpr,
      selectorExpr,
      indexExpr,
      sliceExpr,
      typeAssertionExpr,
      callExpr,
      operand,
      operandName,
      literal,
      basicLit,
      compositeLit,
      literalType,
      literalValue,
      elementList,
      keyedElement,
      key,
      element,
      functionLit,
      selector,
      index,
      slice_,
      simpleSlice,
      fullSlice,
      typeAssertion,
      arguments,
      methodExpr,
      conversion]

    -- Statements
    statements = [
      statement,
      simpleStmt,
      emptyStmt,
      labeledStmt,
      expressionStmt,
      sendStmt,
      incDecStmt,
      assignment,
      assignOp,
      ifStmt,
      elseClause,
      switchStmt,
      exprSwitchStmt,
      exprCaseClause,
      typeSwitchStmt,
      typeSwitchGuard,
      typeCaseClause,
      forStmt,
      forClauseOrRange,
      forClause,
      rangeClause,
      rangeVars,
      goStmt,
      selectStmt,
      commClause,
      commCase,
      receiveCase,
      returnStmt,
      breakStmt,
      continueStmt,
      gotoStmt,
      fallthroughStmt,
      deferStmt,
      block]

    -- Miscellaneous
    miscellaneous = [
      unaryOp,
      mulOp,
      addOp,
      relOp]

-- ============================================================================
-- Constructs (Hydra-specific, not from grammar)
-- ============================================================================

-- add_op     = "+" | "-" | "|" | "^" .
addOp :: TypeDefinition
addOp = define "AddOp" $ T.enum [
  "add",
  "subtract",
  "bitwiseOr",
  "bitwiseXor"]

-- AliasDecl = identifier "=" Type .
aliasDecl :: TypeDefinition
aliasDecl = define "AliasDecl" $ T.record [
  "name">: go "Identifier",
  "type">: go "Type"]

annotatedDeclaration :: TypeDefinition
annotatedDeclaration = define "AnnotatedDeclaration" $ T.record [
  "comment">: T.string,
  "declaration">: go "TopLevelDecl"]

-- Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
arguments :: TypeDefinition
arguments = define "Arguments" $ T.record [
  "typeArg">: T.optional $ go "Type",
  "expressions">: T.list $ go "Expression",
  "ellipsis">: T.boolean]

-- ArrayType   = "[" ArrayLength "]" ElementType .
-- ArrayLength = Expression .
-- ElementType = Type .
arrayType :: TypeDefinition
arrayType = define "ArrayType" $ T.record [
  "length">: go "Expression",
  "element">: go "Type"]

-- assign_op = [ add_op | mul_op ] "=" .
assignOp :: TypeDefinition
assignOp = define "AssignOp" $ T.union [
  "simple">: T.unit,     -- =
  "add">: go "AddOp",    -- +=, -=, |=, ^=
  "mul">: go "MulOp"]    -- *=, /=, %=, <<=, >>=, &=, &^=

-- Assignment = ExpressionList assign_op ExpressionList .
assignment :: TypeDefinition
assignment = define "Assignment" $ T.record [
  "lhs">: nonemptyList $ go "Expression",
  "op">: go "AssignOp",
  "rhs">: nonemptyList $ go "Expression"]

-- BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
basicLit :: TypeDefinition
basicLit = define "BasicLit" $ T.union [
  "int">: go "IntLit",
  "float">: go "FloatLit",
  "imaginary">: go "ImaginaryLit",
  "rune">: go "RuneLit",
  "string">: go "StringLit"]

-- binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
binaryExpr :: TypeDefinition
binaryExpr = define "BinaryExpr" $ T.record [
  "left">: go "Expression",
  "op">: go "BinaryOp",
  "right">: go "Expression"]

binaryOp :: TypeDefinition
binaryOp = define "BinaryOp" $ T.enum [
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
block = define "Block" $ T.wrap $ T.list $ go "Statement"

-- ============================================================================
-- Operators (miscellaneous)
-- ============================================================================

-- BreakStmt = "break" [ Label ] .
breakStmt :: TypeDefinition
breakStmt = define "BreakStmt" $ T.wrap $ T.optional $ go "Identifier"

-- Hydra: Call expression (f(args))
callExpr :: TypeDefinition
callExpr = define "CallExpr" $ T.record [
  "function">: go "PrimaryExpr",
  "arguments">: go "Arguments"]

channelDirection :: TypeDefinition
channelDirection = define "ChannelDirection" $ T.enum [
  "bidirectional", -- chan
  "send",          -- chan<-
  "receive"]       -- <-chan

-- ============================================================================
-- Expressions
-- ============================================================================

-- ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
channelType :: TypeDefinition
channelType = define "ChannelType" $ T.record [
  "direction">: go "ChannelDirection",
  "element">: go "Type"]

-- CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
-- RecvStmt   = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
-- RecvExpr   = Expression .
commCase :: TypeDefinition
commCase = define "CommCase" $ T.union [
  "send">: go "SendStmt",
  "receive">: go "ReceiveCase",
  "default">: T.unit]

-- CommClause = CommCase ":" StatementList .
commClause :: TypeDefinition
commClause = define "CommClause" $ T.record [
  "case">: go "CommCase",
  "statements">: T.list $ go "Statement"]

-- CompositeLit  = LiteralType LiteralValue .
compositeLit :: TypeDefinition
compositeLit = define "CompositeLit" $ T.record [
  "type">: go "LiteralType",
  "value">: go "LiteralValue"]

-- ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
constDecl :: TypeDefinition
constDecl = define "ConstDecl" $ T.wrap $ nonemptyList $ go "ConstSpec"

-- ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
constSpec :: TypeDefinition
constSpec = define "ConstSpec" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: T.optional $ go "Type",
  "values">: T.list $ go "Expression"]

-- ContinueStmt = "continue" [ Label ] .
continueStmt :: TypeDefinition
continueStmt = define "ContinueStmt" $ T.wrap $ T.optional $ go "Identifier"

-- Conversion = Type "(" Expression [ "," ] ")" .
conversion :: TypeDefinition
conversion = define "Conversion" $ T.record [
  "type">: go "Type",
  "expression">: go "Expression"]

-- ============================================================================
-- Statements
-- ============================================================================

-- Declaration   = ConstDecl | TypeDecl | VarDecl .
declaration :: TypeDefinition
declaration = define "Declaration" $ T.union [
  "const">: go "ConstDecl",
  "type">: go "TypeDecl",
  "var">: go "VarDecl"]

-- DeferStmt = "defer" Expression .
deferStmt :: TypeDefinition
deferStmt = define "DeferStmt" $ T.wrap $ go "Expression"

-- Element       = Expression | LiteralValue .
element :: TypeDefinition
element = define "Element" $ T.union [
  "expression">: go "Expression",
  "literal">: go "LiteralValue"]

-- ElementList   = KeyedElement { "," KeyedElement } .
elementList :: TypeDefinition
elementList = define "ElementList" $ T.wrap $ nonemptyList $ go "KeyedElement"

-- Hydra: Else clause (either another if or a block)
elseClause :: TypeDefinition
elseClause = define "ElseClause" $ T.union [
  "if">: go "IfStmt",
  "block">: go "Block"]

-- Hydra: Embedded field (anonymous field)
embeddedField :: TypeDefinition
embeddedField = define "EmbeddedField" $ T.record [
  "pointer">: T.boolean,
  "type">: go "TypeName",
  "tag">: T.optional $ go "Tag"]

-- EmptyStmt = .
emptyStmt :: TypeDefinition
emptyStmt = define "EmptyStmt" $ T.wrap T.unit

-- ExprCaseClause = ExprSwitchCase ":" StatementList .
-- ExprSwitchCase = "case" ExpressionList | "default" .
exprCaseClause :: TypeDefinition
exprCaseClause = define "ExprCaseClause" $ T.record [
  "case">: T.optional $ nonemptyList $ go "Expression", -- Nothing for default
  "statements">: T.list $ go "Statement"]

-- ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
exprSwitchStmt :: TypeDefinition
exprSwitchStmt = define "ExprSwitchStmt" $ T.record [
  "init">: T.optional $ go "SimpleStmt",
  "expression">: T.optional $ go "Expression",
  "cases">: T.list $ go "ExprCaseClause"]

-- Expression = UnaryExpr | Expression binary_op Expression .
expression :: TypeDefinition
expression = define "Expression" $ T.union [
  "unary">: go "UnaryExpr",
  "binary">: go "BinaryExpr"]

-- ExpressionStmt = Expression .
expressionStmt :: TypeDefinition
expressionStmt = define "ExpressionStmt" $ T.wrap $ go "Expression"

-- FallthroughStmt = "fallthrough" .
fallthroughStmt :: TypeDefinition
fallthroughStmt = define "FallthroughStmt" $ T.wrap T.unit

-- FieldDecl     = (IdentifierList Type | EmbeddedField) [ Tag ] .
-- EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
fieldDecl :: TypeDefinition
fieldDecl = define "FieldDecl" $ T.union [
  "named">: go "NamedField",
  "embedded">: go "EmbeddedField"]

-- float_lit         = decimal_float_lit | hex_float_lit .
-- decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
--                     decimal_digits decimal_exponent |
--                     "." decimal_digits [ decimal_exponent ] .
-- hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
floatLit :: TypeDefinition
floatLit = define "FloatLit" $ T.wrap T.float64

-- ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
-- InitStmt = SimpleStmt .
-- PostStmt = SimpleStmt .
forClause :: TypeDefinition
forClause = define "ForClause" $ T.record [
  "init">: T.optional $ go "SimpleStmt",
  "condition">: T.optional $ go "Expression",
  "post">: T.optional $ go "SimpleStmt"]

-- Hydra: Either a for clause, range clause, or simple condition
forClauseOrRange :: TypeDefinition
forClauseOrRange = define "ForClauseOrRange" $ T.union [
  "condition">: go "Expression",
  "clause">: go "ForClause",
  "range">: go "RangeClause"]

-- ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
-- Condition = Expression .
forStmt :: TypeDefinition
forStmt = define "ForStmt" $ T.record [
  "clause">: T.optional $ go "ForClauseOrRange",
  "body">: go "Block"]

-- Hydra: Full slice [lo:hi:max]
fullSlice :: TypeDefinition
fullSlice = define "FullSlice" $ T.record [
  "low">: T.optional $ go "Expression",
  "high">: go "Expression",
  "max">: go "Expression"]

-- FunctionBody = Block .
functionBody :: TypeDefinition
functionBody = define "FunctionBody" $ T.wrap $ go "Block"

-- FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
-- FunctionName = identifier .
functionDecl :: TypeDefinition
functionDecl = define "FunctionDecl" $ T.record [
  "name">: go "Identifier",
  "typeParams">: T.optional $ go "TypeParameters",
  "signature">: go "Signature",
  "body">: T.optional $ go "FunctionBody"]

-- FunctionLit = "func" Signature FunctionBody .
functionLit :: TypeDefinition
functionLit = define "FunctionLit" $ T.record [
  "signature">: go "Signature",
  "body">: go "FunctionBody"]

-- FunctionType   = "func" Signature .
functionType :: TypeDefinition
functionType = define "FunctionType" $ T.wrap $ go "Signature"

go :: String -> Type
go = typeref ns

goModule :: TypeDefinition
goModule = define "Module" $ T.record [
  "package">: go "PackageClause",
  "imports">: T.list $ go "ImportDecl",
  "declarations">: T.list $ go "TopLevelDecl"]

-- ============================================================================
-- Terminals
-- ============================================================================

-- GoStmt = "go" Expression .
goStmt :: TypeDefinition
goStmt = define "GoStmt" $ T.wrap $ go "Expression"

-- GotoStmt = "goto" Label .
gotoStmt :: TypeDefinition
gotoStmt = define "GotoStmt" $ T.wrap $ go "Identifier"

-- identifier = letter { letter | unicode_digit } .
identifier :: TypeDefinition
identifier = define "Identifier" $ T.wrap T.string

-- IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
ifStmt :: TypeDefinition
ifStmt = define "IfStmt" $ T.record [
  "init">: T.optional $ go "SimpleStmt",
  "condition">: go "Expression",
  "then">: go "Block",
  "else">: T.optional $ go "ElseClause"]

-- imaginary_lit = (decimal_digits | int_lit | float_lit) "i" .
imaginaryLit :: TypeDefinition
imaginaryLit = define "ImaginaryLit" $ T.wrap T.float64

-- Hydra: ImportAlias represents the optional alias in an import
-- Either "." for dot import, or an identifier for named alias
importAlias :: TypeDefinition
importAlias = define "ImportAlias" $ T.union [
  "dot">: T.unit,
  "name">: go "Identifier"]

-- ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
importDecl :: TypeDefinition
importDecl = define "ImportDecl" $ T.wrap $ nonemptyList $ go "ImportSpec"

-- ImportPath       = string_lit .
importPath :: TypeDefinition
importPath = define "ImportPath" $ T.wrap $ go "StringLit"

-- ============================================================================
-- Declarations
-- ============================================================================

-- ImportSpec       = [ "." | PackageName ] ImportPath .
importSpec :: TypeDefinition
importSpec = define "ImportSpec" $ T.record [
  "alias">: T.optional $ go "ImportAlias",
  "path">: go "ImportPath"]

-- IncDecStmt = Expression ( "++" | "--" ) .
incDecStmt :: TypeDefinition
incDecStmt = define "IncDecStmt" $ T.record [
  "expression">: go "Expression",
  "increment">: T.boolean] -- true for ++, false for --

-- Index          = "[" Expression [ "," ] "]" .
index :: TypeDefinition
index = define "Index" $ T.wrap $ nonemptyList $ go "Expression"

-- Hydra: Index expression (x[i])
indexExpr :: TypeDefinition
indexExpr = define "IndexExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "index">: go "Expression"]

-- int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
-- decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
-- binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
-- octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
-- hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .
intLit :: TypeDefinition
intLit = define "IntLit" $ T.wrap T.bigint

-- InterfaceElem  = MethodElem | TypeElem .
interfaceElem :: TypeDefinition
interfaceElem = define "InterfaceElem" $ T.union [
  "method">: go "MethodElem",
  "type">: go "TypeElem"]

-- InterfaceType  = "interface" "{" { InterfaceElem ";" } "}" .
interfaceType :: TypeDefinition
interfaceType = define "InterfaceType" $ T.wrap $ T.list $ go "InterfaceElem"

-- interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
interpretedStringLit :: TypeDefinition
interpretedStringLit = define "InterpretedStringLit" $ T.wrap T.string

-- ============================================================================
-- Source file structure
-- ============================================================================

-- Key           = FieldName | Expression | LiteralValue .
-- FieldName     = identifier .
key :: TypeDefinition
key = define "Key" $ T.union [
  "field">: go "Identifier",
  "expression">: go "Expression",
  "literal">: go "LiteralValue"]

-- KeyedElement  = [ Key ":" ] Element .
keyedElement :: TypeDefinition
keyedElement = define "KeyedElement" $ T.record [
  "key">: T.optional $ go "Key",
  "element">: go "Element"]

-- LabeledStmt = Label ":" Statement .
-- Label       = identifier .
labeledStmt :: TypeDefinition
labeledStmt = define "LabeledStmt" $ T.record [
  "label">: go "Identifier",
  "statement">: go "Statement"]

-- Literal     = BasicLit | CompositeLit | FunctionLit .
literal :: TypeDefinition
literal = define "Literal" $ T.union [
  "basic">: go "BasicLit",
  "composite">: go "CompositeLit",
  "function">: go "FunctionLit"]

-- LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
--                 SliceType | MapType | TypeName [ TypeArgs ] .
literalType :: TypeDefinition
literalType = define "LiteralType" $ T.union [
  "struct">: go "StructType",
  "array">: go "ArrayType",
  "inferredArray">: go "Type", -- [...]T
  "slice">: go "SliceType",
  "map">: go "MapType",
  "name">: go "TypeName"]

-- LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
literalValue :: TypeDefinition
literalValue = define "LiteralValue" $ T.wrap $ T.list $ go "KeyedElement"

-- MapType     = "map" "[" KeyType "]" ElementType .
-- KeyType     = Type .
mapType :: TypeDefinition
mapType = define "MapType" $ T.record [
  "key">: go "Type",
  "value">: go "Type"]

-- MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
-- MethodName = identifier .
methodDecl :: TypeDefinition
methodDecl = define "MethodDecl" $ T.record [
  "receiver">: go "Receiver",
  "name">: go "Identifier",
  "signature">: go "Signature",
  "body">: T.optional $ go "FunctionBody"]

-- MethodElem     = MethodName Signature .
methodElem :: TypeDefinition
methodElem = define "MethodElem" $ T.record [
  "name">: go "Identifier",
  "signature">: go "Signature"]

-- MethodExpr    = ReceiverType "." MethodName .
-- ReceiverType  = Type .
methodExpr :: TypeDefinition
methodExpr = define "MethodExpr" $ T.record [
  "receiver">: go "Type",
  "method">: go "Identifier"]

-- mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
mulOp :: TypeDefinition
mulOp = define "MulOp" $ T.enum [
  "multiply",
  "divide",
  "remainder",
  "leftShift",
  "rightShift",
  "bitwiseAnd",
  "bitClear"]

-- Hydra: Named field with names, type, and optional tag
namedField :: TypeDefinition
namedField = define "NamedField" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: go "Type",
  "tag">: T.optional $ go "Tag"]

-- Operand     = Literal | OperandName [ TypeArgs ] | "(" Expression ")" .
-- OperandName = identifier | QualifiedIdent .
operand :: TypeDefinition
operand = define "Operand" $ T.union [
  "literal">: go "Literal",
  "name">: go "OperandName",
  "paren">: go "Expression"]

-- Hydra: Operand name with optional type arguments
operandName :: TypeDefinition
operandName = define "OperandName" $ T.record [
  "name">: go "QualifiedIdent",
  "typeArgs">: T.list $ go "Type"]

-- PackageClause  = "package" PackageName .
-- PackageName    = identifier .
packageClause :: TypeDefinition
packageClause = define "PackageClause" $ T.wrap $ go "Identifier"

-- ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
parameterDecl :: TypeDefinition
parameterDecl = define "ParameterDecl" $ T.record [
  "names">: T.list $ go "Identifier",
  "variadic">: T.boolean,
  "type">: go "Type"]

-- Parameters     = "(" [ ParameterList [ "," ] ] ")" .
-- ParameterList  = ParameterDecl { "," ParameterDecl } .
parameters :: TypeDefinition
parameters = define "Parameters" $ T.wrap $ T.list $ go "ParameterDecl"

-- PointerType = "*" BaseType .
-- BaseType    = Type .
pointerType :: TypeDefinition
pointerType = define "PointerType" $ T.wrap $ go "Type"

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
primaryExpr = define "PrimaryExpr" $ T.union [
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
qualifiedIdent = define "QualifiedIdent" $ T.record [
  "package">: T.optional $ go "Identifier",
  "name">: go "Identifier"]

-- RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
rangeClause :: TypeDefinition
rangeClause = define "RangeClause" $ T.record [
  "vars">: T.optional $ go "RangeVars",
  "expression">: go "Expression"]

-- Hydra: Range variables (either assignment or short declaration)
rangeVars :: TypeDefinition
rangeVars = define "RangeVars" $ T.union [
  "assign">: nonemptyList $ go "Expression",
  "declare">: nonemptyList $ go "Identifier"]

-- raw_string_lit         = "`" { unicode_char | newline } "`" .
rawStringLit :: TypeDefinition
rawStringLit = define "RawStringLit" $ T.wrap T.string

-- Hydra: Receive case with optional assignment
receiveCase :: TypeDefinition
receiveCase = define "ReceiveCase" $ T.record [
  "vars">: T.optional $ go "RangeVars", -- reuse RangeVars type
  "expression">: go "Expression"]

-- Receiver       = Parameters .
-- (Note: Go spec says the receiver must be a single non-variadic parameter)
receiver :: TypeDefinition
receiver = define "Receiver" $ T.record [
  "name">: T.optional $ go "Identifier",
  "type">: go "Type"]

-- ============================================================================
-- Types
-- ============================================================================

-- rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
relOp :: TypeDefinition
relOp = define "RelOp" $ T.enum [
  "equal",
  "notEqual",
  "less",
  "lessEqual",
  "greater",
  "greaterEqual"]

-- Result         = Parameters | Type .
result :: TypeDefinition
result = define "Result" $ T.union [
  "parameters">: go "Parameters",
  "type">: go "Type"]

-- ReturnStmt = "return" [ ExpressionList ] .
returnStmt :: TypeDefinition
returnStmt = define "ReturnStmt" $ T.wrap $ T.list $ go "Expression"

-- rune_lit         = "'" ( unicode_value | byte_value ) "'" .
runeLit :: TypeDefinition
runeLit = define "RuneLit" $ T.wrap T.int32 -- rune is int32

-- SelectStmt = "select" "{" { CommClause } "}" .
selectStmt :: TypeDefinition
selectStmt = define "SelectStmt" $ T.wrap $ T.list $ go "CommClause"

-- Selector       = "." identifier .
selector :: TypeDefinition
selector = define "Selector" $ T.wrap $ go "Identifier"

-- Hydra: Selector expression (x.y)
selectorExpr :: TypeDefinition
selectorExpr = define "SelectorExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "selector">: go "Identifier"]

-- SendStmt = Channel "<-" Expression .
-- Channel  = Expression .
sendStmt :: TypeDefinition
sendStmt = define "SendStmt" $ T.record [
  "channel">: go "Expression",
  "value">: go "Expression"]

-- ShortVarDecl = IdentifierList ":=" ExpressionList .
shortVarDecl :: TypeDefinition
shortVarDecl = define "ShortVarDecl" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "values">: nonemptyList $ go "Expression"]

-- Signature      = Parameters [ Result ] .
signature :: TypeDefinition
signature = define "Signature" $ T.record [
  "parameters">: go "Parameters",
  "result">: T.optional $ go "Result"]

-- Hydra: Simple slice [lo:hi]
simpleSlice :: TypeDefinition
simpleSlice = define "SimpleSlice" $ T.record [
  "low">: T.optional $ go "Expression",
  "high">: T.optional $ go "Expression"]

-- SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
simpleStmt :: TypeDefinition
simpleStmt = define "SimpleStmt" $ T.union [
  "empty">: go "EmptyStmt",
  "expression">: go "ExpressionStmt",
  "send">: go "SendStmt",
  "incDec">: go "IncDecStmt",
  "assignment">: go "Assignment",
  "shortVarDecl">: go "ShortVarDecl"]

-- Hydra: Slice expression (x[lo:hi] or x[lo:hi:max])
sliceExpr :: TypeDefinition
sliceExpr = define "SliceExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "slice">: go "Slice"]

-- SliceType = "[" "]" ElementType .
sliceType :: TypeDefinition
sliceType = define "SliceType" $ T.wrap $ go "Type"

-- Slice          = "[" [ Expression ] ":" [ Expression ] "]" |
--                  "[" [ Expression ] ":" Expression ":" Expression "]" .
slice_ :: TypeDefinition
slice_ = define "Slice" $ T.union [
  "simple">: go "SimpleSlice",
  "full">: go "FullSlice"]

-- SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
sourceFile_ :: TypeDefinition
sourceFile_ = define "SourceFile" $ T.record [
  "package">: go "PackageClause",
  "imports">: T.list $ go "ImportDecl",
  "declarations">: T.list $ go "TopLevelDecl"]

-- Statement =
--     Declaration | LabeledStmt | SimpleStmt |
--     GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
--     FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
--     DeferStmt .
statement :: TypeDefinition
statement = define "Statement" $ T.union [
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
stringLit = define "StringLit" $ T.union [
  "raw">: go "RawStringLit",
  "interpreted">: go "InterpretedStringLit"]

-- StructType    = "struct" "{" { FieldDecl ";" } "}" .
structType :: TypeDefinition
structType = define "StructType" $ T.wrap $ T.list $ go "FieldDecl"

-- SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
switchStmt :: TypeDefinition
switchStmt = define "SwitchStmt" $ T.union [
  "expression">: go "ExprSwitchStmt",
  "type">: go "TypeSwitchStmt"]

-- Tag = string_lit .
tag :: TypeDefinition
tag = define "Tag" $ T.wrap $ go "StringLit"

-- TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
topLevelDecl :: TypeDefinition
topLevelDecl = define "TopLevelDecl" $ T.union [
  "declaration">: go "Declaration",
  "function">: go "FunctionDecl",
  "method">: go "MethodDecl"]

-- TypeAssertion = "." "(" Type ")" .
typeAssertion :: TypeDefinition
typeAssertion = define "TypeAssertion" $ T.wrap $ go "Type"

-- Hydra: Type assertion expression (x.(T))
typeAssertionExpr :: TypeDefinition
typeAssertionExpr = define "TypeAssertionExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "type">: go "Type"]

-- TypeCaseClause  = TypeSwitchCase ":" StatementList .
-- TypeSwitchCase  = "case" TypeList | "default" .
-- TypeList        = Type { "," Type } .
typeCaseClause :: TypeDefinition
typeCaseClause = define "TypeCaseClause" $ T.record [
  "case">: T.optional $ nonemptyList $ go "Type", -- Nothing for default
  "statements">: T.list $ go "Statement"]

-- TypeConstraint = TypeElem .
typeConstraint :: TypeDefinition
typeConstraint = define "TypeConstraint" $ T.wrap $ go "TypeElem"

-- TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
typeDecl :: TypeDefinition
typeDecl = define "TypeDecl" $ T.wrap $ nonemptyList $ go "TypeSpec"

-- TypeDef = identifier [ TypeParameters ] Type .
typeDef :: TypeDefinition
typeDef = define "TypeDef" $ T.record [
  "name">: go "Identifier",
  "typeParams">: T.optional $ go "TypeParameters",
  "type">: go "Type"]

-- TypeElem       = TypeTerm { "|" TypeTerm } .
typeElem :: TypeDefinition
typeElem = define "TypeElem" $ T.wrap $ nonemptyList $ go "TypeTerm"

-- TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
--             SliceType | MapType | ChannelType .
typeLit :: TypeDefinition
typeLit = define "TypeLit" $ T.union [
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
typeName = define "TypeName" $ T.record [
  "name">: go "QualifiedIdent",
  "typeArgs">: T.list $ go "Type"]

-- TypeParamDecl = IdentifierList TypeConstraint .
typeParamDecl :: TypeDefinition
typeParamDecl = define "TypeParamDecl" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "constraint">: go "TypeConstraint"]

-- TypeParameters  = "[" TypeParamList [ "," ] "]" .
-- TypeParamList   = TypeParamDecl { "," TypeParamDecl } .
typeParameters :: TypeDefinition
typeParameters = define "TypeParameters" $ T.wrap $ nonemptyList $ go "TypeParamDecl"

-- TypeSpec = AliasDecl | TypeDef .
typeSpec :: TypeDefinition
typeSpec = define "TypeSpec" $ T.union [
  "alias">: go "AliasDecl",
  "definition">: go "TypeDef"]

-- TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
typeSwitchGuard :: TypeDefinition
typeSwitchGuard = define "TypeSwitchGuard" $ T.record [
  "name">: T.optional $ go "Identifier",
  "expression">: go "PrimaryExpr"]

-- TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
typeSwitchStmt :: TypeDefinition
typeSwitchStmt = define "TypeSwitchStmt" $ T.record [
  "init">: T.optional $ go "SimpleStmt",
  "guard">: go "TypeSwitchGuard",
  "cases">: T.list $ go "TypeCaseClause"]

-- TypeTerm       = Type | UnderlyingType .
-- UnderlyingType = "~" Type .
typeTerm :: TypeDefinition
typeTerm = define "TypeTerm" $ T.record [
  "underlying">: T.boolean, -- true if ~Type
  "type">: go "Type"]

-- Type      = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
type_ :: TypeDefinition
type_ = define "Type" $ T.union [
  "name">: go "TypeName",
  "literal">: go "TypeLit",
  "paren">: go "Type"]

-- UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
unaryExpr :: TypeDefinition
unaryExpr = define "UnaryExpr" $ T.union [
  "primary">: go "PrimaryExpr",
  "op">: go "UnaryOperation"]

-- unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
unaryOp :: TypeDefinition
unaryOp = define "UnaryOp" $ T.enum [
  "plus",       -- +
  "minus",      -- -
  "not",        -- !
  "xor",        -- ^
  "deref",      -- *
  "addressOf",  -- &
  "receive"]    -- <-

-- Hydra: Unary operation with operator and operand
unaryOperation :: TypeDefinition
unaryOperation = define "UnaryOperation" $ T.record [
  "op">: go "UnaryOp",
  "operand">: go "UnaryExpr"]

-- VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
varDecl :: TypeDefinition
varDecl = define "VarDecl" $ T.wrap $ nonemptyList $ go "VarSpec"

-- VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
varSpec :: TypeDefinition
varSpec = define "VarSpec" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: T.optional $ go "Type",
  "values">: T.list $ go "Expression"]
