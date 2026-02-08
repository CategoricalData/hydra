module Hydra.Ext.Sources.Go.Syntax where

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


ns :: Namespace
ns = Namespace "hydra.ext.go.syntax"

define :: String -> Type -> Binding
define = datatype ns

go :: String -> Type
go = typeref ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just ("A Go syntax model, based on the Go Language Specification retrieved on 2025-02-05"
      ++ " from https://go.dev/ref/spec")
  where
    elements = constructs ++ terminals ++ sourceFile ++ declarations ++ types
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

annotatedDeclaration :: Binding
annotatedDeclaration = define "AnnotatedDeclaration" $ T.record [
  "comment">: T.string,
  "declaration">: go "TopLevelDecl"]

goModule :: Binding
goModule = define "Module" $ T.record [
  "package">: go "PackageClause",
  "imports">: T.list $ go "ImportDecl",
  "declarations">: T.list $ go "TopLevelDecl"]

-- ============================================================================
-- Terminals
-- ============================================================================

-- identifier = letter { letter | unicode_digit } .
identifier :: Binding
identifier = define "Identifier" $ T.wrap T.string

-- int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
-- decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
-- binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
-- octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
-- hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .
intLit :: Binding
intLit = define "IntLit" $ T.wrap T.bigint

-- float_lit         = decimal_float_lit | hex_float_lit .
-- decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
--                     decimal_digits decimal_exponent |
--                     "." decimal_digits [ decimal_exponent ] .
-- hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
floatLit :: Binding
floatLit = define "FloatLit" $ T.wrap T.float64

-- imaginary_lit = (decimal_digits | int_lit | float_lit) "i" .
imaginaryLit :: Binding
imaginaryLit = define "ImaginaryLit" $ T.wrap T.float64

-- rune_lit         = "'" ( unicode_value | byte_value ) "'" .
runeLit :: Binding
runeLit = define "RuneLit" $ T.wrap T.int32 -- rune is int32

-- string_lit             = raw_string_lit | interpreted_string_lit .
stringLit :: Binding
stringLit = define "StringLit" $ T.union [
  "raw">: go "RawStringLit",
  "interpreted">: go "InterpretedStringLit"]

-- raw_string_lit         = "`" { unicode_char | newline } "`" .
rawStringLit :: Binding
rawStringLit = define "RawStringLit" $ T.wrap T.string

-- interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
interpretedStringLit :: Binding
interpretedStringLit = define "InterpretedStringLit" $ T.wrap T.string

-- ============================================================================
-- Source file structure
-- ============================================================================

-- SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
sourceFile_ :: Binding
sourceFile_ = define "SourceFile" $ T.record [
  "package">: go "PackageClause",
  "imports">: T.list $ go "ImportDecl",
  "declarations">: T.list $ go "TopLevelDecl"]

-- PackageClause  = "package" PackageName .
-- PackageName    = identifier .
packageClause :: Binding
packageClause = define "PackageClause" $ T.wrap $ go "Identifier"

-- ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
importDecl :: Binding
importDecl = define "ImportDecl" $ T.wrap $ nonemptyList $ go "ImportSpec"

-- ImportSpec       = [ "." | PackageName ] ImportPath .
importSpec :: Binding
importSpec = define "ImportSpec" $ T.record [
  "alias">: T.maybe $ go "ImportAlias",
  "path">: go "ImportPath"]

-- Hydra: ImportAlias represents the optional alias in an import
-- Either "." for dot import, or an identifier for named alias
importAlias :: Binding
importAlias = define "ImportAlias" $ T.union [
  "dot">: T.unit,
  "name">: go "Identifier"]

-- ImportPath       = string_lit .
importPath :: Binding
importPath = define "ImportPath" $ T.wrap $ go "StringLit"

-- ============================================================================
-- Declarations
-- ============================================================================

-- Declaration   = ConstDecl | TypeDecl | VarDecl .
declaration :: Binding
declaration = define "Declaration" $ T.union [
  "const">: go "ConstDecl",
  "type">: go "TypeDecl",
  "var">: go "VarDecl"]

-- TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
topLevelDecl :: Binding
topLevelDecl = define "TopLevelDecl" $ T.union [
  "declaration">: go "Declaration",
  "function">: go "FunctionDecl",
  "method">: go "MethodDecl"]

-- ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
constDecl :: Binding
constDecl = define "ConstDecl" $ T.wrap $ nonemptyList $ go "ConstSpec"

-- ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
constSpec :: Binding
constSpec = define "ConstSpec" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: T.maybe $ go "Type",
  "values">: T.list $ go "Expression"]

-- VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
varDecl :: Binding
varDecl = define "VarDecl" $ T.wrap $ nonemptyList $ go "VarSpec"

-- VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
varSpec :: Binding
varSpec = define "VarSpec" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: T.maybe $ go "Type",
  "values">: T.list $ go "Expression"]

-- ShortVarDecl = IdentifierList ":=" ExpressionList .
shortVarDecl :: Binding
shortVarDecl = define "ShortVarDecl" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "values">: nonemptyList $ go "Expression"]

-- TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
typeDecl :: Binding
typeDecl = define "TypeDecl" $ T.wrap $ nonemptyList $ go "TypeSpec"

-- TypeSpec = AliasDecl | TypeDef .
typeSpec :: Binding
typeSpec = define "TypeSpec" $ T.union [
  "alias">: go "AliasDecl",
  "definition">: go "TypeDef"]

-- AliasDecl = identifier "=" Type .
aliasDecl :: Binding
aliasDecl = define "AliasDecl" $ T.record [
  "name">: go "Identifier",
  "type">: go "Type"]

-- TypeDef = identifier [ TypeParameters ] Type .
typeDef :: Binding
typeDef = define "TypeDef" $ T.record [
  "name">: go "Identifier",
  "typeParams">: T.maybe $ go "TypeParameters",
  "type">: go "Type"]

-- TypeParameters  = "[" TypeParamList [ "," ] "]" .
-- TypeParamList   = TypeParamDecl { "," TypeParamDecl } .
typeParameters :: Binding
typeParameters = define "TypeParameters" $ T.wrap $ nonemptyList $ go "TypeParamDecl"

-- TypeParamDecl = IdentifierList TypeConstraint .
typeParamDecl :: Binding
typeParamDecl = define "TypeParamDecl" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "constraint">: go "TypeConstraint"]

-- TypeConstraint = TypeElem .
typeConstraint :: Binding
typeConstraint = define "TypeConstraint" $ T.wrap $ go "TypeElem"

-- FunctionDecl = "func" FunctionName [ TypeParameters ] Signature [ FunctionBody ] .
-- FunctionName = identifier .
functionDecl :: Binding
functionDecl = define "FunctionDecl" $ T.record [
  "name">: go "Identifier",
  "typeParams">: T.maybe $ go "TypeParameters",
  "signature">: go "Signature",
  "body">: T.maybe $ go "FunctionBody"]

-- FunctionBody = Block .
functionBody :: Binding
functionBody = define "FunctionBody" $ T.wrap $ go "Block"

-- MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
-- MethodName = identifier .
methodDecl :: Binding
methodDecl = define "MethodDecl" $ T.record [
  "receiver">: go "Receiver",
  "name">: go "Identifier",
  "signature">: go "Signature",
  "body">: T.maybe $ go "FunctionBody"]

-- Receiver       = Parameters .
-- (Note: Go spec says the receiver must be a single non-variadic parameter)
receiver :: Binding
receiver = define "Receiver" $ T.record [
  "name">: T.maybe $ go "Identifier",
  "type">: go "Type"]

-- ============================================================================
-- Types
-- ============================================================================

-- Type      = TypeName [ TypeArgs ] | TypeLit | "(" Type ")" .
type_ :: Binding
type_ = define "Type" $ T.union [
  "name">: go "TypeName",
  "literal">: go "TypeLit",
  "paren">: go "Type"]

-- TypeName  = identifier | QualifiedIdent .
typeName :: Binding
typeName = define "TypeName" $ T.record [
  "name">: go "QualifiedIdent",
  "typeArgs">: T.list $ go "Type"]

-- QualifiedIdent = PackageName "." identifier .
qualifiedIdent :: Binding
qualifiedIdent = define "QualifiedIdent" $ T.record [
  "package">: T.maybe $ go "Identifier",
  "name">: go "Identifier"]

-- TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
--             SliceType | MapType | ChannelType .
typeLit :: Binding
typeLit = define "TypeLit" $ T.union [
  "array">: go "ArrayType",
  "struct">: go "StructType",
  "pointer">: go "PointerType",
  "function">: go "FunctionType",
  "interface">: go "InterfaceType",
  "slice">: go "SliceType",
  "map">: go "MapType",
  "channel">: go "ChannelType"]

-- ArrayType   = "[" ArrayLength "]" ElementType .
-- ArrayLength = Expression .
-- ElementType = Type .
arrayType :: Binding
arrayType = define "ArrayType" $ T.record [
  "length">: go "Expression",
  "element">: go "Type"]

-- SliceType = "[" "]" ElementType .
sliceType :: Binding
sliceType = define "SliceType" $ T.wrap $ go "Type"

-- StructType    = "struct" "{" { FieldDecl ";" } "}" .
structType :: Binding
structType = define "StructType" $ T.wrap $ T.list $ go "FieldDecl"

-- FieldDecl     = (IdentifierList Type | EmbeddedField) [ Tag ] .
-- EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
fieldDecl :: Binding
fieldDecl = define "FieldDecl" $ T.union [
  "named">: go "NamedField",
  "embedded">: go "EmbeddedField"]

-- Hydra: Named field with names, type, and optional tag
namedField :: Binding
namedField = define "NamedField" $ T.record [
  "names">: nonemptyList $ go "Identifier",
  "type">: go "Type",
  "tag">: T.maybe $ go "Tag"]

-- Hydra: Embedded field (anonymous field)
embeddedField :: Binding
embeddedField = define "EmbeddedField" $ T.record [
  "pointer">: T.boolean,
  "type">: go "TypeName",
  "tag">: T.maybe $ go "Tag"]

-- Tag = string_lit .
tag :: Binding
tag = define "Tag" $ T.wrap $ go "StringLit"

-- PointerType = "*" BaseType .
-- BaseType    = Type .
pointerType :: Binding
pointerType = define "PointerType" $ T.wrap $ go "Type"

-- FunctionType   = "func" Signature .
functionType :: Binding
functionType = define "FunctionType" $ T.wrap $ go "Signature"

-- Signature      = Parameters [ Result ] .
signature :: Binding
signature = define "Signature" $ T.record [
  "parameters">: go "Parameters",
  "result">: T.maybe $ go "Result"]

-- Result         = Parameters | Type .
result :: Binding
result = define "Result" $ T.union [
  "parameters">: go "Parameters",
  "type">: go "Type"]

-- Parameters     = "(" [ ParameterList [ "," ] ] ")" .
-- ParameterList  = ParameterDecl { "," ParameterDecl } .
parameters :: Binding
parameters = define "Parameters" $ T.wrap $ T.list $ go "ParameterDecl"

-- ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
parameterDecl :: Binding
parameterDecl = define "ParameterDecl" $ T.record [
  "names">: T.list $ go "Identifier",
  "variadic">: T.boolean,
  "type">: go "Type"]

-- InterfaceType  = "interface" "{" { InterfaceElem ";" } "}" .
interfaceType :: Binding
interfaceType = define "InterfaceType" $ T.wrap $ T.list $ go "InterfaceElem"

-- InterfaceElem  = MethodElem | TypeElem .
interfaceElem :: Binding
interfaceElem = define "InterfaceElem" $ T.union [
  "method">: go "MethodElem",
  "type">: go "TypeElem"]

-- MethodElem     = MethodName Signature .
methodElem :: Binding
methodElem = define "MethodElem" $ T.record [
  "name">: go "Identifier",
  "signature">: go "Signature"]

-- TypeElem       = TypeTerm { "|" TypeTerm } .
typeElem :: Binding
typeElem = define "TypeElem" $ T.wrap $ nonemptyList $ go "TypeTerm"

-- TypeTerm       = Type | UnderlyingType .
-- UnderlyingType = "~" Type .
typeTerm :: Binding
typeTerm = define "TypeTerm" $ T.record [
  "underlying">: T.boolean, -- true if ~Type
  "type">: go "Type"]

-- MapType     = "map" "[" KeyType "]" ElementType .
-- KeyType     = Type .
mapType :: Binding
mapType = define "MapType" $ T.record [
  "key">: go "Type",
  "value">: go "Type"]

-- ChannelType = ( "chan" | "chan" "<-" | "<-" "chan" ) ElementType .
channelType :: Binding
channelType = define "ChannelType" $ T.record [
  "direction">: go "ChannelDirection",
  "element">: go "Type"]

channelDirection :: Binding
channelDirection = define "ChannelDirection" $ T.enum [
  "bidirectional", -- chan
  "send",          -- chan<-
  "receive"]       -- <-chan

-- ============================================================================
-- Expressions
-- ============================================================================

-- Expression = UnaryExpr | Expression binary_op Expression .
expression :: Binding
expression = define "Expression" $ T.union [
  "unary">: go "UnaryExpr",
  "binary">: go "BinaryExpr"]

-- UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
unaryExpr :: Binding
unaryExpr = define "UnaryExpr" $ T.union [
  "primary">: go "PrimaryExpr",
  "op">: go "UnaryOperation"]

-- Hydra: Unary operation with operator and operand
unaryOperation :: Binding
unaryOperation = define "UnaryOperation" $ T.record [
  "op">: go "UnaryOp",
  "operand">: go "UnaryExpr"]

-- binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
binaryExpr :: Binding
binaryExpr = define "BinaryExpr" $ T.record [
  "left">: go "Expression",
  "op">: go "BinaryOp",
  "right">: go "Expression"]

binaryOp :: Binding
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

-- PrimaryExpr =
--     Operand |
--     Conversion |
--     MethodExpr |
--     PrimaryExpr Selector |
--     PrimaryExpr Index |
--     PrimaryExpr Slice |
--     PrimaryExpr TypeAssertion |
--     PrimaryExpr Arguments .
primaryExpr :: Binding
primaryExpr = define "PrimaryExpr" $ T.union [
  "operand">: go "Operand",
  "conversion">: go "Conversion",
  "methodExpr">: go "MethodExpr",
  "selector">: go "SelectorExpr",
  "index">: go "IndexExpr",
  "slice">: go "SliceExpr",
  "typeAssertion">: go "TypeAssertionExpr",
  "call">: go "CallExpr"]

-- Hydra: Selector expression (x.y)
selectorExpr :: Binding
selectorExpr = define "SelectorExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "selector">: go "Identifier"]

-- Hydra: Index expression (x[i])
indexExpr :: Binding
indexExpr = define "IndexExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "index">: go "Expression"]

-- Hydra: Slice expression (x[lo:hi] or x[lo:hi:max])
sliceExpr :: Binding
sliceExpr = define "SliceExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "slice">: go "Slice"]

-- Hydra: Type assertion expression (x.(T))
typeAssertionExpr :: Binding
typeAssertionExpr = define "TypeAssertionExpr" $ T.record [
  "expr">: go "PrimaryExpr",
  "type">: go "Type"]

-- Hydra: Call expression (f(args))
callExpr :: Binding
callExpr = define "CallExpr" $ T.record [
  "function">: go "PrimaryExpr",
  "arguments">: go "Arguments"]

-- Operand     = Literal | OperandName [ TypeArgs ] | "(" Expression ")" .
-- OperandName = identifier | QualifiedIdent .
operand :: Binding
operand = define "Operand" $ T.union [
  "literal">: go "Literal",
  "name">: go "OperandName",
  "paren">: go "Expression"]

-- Hydra: Operand name with optional type arguments
operandName :: Binding
operandName = define "OperandName" $ T.record [
  "name">: go "QualifiedIdent",
  "typeArgs">: T.list $ go "Type"]

-- Literal     = BasicLit | CompositeLit | FunctionLit .
literal :: Binding
literal = define "Literal" $ T.union [
  "basic">: go "BasicLit",
  "composite">: go "CompositeLit",
  "function">: go "FunctionLit"]

-- BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
basicLit :: Binding
basicLit = define "BasicLit" $ T.union [
  "int">: go "IntLit",
  "float">: go "FloatLit",
  "imaginary">: go "ImaginaryLit",
  "rune">: go "RuneLit",
  "string">: go "StringLit"]

-- CompositeLit  = LiteralType LiteralValue .
compositeLit :: Binding
compositeLit = define "CompositeLit" $ T.record [
  "type">: go "LiteralType",
  "value">: go "LiteralValue"]

-- LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
--                 SliceType | MapType | TypeName [ TypeArgs ] .
literalType :: Binding
literalType = define "LiteralType" $ T.union [
  "struct">: go "StructType",
  "array">: go "ArrayType",
  "inferredArray">: go "Type", -- [...]T
  "slice">: go "SliceType",
  "map">: go "MapType",
  "name">: go "TypeName"]

-- LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
literalValue :: Binding
literalValue = define "LiteralValue" $ T.wrap $ T.list $ go "KeyedElement"

-- ElementList   = KeyedElement { "," KeyedElement } .
elementList :: Binding
elementList = define "ElementList" $ T.wrap $ nonemptyList $ go "KeyedElement"

-- KeyedElement  = [ Key ":" ] Element .
keyedElement :: Binding
keyedElement = define "KeyedElement" $ T.record [
  "key">: T.maybe $ go "Key",
  "element">: go "Element"]

-- Key           = FieldName | Expression | LiteralValue .
-- FieldName     = identifier .
key :: Binding
key = define "Key" $ T.union [
  "field">: go "Identifier",
  "expression">: go "Expression",
  "literal">: go "LiteralValue"]

-- Element       = Expression | LiteralValue .
element :: Binding
element = define "Element" $ T.union [
  "expression">: go "Expression",
  "literal">: go "LiteralValue"]

-- FunctionLit = "func" Signature FunctionBody .
functionLit :: Binding
functionLit = define "FunctionLit" $ T.record [
  "signature">: go "Signature",
  "body">: go "FunctionBody"]

-- Selector       = "." identifier .
selector :: Binding
selector = define "Selector" $ T.wrap $ go "Identifier"

-- Index          = "[" Expression [ "," ] "]" .
index :: Binding
index = define "Index" $ T.wrap $ nonemptyList $ go "Expression"

-- Slice          = "[" [ Expression ] ":" [ Expression ] "]" |
--                  "[" [ Expression ] ":" Expression ":" Expression "]" .
slice_ :: Binding
slice_ = define "Slice" $ T.union [
  "simple">: go "SimpleSlice",
  "full">: go "FullSlice"]

-- Hydra: Simple slice [lo:hi]
simpleSlice :: Binding
simpleSlice = define "SimpleSlice" $ T.record [
  "low">: T.maybe $ go "Expression",
  "high">: T.maybe $ go "Expression"]

-- Hydra: Full slice [lo:hi:max]
fullSlice :: Binding
fullSlice = define "FullSlice" $ T.record [
  "low">: T.maybe $ go "Expression",
  "high">: go "Expression",
  "max">: go "Expression"]

-- TypeAssertion = "." "(" Type ")" .
typeAssertion :: Binding
typeAssertion = define "TypeAssertion" $ T.wrap $ go "Type"

-- Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
arguments :: Binding
arguments = define "Arguments" $ T.record [
  "typeArg">: T.maybe $ go "Type",
  "expressions">: T.list $ go "Expression",
  "ellipsis">: T.boolean]

-- MethodExpr    = ReceiverType "." MethodName .
-- ReceiverType  = Type .
methodExpr :: Binding
methodExpr = define "MethodExpr" $ T.record [
  "receiver">: go "Type",
  "method">: go "Identifier"]

-- Conversion = Type "(" Expression [ "," ] ")" .
conversion :: Binding
conversion = define "Conversion" $ T.record [
  "type">: go "Type",
  "expression">: go "Expression"]

-- ============================================================================
-- Statements
-- ============================================================================

-- Statement =
--     Declaration | LabeledStmt | SimpleStmt |
--     GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
--     FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
--     DeferStmt .
statement :: Binding
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

-- SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
simpleStmt :: Binding
simpleStmt = define "SimpleStmt" $ T.union [
  "empty">: go "EmptyStmt",
  "expression">: go "ExpressionStmt",
  "send">: go "SendStmt",
  "incDec">: go "IncDecStmt",
  "assignment">: go "Assignment",
  "shortVarDecl">: go "ShortVarDecl"]

-- EmptyStmt = .
emptyStmt :: Binding
emptyStmt = define "EmptyStmt" $ T.wrap T.unit

-- LabeledStmt = Label ":" Statement .
-- Label       = identifier .
labeledStmt :: Binding
labeledStmt = define "LabeledStmt" $ T.record [
  "label">: go "Identifier",
  "statement">: go "Statement"]

-- ExpressionStmt = Expression .
expressionStmt :: Binding
expressionStmt = define "ExpressionStmt" $ T.wrap $ go "Expression"

-- SendStmt = Channel "<-" Expression .
-- Channel  = Expression .
sendStmt :: Binding
sendStmt = define "SendStmt" $ T.record [
  "channel">: go "Expression",
  "value">: go "Expression"]

-- IncDecStmt = Expression ( "++" | "--" ) .
incDecStmt :: Binding
incDecStmt = define "IncDecStmt" $ T.record [
  "expression">: go "Expression",
  "increment">: T.boolean] -- true for ++, false for --

-- Assignment = ExpressionList assign_op ExpressionList .
assignment :: Binding
assignment = define "Assignment" $ T.record [
  "lhs">: nonemptyList $ go "Expression",
  "op">: go "AssignOp",
  "rhs">: nonemptyList $ go "Expression"]

-- assign_op = [ add_op | mul_op ] "=" .
assignOp :: Binding
assignOp = define "AssignOp" $ T.union [
  "simple">: T.unit,     -- =
  "add">: go "AddOp",    -- +=, -=, |=, ^=
  "mul">: go "MulOp"]    -- *=, /=, %=, <<=, >>=, &=, &^=

-- IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
ifStmt :: Binding
ifStmt = define "IfStmt" $ T.record [
  "init">: T.maybe $ go "SimpleStmt",
  "condition">: go "Expression",
  "then">: go "Block",
  "else">: T.maybe $ go "ElseClause"]

-- Hydra: Else clause (either another if or a block)
elseClause :: Binding
elseClause = define "ElseClause" $ T.union [
  "if">: go "IfStmt",
  "block">: go "Block"]

-- SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
switchStmt :: Binding
switchStmt = define "SwitchStmt" $ T.union [
  "expression">: go "ExprSwitchStmt",
  "type">: go "TypeSwitchStmt"]

-- ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
exprSwitchStmt :: Binding
exprSwitchStmt = define "ExprSwitchStmt" $ T.record [
  "init">: T.maybe $ go "SimpleStmt",
  "expression">: T.maybe $ go "Expression",
  "cases">: T.list $ go "ExprCaseClause"]

-- ExprCaseClause = ExprSwitchCase ":" StatementList .
-- ExprSwitchCase = "case" ExpressionList | "default" .
exprCaseClause :: Binding
exprCaseClause = define "ExprCaseClause" $ T.record [
  "case">: T.maybe $ nonemptyList $ go "Expression", -- Nothing for default
  "statements">: T.list $ go "Statement"]

-- TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
typeSwitchStmt :: Binding
typeSwitchStmt = define "TypeSwitchStmt" $ T.record [
  "init">: T.maybe $ go "SimpleStmt",
  "guard">: go "TypeSwitchGuard",
  "cases">: T.list $ go "TypeCaseClause"]

-- TypeSwitchGuard = [ identifier ":=" ] PrimaryExpr "." "(" "type" ")" .
typeSwitchGuard :: Binding
typeSwitchGuard = define "TypeSwitchGuard" $ T.record [
  "name">: T.maybe $ go "Identifier",
  "expression">: go "PrimaryExpr"]

-- TypeCaseClause  = TypeSwitchCase ":" StatementList .
-- TypeSwitchCase  = "case" TypeList | "default" .
-- TypeList        = Type { "," Type } .
typeCaseClause :: Binding
typeCaseClause = define "TypeCaseClause" $ T.record [
  "case">: T.maybe $ nonemptyList $ go "Type", -- Nothing for default
  "statements">: T.list $ go "Statement"]

-- ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
-- Condition = Expression .
forStmt :: Binding
forStmt = define "ForStmt" $ T.record [
  "clause">: T.maybe $ go "ForClauseOrRange",
  "body">: go "Block"]

-- Hydra: Either a for clause, range clause, or simple condition
forClauseOrRange :: Binding
forClauseOrRange = define "ForClauseOrRange" $ T.union [
  "condition">: go "Expression",
  "clause">: go "ForClause",
  "range">: go "RangeClause"]

-- ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
-- InitStmt = SimpleStmt .
-- PostStmt = SimpleStmt .
forClause :: Binding
forClause = define "ForClause" $ T.record [
  "init">: T.maybe $ go "SimpleStmt",
  "condition">: T.maybe $ go "Expression",
  "post">: T.maybe $ go "SimpleStmt"]

-- RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression .
rangeClause :: Binding
rangeClause = define "RangeClause" $ T.record [
  "vars">: T.maybe $ go "RangeVars",
  "expression">: go "Expression"]

-- Hydra: Range variables (either assignment or short declaration)
rangeVars :: Binding
rangeVars = define "RangeVars" $ T.union [
  "assign">: nonemptyList $ go "Expression",
  "declare">: nonemptyList $ go "Identifier"]

-- GoStmt = "go" Expression .
goStmt :: Binding
goStmt = define "GoStmt" $ T.wrap $ go "Expression"

-- SelectStmt = "select" "{" { CommClause } "}" .
selectStmt :: Binding
selectStmt = define "SelectStmt" $ T.wrap $ T.list $ go "CommClause"

-- CommClause = CommCase ":" StatementList .
commClause :: Binding
commClause = define "CommClause" $ T.record [
  "case">: go "CommCase",
  "statements">: T.list $ go "Statement"]

-- CommCase   = "case" ( SendStmt | RecvStmt ) | "default" .
-- RecvStmt   = [ ExpressionList "=" | IdentifierList ":=" ] RecvExpr .
-- RecvExpr   = Expression .
commCase :: Binding
commCase = define "CommCase" $ T.union [
  "send">: go "SendStmt",
  "receive">: go "ReceiveCase",
  "default">: T.unit]

-- Hydra: Receive case with optional assignment
receiveCase :: Binding
receiveCase = define "ReceiveCase" $ T.record [
  "vars">: T.maybe $ go "RangeVars", -- reuse RangeVars type
  "expression">: go "Expression"]

-- ReturnStmt = "return" [ ExpressionList ] .
returnStmt :: Binding
returnStmt = define "ReturnStmt" $ T.wrap $ T.list $ go "Expression"

-- BreakStmt = "break" [ Label ] .
breakStmt :: Binding
breakStmt = define "BreakStmt" $ T.wrap $ T.maybe $ go "Identifier"

-- ContinueStmt = "continue" [ Label ] .
continueStmt :: Binding
continueStmt = define "ContinueStmt" $ T.wrap $ T.maybe $ go "Identifier"

-- GotoStmt = "goto" Label .
gotoStmt :: Binding
gotoStmt = define "GotoStmt" $ T.wrap $ go "Identifier"

-- FallthroughStmt = "fallthrough" .
fallthroughStmt :: Binding
fallthroughStmt = define "FallthroughStmt" $ T.wrap T.unit

-- DeferStmt = "defer" Expression .
deferStmt :: Binding
deferStmt = define "DeferStmt" $ T.wrap $ go "Expression"

-- Block = "{" StatementList "}" .
-- StatementList = { Statement ";" } .
block :: Binding
block = define "Block" $ T.wrap $ T.list $ go "Statement"

-- ============================================================================
-- Operators (miscellaneous)
-- ============================================================================

-- unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
unaryOp :: Binding
unaryOp = define "UnaryOp" $ T.enum [
  "plus",       -- +
  "minus",      -- -
  "not",        -- !
  "xor",        -- ^
  "deref",      -- *
  "addressOf",  -- &
  "receive"]    -- <-

-- mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
mulOp :: Binding
mulOp = define "MulOp" $ T.enum [
  "multiply",
  "divide",
  "remainder",
  "leftShift",
  "rightShift",
  "bitwiseAnd",
  "bitClear"]

-- add_op     = "+" | "-" | "|" | "^" .
addOp :: Binding
addOp = define "AddOp" $ T.enum [
  "add",
  "subtract",
  "bitwiseOr",
  "bitwiseXor"]

-- rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
relOp :: Binding
relOp = define "RelOp" $ T.enum [
  "equal",
  "notEqual",
  "less",
  "lessEqual",
  "greater",
  "greaterEqual"]
