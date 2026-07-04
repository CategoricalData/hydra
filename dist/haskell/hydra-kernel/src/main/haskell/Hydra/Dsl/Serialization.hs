-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.serialization

module Hydra.Dsl.Serialization where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Error.System as DslErrorSystem
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Serialization as Serialization
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL reference to hydra.serialization.angleBraces
angleBraces :: Typed.TypedTerm Ast.Brackets
angleBraces = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.angleBraces"))

-- | DSL reference to hydra.serialization.angleBracesList
angleBracesList :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
angleBracesList arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.angleBracesList")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.bracesListAdaptive
bracesListAdaptive :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
bracesListAdaptive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.bracesListAdaptive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.bracketList
bracketList :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
bracketList arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.bracketList")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.bracketListAdaptive
bracketListAdaptive :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
bracketListAdaptive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.bracketListAdaptive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.brackets
brackets :: Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
brackets arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.brackets")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.chooseLayout
chooseLayout :: Typed.TypedTerm Int -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
chooseLayout arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.chooseLayout")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.commaSep
commaSep :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
commaSep arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.commaSep")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.commaSepAdaptive
commaSepAdaptive :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
commaSepAdaptive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.commaSepAdaptive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.cst
cst :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr
cst arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.cst")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.curlyBlock
curlyBlock :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
curlyBlock arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.curlyBlock")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.curlyBraces
curlyBraces :: Typed.TypedTerm Ast.Brackets
curlyBraces = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.curlyBraces"))

-- | DSL reference to hydra.serialization.curlyBracesList
curlyBracesList :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
curlyBracesList arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.curlyBracesList")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.customIndent
customIndent :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm String
customIndent arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.customIndent")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.customIndentBlock
customIndentBlock :: Typed.TypedTerm String -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
customIndentBlock arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.customIndentBlock")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.dotSep
dotSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
dotSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.dotSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.doubleNewlineSep
doubleNewlineSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
doubleNewlineSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.doubleNewlineSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.doubleSpace
doubleSpace :: Typed.TypedTerm String
doubleSpace = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.doubleSpace"))

-- | DSL reference to hydra.serialization.expressionLength
expressionLength :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Int
expressionLength arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.expressionLength")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.fullBlockStyle
fullBlockStyle :: Typed.TypedTerm Ast.BlockStyle
fullBlockStyle = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.fullBlockStyle"))

-- | DSL reference to hydra.serialization.halfBlockStyle
halfBlockStyle :: Typed.TypedTerm Ast.BlockStyle
halfBlockStyle = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.halfBlockStyle"))

-- | DSL reference to hydra.serialization.ifx
ifx :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
ifx arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.ifx")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.indent
indent :: Typed.TypedTerm String -> Typed.TypedTerm String
indent arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.indent")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.indentBlock
indentBlock :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
indentBlock arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.indentBlock")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.indentSubsequentLines
indentSubsequentLines :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
indentSubsequentLines arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.indentSubsequentLines")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.infixWs
infixWs :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
infixWs arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.infixWs")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.infixWsList
infixWsList :: Typed.TypedTerm String -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
infixWsList arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.infixWsList")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.inlineStyle
inlineStyle :: Typed.TypedTerm Ast.BlockStyle
inlineStyle = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.inlineStyle"))

-- | DSL reference to hydra.serialization.maxLineWidth
maxLineWidth :: Typed.TypedTerm Int
maxLineWidth = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.maxLineWidth"))

-- | DSL reference to hydra.serialization.newlineSep
newlineSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
newlineSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.newlineSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.noPadding
noPadding :: Typed.TypedTerm Ast.Padding
noPadding = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.noPadding"))

-- | DSL reference to hydra.serialization.noSep
noSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
noSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.noSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.num
num :: Typed.TypedTerm Int -> Typed.TypedTerm Ast.Expr
num arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.num")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.op
op :: Typed.TypedTerm String -> Typed.TypedTerm Int -> Typed.TypedTerm Ast.Associativity -> Typed.TypedTerm Ast.Op
op arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.op")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.orOp
orOp :: Typed.TypedTerm Bool -> Typed.TypedTerm Ast.Op
orOp arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.orOp")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.orSep
orSep :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
orSep arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.orSep")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.parenList
parenList :: Typed.TypedTerm Bool -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
parenList arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.parenList")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.parenListAdaptive
parenListAdaptive :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
parenListAdaptive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.parenListAdaptive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.parens
parens :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
parens arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.parens")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.parentheses
parentheses :: Typed.TypedTerm Ast.Brackets
parentheses = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.parentheses"))

-- | DSL reference to hydra.serialization.parenthesize
parenthesize :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
parenthesize arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.parenthesize")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.prefix
prefix :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
prefix arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.prefix")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.printExpr
printExpr :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm String
printExpr arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.printExpr")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.semicolonSep
semicolonSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
semicolonSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.semicolonSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.sep
sep :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
sep arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.sep")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.spaceSep
spaceSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
spaceSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.spaceSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.spaceSepAdaptive
spaceSepAdaptive :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
spaceSepAdaptive arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.spaceSepAdaptive")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.squareBrackets
squareBrackets :: Typed.TypedTerm Ast.Brackets
squareBrackets = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.serialization.squareBrackets"))

-- | DSL reference to hydra.serialization.structuralSep
structuralSep :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
structuralSep arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.structuralSep")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.structuralSpaceSep
structuralSpaceSep :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
structuralSpaceSep arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.structuralSpaceSep")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.suffix
suffix :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
suffix arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.suffix")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.sym
sym :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Symbol
sym arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.sym")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.symbolSep
symbolSep :: Typed.TypedTerm String -> Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
symbolSep arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.symbolSep")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.serialization.tabIndent
tabIndent :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
tabIndent arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.tabIndent")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.tabIndentDoubleSpace
tabIndentDoubleSpace :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
tabIndentDoubleSpace arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.tabIndentDoubleSpace")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.tabIndentSingleSpace
tabIndentSingleSpace :: Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.Expr
tabIndentSingleSpace arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.tabIndentSingleSpace")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.unsupportedType
unsupportedType :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr
unsupportedType arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.unsupportedType")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.unsupportedVariant
unsupportedVariant :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm Ast.Expr
unsupportedVariant arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.unsupportedVariant")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.serialization.withComma
withComma :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
withComma arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.withComma")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.serialization.withSemi
withSemi :: Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr
withSemi arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.serialization.withSemi")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
