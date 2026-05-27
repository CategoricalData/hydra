-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.ast

module Hydra.Dsl.Ast where
import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the both variant of hydra.ast.Associativity
associativityBoth :: Phantoms.TTerm Ast.Associativity
associativityBoth =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the left variant of hydra.ast.Associativity
associativityLeft :: Phantoms.TTerm Ast.Associativity
associativityLeft =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "left"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.ast.Associativity
associativityNone :: Phantoms.TTerm Ast.Associativity
associativityNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the right variant of hydra.ast.Associativity
associativityRight :: Phantoms.TTerm Ast.Associativity
associativityRight =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "right"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.ast.BlockStyle
blockStyle :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.BlockStyle
blockStyle indent newlineBeforeContent newlineAfterContent =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Phantoms.unTTerm indent)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Phantoms.unTTerm newlineBeforeContent)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Phantoms.unTTerm newlineAfterContent)}]}))
-- | DSL accessor for the indent field of hydra.ast.BlockStyle
blockStyleIndent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm (Maybe String)
blockStyleIndent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionFieldName = (Core.Name "indent")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the newlineAfterContent field of hydra.ast.BlockStyle
blockStyleNewlineAfterContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool
blockStyleNewlineAfterContent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the newlineBeforeContent field of hydra.ast.BlockStyle
blockStyleNewlineBeforeContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool
blockStyleNewlineBeforeContent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the indent field of hydra.ast.BlockStyle
blockStyleWithIndent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.BlockStyle
blockStyleWithIndent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the newlineAfterContent field of hydra.ast.BlockStyle
blockStyleWithNewlineAfterContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.BlockStyle
blockStyleWithNewlineAfterContent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "indent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the newlineBeforeContent field of hydra.ast.BlockStyle
blockStyleWithNewlineBeforeContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.BlockStyle
blockStyleWithNewlineBeforeContent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "indent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.ast.BracketExpr
bracketExpr :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Ast.BracketExpr
bracketExpr brackets enclosed style =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Phantoms.unTTerm brackets)},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Phantoms.unTTerm enclosed)},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm style)}]}))
-- | DSL accessor for the brackets field of hydra.ast.BracketExpr
bracketExprBrackets :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Brackets
bracketExprBrackets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionFieldName = (Core.Name "brackets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the enclosed field of hydra.ast.BracketExpr
bracketExprEnclosed :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Expr
bracketExprEnclosed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionFieldName = (Core.Name "enclosed")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the style field of hydra.ast.BracketExpr
bracketExprStyle :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.BlockStyle
bracketExprStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionFieldName = (Core.Name "style")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the brackets field of hydra.ast.BracketExpr
bracketExprWithBrackets :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.BracketExpr
bracketExprWithBrackets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "enclosed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the enclosed field of hydra.ast.BracketExpr
bracketExprWithEnclosed :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.BracketExpr
bracketExprWithEnclosed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "brackets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the style field of hydra.ast.BracketExpr
bracketExprWithStyle :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Ast.BracketExpr
bracketExprWithStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "brackets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "enclosed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.ast.Brackets
brackets :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Brackets
brackets open close =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm open)},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Phantoms.unTTerm close)}]}))
-- | DSL accessor for the close field of hydra.ast.Brackets
bracketsClose :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol
bracketsClose x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
        Core.projectionFieldName = (Core.Name "close")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the open field of hydra.ast.Brackets
bracketsOpen :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol
bracketsOpen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
        Core.projectionFieldName = (Core.Name "open")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the close field of hydra.ast.Brackets
bracketsWithClose :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Brackets
bracketsWithClose original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
              Core.projectionFieldName = (Core.Name "open")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the open field of hydra.ast.Brackets
bracketsWithOpen :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Brackets
bracketsWithOpen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
              Core.projectionFieldName = (Core.Name "close")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the brackets variant of hydra.ast.Expr
exprBrackets :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Expr
exprBrackets x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brackets"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the const variant of hydra.ast.Expr
exprConst :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Expr
exprConst x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the indent variant of hydra.ast.Expr
exprIndent :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.Expr
exprIndent x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the op variant of hydra.ast.Expr
exprOp :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr
exprOp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "op"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the seq variant of hydra.ast.Expr
exprSeq :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm Ast.Expr
exprSeq x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seq"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the allLines variant of hydra.ast.IndentStyle
indentStyleAllLines :: Phantoms.TTerm String -> Phantoms.TTerm Ast.IndentStyle
indentStyleAllLines x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "allLines"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the subsequentLines variant of hydra.ast.IndentStyle
indentStyleSubsequentLines :: Phantoms.TTerm String -> Phantoms.TTerm Ast.IndentStyle
indentStyleSubsequentLines x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subsequentLines"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.ast.IndentedExpression
indentedExpression :: Phantoms.TTerm Ast.IndentStyle -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.IndentedExpression
indentedExpression style expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm style)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.ast.IndentedExpression
indentedExpressionExpr :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.Expr
indentedExpressionExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the style field of hydra.ast.IndentedExpression
indentedExpressionStyle :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.IndentStyle
indentedExpressionStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
        Core.projectionFieldName = (Core.Name "style")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.ast.IndentedExpression
indentedExpressionWithExpr :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.IndentedExpression
indentedExpressionWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the style field of hydra.ast.IndentedExpression
indentedExpressionWithStyle :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.IndentStyle -> Phantoms.TTerm Ast.IndentedExpression
indentedExpressionWithStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.ast.Op
op :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Precedence -> Phantoms.TTerm Ast.Associativity -> Phantoms.TTerm Ast.Op
op symbol padding precedence associativity =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Phantoms.unTTerm symbol)},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Phantoms.unTTerm padding)},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Phantoms.unTTerm precedence)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTTerm associativity)}]}))
-- | DSL accessor for the associativity field of hydra.ast.Op
opAssociativity :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Associativity
opAssociativity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "associativity")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.ast.OpExpr
opExpr :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.OpExpr
opExpr op lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.ast.OpExpr
opExprLhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr
opExprLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.ast.OpExpr
opExprOp :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Op
opExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.ast.OpExpr
opExprRhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr
opExprRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.ast.OpExpr
opExprWithLhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.OpExpr
opExprWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.ast.OpExpr
opExprWithOp :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.OpExpr
opExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.ast.OpExpr
opExprWithRhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.OpExpr
opExprWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the padding field of hydra.ast.Op
opPadding :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Padding
opPadding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "padding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the precedence field of hydra.ast.Op
opPrecedence :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Precedence
opPrecedence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "precedence")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the symbol field of hydra.ast.Op
opSymbol :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Symbol
opSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "symbol")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the associativity field of hydra.ast.Op
opWithAssociativity :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Associativity -> Phantoms.TTerm Ast.Op
opWithAssociativity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "symbol")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "padding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "precedence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the padding field of hydra.ast.Op
opWithPadding :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Op
opWithPadding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "symbol")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "precedence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the precedence field of hydra.ast.Op
opWithPrecedence :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Precedence -> Phantoms.TTerm Ast.Op
opWithPrecedence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "symbol")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "padding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the symbol field of hydra.ast.Op
opWithSymbol :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Op
opWithSymbol original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "padding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "precedence")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.ast.Padding
padding :: Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Padding
padding left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))
-- | DSL accessor for the left field of hydra.ast.Padding
paddingLeft :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws
paddingLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the right field of hydra.ast.Padding
paddingRight :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws
paddingRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the left field of hydra.ast.Padding
paddingWithLeft :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Padding
paddingWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the right field of hydra.ast.Padding
paddingWithRight :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Padding
paddingWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.ast.Precedence wrapper
precedence :: Phantoms.TTerm Int -> Phantoms.TTerm Ast.Precedence
precedence x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Precedence"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.ast.SeqExpr
seqExpr :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm [Ast.Expr] -> Phantoms.TTerm Ast.SeqExpr
seqExpr op elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))
-- | DSL accessor for the elements field of hydra.ast.SeqExpr
seqExprElements :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm [Ast.Expr]
seqExprElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.ast.SeqExpr
seqExprOp :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm Ast.Op
seqExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the elements field of hydra.ast.SeqExpr
seqExprWithElements :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm [Ast.Expr] -> Phantoms.TTerm Ast.SeqExpr
seqExprWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the op field of hydra.ast.SeqExpr
seqExprWithOp :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.SeqExpr
seqExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
              Core.projectionFieldName = (Core.Name "elements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.ast.Symbol wrapper
symbol :: Phantoms.TTerm String -> Phantoms.TTerm Ast.Symbol
symbol x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Symbol"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.ast.Precedence
unPrecedence :: Phantoms.TTerm Ast.Precedence -> Phantoms.TTerm Int
unPrecedence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.ast.Precedence")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.ast.Symbol
unSymbol :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm String
unSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.ast.Symbol")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the break variant of hydra.ast.Ws
wsBreak :: Phantoms.TTerm Ast.Ws
wsBreak =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the breakAndIndent variant of hydra.ast.Ws
wsBreakAndIndent :: Phantoms.TTerm String -> Phantoms.TTerm Ast.Ws
wsBreakAndIndent x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "breakAndIndent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the doubleBreak variant of hydra.ast.Ws
wsDoubleBreak :: Phantoms.TTerm Ast.Ws
wsDoubleBreak =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleBreak"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.ast.Ws
wsNone :: Phantoms.TTerm Ast.Ws
wsNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the space variant of hydra.ast.Ws
wsSpace :: Phantoms.TTerm Ast.Ws
wsSpace =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "space"),
        Core.fieldTerm = Core.TermUnit}}))
