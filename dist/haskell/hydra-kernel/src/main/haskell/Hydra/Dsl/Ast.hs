-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.ast

module Hydra.Dsl.Ast where
import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the both variant of hydra.ast.Associativity
associativityBoth :: Typed.TypedTerm Ast.Associativity
associativityBoth =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the left variant of hydra.ast.Associativity
associativityLeft :: Typed.TypedTerm Ast.Associativity
associativityLeft =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "left"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.ast.Associativity
associativityNone :: Typed.TypedTerm Ast.Associativity
associativityNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the right variant of hydra.ast.Associativity
associativityRight :: Typed.TypedTerm Ast.Associativity
associativityRight =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "right"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.ast.BlockStyle
blockStyle :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Ast.BlockStyle
blockStyle indent newlineBeforeContent newlineAfterContent =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Typed.unTypedTerm indent)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Typed.unTypedTerm newlineBeforeContent)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Typed.unTypedTerm newlineAfterContent)}]}))
-- | DSL accessor for the indent field of hydra.ast.BlockStyle
blockStyleIndent :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm (Maybe String)
blockStyleIndent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionFieldName = (Core.Name "indent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the newlineAfterContent field of hydra.ast.BlockStyle
blockStyleNewlineAfterContent :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Bool
blockStyleNewlineAfterContent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the newlineBeforeContent field of hydra.ast.BlockStyle
blockStyleNewlineBeforeContent :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Bool
blockStyleNewlineBeforeContent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the indent field of hydra.ast.BlockStyle
blockStyleWithIndent :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Ast.BlockStyle
blockStyleWithIndent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the newlineAfterContent field of hydra.ast.BlockStyle
blockStyleWithNewlineAfterContent :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Bool -> Typed.TypedTerm Ast.BlockStyle
blockStyleWithNewlineAfterContent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "indent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the newlineBeforeContent field of hydra.ast.BlockStyle
blockStyleWithNewlineBeforeContent :: Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Bool -> Typed.TypedTerm Ast.BlockStyle
blockStyleWithNewlineBeforeContent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "indent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.ast.BracketExpr
bracketExpr :: Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Ast.BracketExpr
bracketExpr brackets enclosed style =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Typed.unTypedTerm brackets)},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Typed.unTypedTerm enclosed)},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Typed.unTypedTerm style)}]}))
-- | DSL accessor for the brackets field of hydra.ast.BracketExpr
bracketExprBrackets :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.Brackets
bracketExprBrackets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionFieldName = (Core.Name "brackets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the enclosed field of hydra.ast.BracketExpr
bracketExprEnclosed :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.Expr
bracketExprEnclosed x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionFieldName = (Core.Name "enclosed")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the style field of hydra.ast.BracketExpr
bracketExprStyle :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.BlockStyle
bracketExprStyle x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionFieldName = (Core.Name "style")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the brackets field of hydra.ast.BracketExpr
bracketExprWithBrackets :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.BracketExpr
bracketExprWithBrackets original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "enclosed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the enclosed field of hydra.ast.BracketExpr
bracketExprWithEnclosed :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.BracketExpr
bracketExprWithEnclosed original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "brackets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the style field of hydra.ast.BracketExpr
bracketExprWithStyle :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.BlockStyle -> Typed.TypedTerm Ast.BracketExpr
bracketExprWithStyle original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "brackets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionFieldName = (Core.Name "enclosed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.ast.Brackets
brackets :: Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Brackets
brackets open close =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Typed.unTypedTerm open)},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Typed.unTypedTerm close)}]}))
-- | DSL accessor for the close field of hydra.ast.Brackets
bracketsClose :: Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.Symbol
bracketsClose x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
        Core.projectionFieldName = (Core.Name "close")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the open field of hydra.ast.Brackets
bracketsOpen :: Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.Symbol
bracketsOpen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
        Core.projectionFieldName = (Core.Name "open")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the close field of hydra.ast.Brackets
bracketsWithClose :: Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Brackets
bracketsWithClose original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
              Core.projectionFieldName = (Core.Name "open")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the open field of hydra.ast.Brackets
bracketsWithOpen :: Typed.TypedTerm Ast.Brackets -> Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Brackets
bracketsWithOpen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
              Core.projectionFieldName = (Core.Name "close")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the brackets variant of hydra.ast.Expr
exprBrackets :: Typed.TypedTerm Ast.BracketExpr -> Typed.TypedTerm Ast.Expr
exprBrackets x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brackets"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the const variant of hydra.ast.Expr
exprConst :: Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Expr
exprConst x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the indent variant of hydra.ast.Expr
exprIndent :: Typed.TypedTerm Ast.IndentedExpression -> Typed.TypedTerm Ast.Expr
exprIndent x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indent"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the op variant of hydra.ast.Expr
exprOp :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Expr
exprOp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "op"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the seq variant of hydra.ast.Expr
exprSeq :: Typed.TypedTerm Ast.SeqExpr -> Typed.TypedTerm Ast.Expr
exprSeq x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seq"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the allLines variant of hydra.ast.IndentStyle
indentStyleAllLines :: Typed.TypedTerm String -> Typed.TypedTerm Ast.IndentStyle
indentStyleAllLines x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "allLines"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the subsequentLines variant of hydra.ast.IndentStyle
indentStyleSubsequentLines :: Typed.TypedTerm String -> Typed.TypedTerm Ast.IndentStyle
indentStyleSubsequentLines x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subsequentLines"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.ast.IndentedExpression
indentedExpression :: Typed.TypedTerm Ast.IndentStyle -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.IndentedExpression
indentedExpression style expr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Typed.unTypedTerm style)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)}]}))
-- | DSL accessor for the expr field of hydra.ast.IndentedExpression
indentedExpressionExpr :: Typed.TypedTerm Ast.IndentedExpression -> Typed.TypedTerm Ast.Expr
indentedExpressionExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the style field of hydra.ast.IndentedExpression
indentedExpressionStyle :: Typed.TypedTerm Ast.IndentedExpression -> Typed.TypedTerm Ast.IndentStyle
indentedExpressionStyle x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
        Core.projectionFieldName = (Core.Name "style")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.ast.IndentedExpression
indentedExpressionWithExpr :: Typed.TypedTerm Ast.IndentedExpression -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.IndentedExpression
indentedExpressionWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the style field of hydra.ast.IndentedExpression
indentedExpressionWithStyle :: Typed.TypedTerm Ast.IndentedExpression -> Typed.TypedTerm Ast.IndentStyle -> Typed.TypedTerm Ast.IndentedExpression
indentedExpressionWithStyle original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.ast.Op
op :: Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Padding -> Typed.TypedTerm Ast.Precedence -> Typed.TypedTerm Ast.Associativity -> Typed.TypedTerm Ast.Op
op symbol padding precedence associativity =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Typed.unTypedTerm symbol)},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Typed.unTypedTerm padding)},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Typed.unTypedTerm precedence)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Typed.unTypedTerm associativity)}]}))
-- | DSL accessor for the associativity field of hydra.ast.Op
opAssociativity :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Associativity
opAssociativity x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "associativity")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.ast.OpExpr
opExpr :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.OpExpr
opExpr op lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.ast.OpExpr
opExprLhs :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Expr
opExprLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.ast.OpExpr
opExprOp :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Op
opExprOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.ast.OpExpr
opExprRhs :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Expr
opExprRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.ast.OpExpr
opExprWithLhs :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.OpExpr
opExprWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.ast.OpExpr
opExprWithOp :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.OpExpr
opExprWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.ast.OpExpr
opExprWithRhs :: Typed.TypedTerm Ast.OpExpr -> Typed.TypedTerm Ast.Expr -> Typed.TypedTerm Ast.OpExpr
opExprWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the padding field of hydra.ast.Op
opPadding :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Padding
opPadding x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "padding")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the precedence field of hydra.ast.Op
opPrecedence :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Precedence
opPrecedence x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "precedence")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the symbol field of hydra.ast.Op
opSymbol :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Symbol
opSymbol x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionFieldName = (Core.Name "symbol")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the associativity field of hydra.ast.Op
opWithAssociativity :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Associativity -> Typed.TypedTerm Ast.Op
opWithAssociativity original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "symbol")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "padding")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "precedence")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the padding field of hydra.ast.Op
opWithPadding :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Padding -> Typed.TypedTerm Ast.Op
opWithPadding original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "symbol")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "precedence")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the precedence field of hydra.ast.Op
opWithPrecedence :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Precedence -> Typed.TypedTerm Ast.Op
opWithPrecedence original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "symbol")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "padding")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the symbol field of hydra.ast.Op
opWithSymbol :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm Ast.Op
opWithSymbol original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "padding")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "precedence")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionFieldName = (Core.Name "associativity")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.ast.Padding
padding :: Typed.TypedTerm Ast.Ws -> Typed.TypedTerm Ast.Ws -> Typed.TypedTerm Ast.Padding
padding left right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.ast.Padding
paddingLeft :: Typed.TypedTerm Ast.Padding -> Typed.TypedTerm Ast.Ws
paddingLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.ast.Padding
paddingRight :: Typed.TypedTerm Ast.Padding -> Typed.TypedTerm Ast.Ws
paddingRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.ast.Padding
paddingWithLeft :: Typed.TypedTerm Ast.Padding -> Typed.TypedTerm Ast.Ws -> Typed.TypedTerm Ast.Padding
paddingWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.ast.Padding
paddingWithRight :: Typed.TypedTerm Ast.Padding -> Typed.TypedTerm Ast.Ws -> Typed.TypedTerm Ast.Padding
paddingWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.ast.Precedence wrapper
precedence :: Typed.TypedTerm Int -> Typed.TypedTerm Ast.Precedence
precedence x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Precedence"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.ast.SeqExpr
seqExpr :: Typed.TypedTerm Ast.Op -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.SeqExpr
seqExpr op elements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm elements)}]}))
-- | DSL accessor for the elements field of hydra.ast.SeqExpr
seqExprElements :: Typed.TypedTerm Ast.SeqExpr -> Typed.TypedTerm [Ast.Expr]
seqExprElements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.ast.SeqExpr
seqExprOp :: Typed.TypedTerm Ast.SeqExpr -> Typed.TypedTerm Ast.Op
seqExprOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the elements field of hydra.ast.SeqExpr
seqExprWithElements :: Typed.TypedTerm Ast.SeqExpr -> Typed.TypedTerm [Ast.Expr] -> Typed.TypedTerm Ast.SeqExpr
seqExprWithElements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the op field of hydra.ast.SeqExpr
seqExprWithOp :: Typed.TypedTerm Ast.SeqExpr -> Typed.TypedTerm Ast.Op -> Typed.TypedTerm Ast.SeqExpr
seqExprWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
              Core.projectionFieldName = (Core.Name "elements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.ast.Symbol wrapper
symbol :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Symbol
symbol x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Symbol"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.ast.Precedence
unPrecedence :: Typed.TypedTerm Ast.Precedence -> Typed.TypedTerm Int
unPrecedence x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.ast.Precedence")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.ast.Symbol
unSymbol :: Typed.TypedTerm Ast.Symbol -> Typed.TypedTerm String
unSymbol x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.ast.Symbol")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the break variant of hydra.ast.Ws
wsBreak :: Typed.TypedTerm Ast.Ws
wsBreak =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the breakAndIndent variant of hydra.ast.Ws
wsBreakAndIndent :: Typed.TypedTerm String -> Typed.TypedTerm Ast.Ws
wsBreakAndIndent x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "breakAndIndent"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the doubleBreak variant of hydra.ast.Ws
wsDoubleBreak :: Typed.TypedTerm Ast.Ws
wsDoubleBreak =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleBreak"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.ast.Ws
wsNone :: Typed.TypedTerm Ast.Ws
wsNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the space variant of hydra.ast.Ws
wsSpace :: Typed.TypedTerm Ast.Ws
wsSpace =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "space"),
        Core.fieldTerm = Core.TermUnit}}))
