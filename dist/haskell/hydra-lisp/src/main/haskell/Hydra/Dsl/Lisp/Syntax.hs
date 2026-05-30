-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.lisp.syntax

module Hydra.Dsl.Lisp.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Lisp.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.lisp.syntax.AndExpression
andExpression :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.AndExpression
andExpression expressions =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.AndExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.AndExpression
andExpressionExpressions :: Typed.TypedTerm Syntax.AndExpression -> Typed.TypedTerm [Syntax.Expression]
andExpressionExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.AndExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.AndExpression
andExpressionWithExpressions :: Typed.TypedTerm Syntax.AndExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.AndExpression
andExpressionWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.AndExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.Application
application :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Application
application function arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.lisp.syntax.Application
applicationArguments :: Typed.TypedTerm Syntax.Application -> Typed.TypedTerm [Syntax.Expression]
applicationArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the function field of hydra.lisp.syntax.Application
applicationFunction :: Typed.TypedTerm Syntax.Application -> Typed.TypedTerm Syntax.Expression
applicationFunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.lisp.syntax.Application
applicationWithArguments :: Typed.TypedTerm Syntax.Application -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Application
applicationWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the function field of hydra.lisp.syntax.Application
applicationWithFunction :: Typed.TypedTerm Syntax.Application -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Application
applicationWithFunction original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.BeginExpression
beginExpression :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.BeginExpression
beginExpression expressions =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.BeginExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.BeginExpression
beginExpressionExpressions :: Typed.TypedTerm Syntax.BeginExpression -> Typed.TypedTerm [Syntax.Expression]
beginExpressionExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.BeginExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.BeginExpression
beginExpressionWithExpressions :: Typed.TypedTerm Syntax.BeginExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.BeginExpression
beginExpressionWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.BeginExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the hashTF variant of hydra.lisp.syntax.BooleanStyle
booleanStyleHashTF :: Typed.TypedTerm Syntax.BooleanStyle
booleanStyleHashTF =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hashTF"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tNil variant of hydra.lisp.syntax.BooleanStyle
booleanStyleTNil :: Typed.TypedTerm Syntax.BooleanStyle
booleanStyleTNil =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tNil"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the trueFalse variant of hydra.lisp.syntax.BooleanStyle
booleanStyleTrueFalse :: Typed.TypedTerm Syntax.BooleanStyle
booleanStyleTrueFalse =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trueFalse"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.CaseClause
caseClause :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CaseClause
caseClause keys body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Typed.unTypedTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.CaseClause
caseClauseBody :: Typed.TypedTerm Syntax.CaseClause -> Typed.TypedTerm Syntax.Expression
caseClauseBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the keys field of hydra.lisp.syntax.CaseClause
caseClauseKeys :: Typed.TypedTerm Syntax.CaseClause -> Typed.TypedTerm [Syntax.Expression]
caseClauseKeys x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.CaseClause
caseClauseWithBody :: Typed.TypedTerm Syntax.CaseClause -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CaseClause
caseClauseWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the keys field of hydra.lisp.syntax.CaseClause
caseClauseWithKeys :: Typed.TypedTerm Syntax.CaseClause -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.CaseClause
caseClauseWithKeys original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.CaseExpression
caseExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.CaseClause] -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.CaseExpression
caseExpression scrutinee clauses default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Typed.unTypedTerm scrutinee)},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Typed.unTypedTerm clauses)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the clauses field of hydra.lisp.syntax.CaseExpression
caseExpressionClauses :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm [Syntax.CaseClause]
caseExpressionClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "clauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.lisp.syntax.CaseExpression
caseExpressionDefault :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
caseExpressionDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the scrutinee field of hydra.lisp.syntax.CaseExpression
caseExpressionScrutinee :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm Syntax.Expression
caseExpressionScrutinee x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "scrutinee")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the clauses field of hydra.lisp.syntax.CaseExpression
caseExpressionWithClauses :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm [Syntax.CaseClause] -> Typed.TypedTerm Syntax.CaseExpression
caseExpressionWithClauses original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "scrutinee")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the default field of hydra.lisp.syntax.CaseExpression
caseExpressionWithDefault :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.CaseExpression
caseExpressionWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "scrutinee")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "clauses")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the scrutinee field of hydra.lisp.syntax.CaseExpression
caseExpressionWithScrutinee :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CaseExpression
caseExpressionWithScrutinee original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "clauses")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.CharacterLiteral
characterLiteral :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.CharacterLiteral
characterLiteral value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CharacterLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the value field of hydra.lisp.syntax.CharacterLiteral
characterLiteralValue :: Typed.TypedTerm Syntax.CharacterLiteral -> Typed.TypedTerm String
characterLiteralValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CharacterLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the value field of hydra.lisp.syntax.CharacterLiteral
characterLiteralWithValue :: Typed.TypedTerm Syntax.CharacterLiteral -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.CharacterLiteral
characterLiteralWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CharacterLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.Comment
comment :: Typed.TypedTerm Syntax.CommentStyle -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.Comment
comment style text =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Typed.unTypedTerm style)},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Typed.unTypedTerm text)}]}))
-- | DSL accessor for the style field of hydra.lisp.syntax.Comment
commentStyle :: Typed.TypedTerm Syntax.Comment -> Typed.TypedTerm Syntax.CommentStyle
commentStyle x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
        Core.projectionFieldName = (Core.Name "style")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the block variant of hydra.lisp.syntax.CommentStyle
commentStyleBlock :: Typed.TypedTerm Syntax.CommentStyle
commentStyleBlock =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the datum variant of hydra.lisp.syntax.CommentStyle
commentStyleDatum :: Typed.TypedTerm Syntax.CommentStyle
commentStyleDatum =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datum"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the line variant of hydra.lisp.syntax.CommentStyle
commentStyleLine :: Typed.TypedTerm Syntax.CommentStyle
commentStyleLine =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "line"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the text field of hydra.lisp.syntax.Comment
commentText :: Typed.TypedTerm Syntax.Comment -> Typed.TypedTerm String
commentText x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
        Core.projectionFieldName = (Core.Name "text")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the style field of hydra.lisp.syntax.Comment
commentWithStyle :: Typed.TypedTerm Syntax.Comment -> Typed.TypedTerm Syntax.CommentStyle -> Typed.TypedTerm Syntax.Comment
commentWithStyle original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
              Core.projectionFieldName = (Core.Name "text")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the text field of hydra.lisp.syntax.Comment
commentWithText :: Typed.TypedTerm Syntax.Comment -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.Comment
commentWithText original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.CondClause
condClause :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CondClause
condClause condition body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.CondClause
condClauseBody :: Typed.TypedTerm Syntax.CondClause -> Typed.TypedTerm Syntax.Expression
condClauseBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the condition field of hydra.lisp.syntax.CondClause
condClauseCondition :: Typed.TypedTerm Syntax.CondClause -> Typed.TypedTerm Syntax.Expression
condClauseCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.CondClause
condClauseWithBody :: Typed.TypedTerm Syntax.CondClause -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CondClause
condClauseWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the condition field of hydra.lisp.syntax.CondClause
condClauseWithCondition :: Typed.TypedTerm Syntax.CondClause -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CondClause
condClauseWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.CondExpression
condExpression :: Typed.TypedTerm [Syntax.CondClause] -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.CondExpression
condExpression clauses default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Typed.unTypedTerm clauses)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the clauses field of hydra.lisp.syntax.CondExpression
condExpressionClauses :: Typed.TypedTerm Syntax.CondExpression -> Typed.TypedTerm [Syntax.CondClause]
condExpressionClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
        Core.projectionFieldName = (Core.Name "clauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.lisp.syntax.CondExpression
condExpressionDefault :: Typed.TypedTerm Syntax.CondExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
condExpressionDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the clauses field of hydra.lisp.syntax.CondExpression
condExpressionWithClauses :: Typed.TypedTerm Syntax.CondExpression -> Typed.TypedTerm [Syntax.CondClause] -> Typed.TypedTerm Syntax.CondExpression
condExpressionWithClauses original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the default field of hydra.lisp.syntax.CondExpression
condExpressionWithDefault :: Typed.TypedTerm Syntax.CondExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.CondExpression
condExpressionWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
              Core.projectionFieldName = (Core.Name "clauses")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ConsExpression
consExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConsExpression
consExpression head tail =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm tail)}]}))
-- | DSL accessor for the head field of hydra.lisp.syntax.ConsExpression
consExpressionHead :: Typed.TypedTerm Syntax.ConsExpression -> Typed.TypedTerm Syntax.Expression
consExpressionHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tail field of hydra.lisp.syntax.ConsExpression
consExpressionTail :: Typed.TypedTerm Syntax.ConsExpression -> Typed.TypedTerm Syntax.Expression
consExpressionTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.lisp.syntax.ConsExpression
consExpressionWithHead :: Typed.TypedTerm Syntax.ConsExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConsExpression
consExpressionWithHead original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
              Core.projectionFieldName = (Core.Name "tail")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.lisp.syntax.ConsExpression
consExpressionWithTail :: Typed.TypedTerm Syntax.ConsExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConsExpression
consExpressionWithTail original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ConstantDefinition
constantDefinition :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.ConstantDefinition
constantDefinition name value doc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionDoc :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring)
constantDefinitionDoc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionName :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm Syntax.Symbol
constantDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionValue :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm Syntax.Expression
constantDefinitionValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionWithDoc :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.ConstantDefinition
constantDefinitionWithDoc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionWithName :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.ConstantDefinition
constantDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionWithValue :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConstantDefinition
constantDefinitionWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.ConstructorPattern
constructorPattern :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.ConstructorPattern
constructorPattern constructor arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Typed.unTypedTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.lisp.syntax.ConstructorPattern
constructorPatternArguments :: Typed.TypedTerm Syntax.ConstructorPattern -> Typed.TypedTerm [Syntax.Pattern]
constructorPatternArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constructor field of hydra.lisp.syntax.ConstructorPattern
constructorPatternConstructor :: Typed.TypedTerm Syntax.ConstructorPattern -> Typed.TypedTerm Syntax.Symbol
constructorPatternConstructor x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
        Core.projectionFieldName = (Core.Name "constructor")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.lisp.syntax.ConstructorPattern
constructorPatternWithArguments :: Typed.TypedTerm Syntax.ConstructorPattern -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.ConstructorPattern
constructorPatternWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the constructor field of hydra.lisp.syntax.ConstructorPattern
constructorPatternWithConstructor :: Typed.TypedTerm Syntax.ConstructorPattern -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.ConstructorPattern
constructorPatternWithConstructor original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.DestructuringBinding
destructuringBinding :: Typed.TypedTerm Syntax.DestructuringPattern -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DestructuringBinding
destructuringBinding pattern value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the pattern field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingPattern :: Typed.TypedTerm Syntax.DestructuringBinding -> Typed.TypedTerm Syntax.DestructuringPattern
destructuringBindingPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingValue :: Typed.TypedTerm Syntax.DestructuringBinding -> Typed.TypedTerm Syntax.Expression
destructuringBindingValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingWithPattern :: Typed.TypedTerm Syntax.DestructuringBinding -> Typed.TypedTerm Syntax.DestructuringPattern -> Typed.TypedTerm Syntax.DestructuringBinding
destructuringBindingWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingWithValue :: Typed.TypedTerm Syntax.DestructuringBinding -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DestructuringBinding
destructuringBindingWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the associative variant of hydra.lisp.syntax.DestructuringPattern
destructuringPatternAssociative :: Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.DestructuringPattern
destructuringPatternAssociative x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "associative"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rest variant of hydra.lisp.syntax.DestructuringPattern
destructuringPatternRest :: Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.DestructuringPattern
destructuringPatternRest x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequential variant of hydra.lisp.syntax.DestructuringPattern
destructuringPatternSequential :: Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.DestructuringPattern
destructuringPatternSequential x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequential"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the clojure variant of hydra.lisp.syntax.Dialect
dialectClojure :: Typed.TypedTerm Syntax.Dialect
dialectClojure =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "clojure"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the commonLisp variant of hydra.lisp.syntax.Dialect
dialectCommonLisp :: Typed.TypedTerm Syntax.Dialect
dialectCommonLisp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "commonLisp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the emacsLisp variant of hydra.lisp.syntax.Dialect
dialectEmacsLisp :: Typed.TypedTerm Syntax.Dialect
dialectEmacsLisp =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emacsLisp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the scheme variant of hydra.lisp.syntax.Dialect
dialectScheme :: Typed.TypedTerm Syntax.Dialect
dialectScheme =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scheme"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.DoExpression
doExpression :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.DoExpression
doExpression expressions =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DoExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.DoExpression
doExpressionExpressions :: Typed.TypedTerm Syntax.DoExpression -> Typed.TypedTerm [Syntax.Expression]
doExpressionExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DoExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.DoExpression
doExpressionWithExpressions :: Typed.TypedTerm Syntax.DoExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.DoExpression
doExpressionWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DoExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.lisp.syntax.Docstring wrapper
docstring :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Docstring
docstring x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.lisp.syntax.Docstring"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.lisp.syntax.DottedPair
dottedPair :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DottedPair
dottedPair car cdr =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Typed.unTypedTerm car)},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Typed.unTypedTerm cdr)}]}))
-- | DSL accessor for the car field of hydra.lisp.syntax.DottedPair
dottedPairCar :: Typed.TypedTerm Syntax.DottedPair -> Typed.TypedTerm Syntax.Expression
dottedPairCar x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
        Core.projectionFieldName = (Core.Name "car")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the cdr field of hydra.lisp.syntax.DottedPair
dottedPairCdr :: Typed.TypedTerm Syntax.DottedPair -> Typed.TypedTerm Syntax.Expression
dottedPairCdr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
        Core.projectionFieldName = (Core.Name "cdr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the car field of hydra.lisp.syntax.DottedPair
dottedPairWithCar :: Typed.TypedTerm Syntax.DottedPair -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DottedPair
dottedPairWithCar original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
              Core.projectionFieldName = (Core.Name "cdr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the cdr field of hydra.lisp.syntax.DottedPair
dottedPairWithCdr :: Typed.TypedTerm Syntax.DottedPair -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DottedPair
dottedPairWithCdr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
              Core.projectionFieldName = (Core.Name "car")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ExportDeclaration
exportDeclaration :: Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.ExportDeclaration
exportDeclaration symbols =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ExportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Typed.unTypedTerm symbols)}]}))
-- | DSL accessor for the symbols field of hydra.lisp.syntax.ExportDeclaration
exportDeclarationSymbols :: Typed.TypedTerm Syntax.ExportDeclaration -> Typed.TypedTerm [Syntax.Symbol]
exportDeclarationSymbols x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ExportDeclaration"),
        Core.projectionFieldName = (Core.Name "symbols")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the symbols field of hydra.lisp.syntax.ExportDeclaration
exportDeclarationWithSymbols :: Typed.TypedTerm Syntax.ExportDeclaration -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.ExportDeclaration
exportDeclarationWithSymbols original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ExportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the and variant of hydra.lisp.syntax.Expression
expressionAnd :: Typed.TypedTerm Syntax.AndExpression -> Typed.TypedTerm Syntax.Expression
expressionAnd x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.lisp.syntax.Expression
expressionApplication :: Typed.TypedTerm Syntax.Application -> Typed.TypedTerm Syntax.Expression
expressionApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the begin variant of hydra.lisp.syntax.Expression
expressionBegin :: Typed.TypedTerm Syntax.BeginExpression -> Typed.TypedTerm Syntax.Expression
expressionBegin x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "begin"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the case variant of hydra.lisp.syntax.Expression
expressionCase :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm Syntax.Expression
expressionCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cond variant of hydra.lisp.syntax.Expression
expressionCond :: Typed.TypedTerm Syntax.CondExpression -> Typed.TypedTerm Syntax.Expression
expressionCond x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cond"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the cons variant of hydra.lisp.syntax.Expression
expressionCons :: Typed.TypedTerm Syntax.ConsExpression -> Typed.TypedTerm Syntax.Expression
expressionCons x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cons"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the do variant of hydra.lisp.syntax.Expression
expressionDo :: Typed.TypedTerm Syntax.DoExpression -> Typed.TypedTerm Syntax.Expression
expressionDo x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dottedPair variant of hydra.lisp.syntax.Expression
expressionDottedPair :: Typed.TypedTerm Syntax.DottedPair -> Typed.TypedTerm Syntax.Expression
expressionDottedPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dottedPair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.lisp.syntax.Expression
expressionFieldAccess :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Expression
expressionFieldAccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.lisp.syntax.Expression
expressionIf :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
expressionIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.lisp.syntax.Expression
expressionLambda :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm Syntax.Expression
expressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the let variant of hydra.lisp.syntax.Expression
expressionLet :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm Syntax.Expression
expressionLet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.lisp.syntax.Expression
expressionList :: Typed.TypedTerm Syntax.ListLiteral -> Typed.TypedTerm Syntax.Expression
expressionList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.lisp.syntax.Expression
expressionLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Expression
expressionLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the map variant of hydra.lisp.syntax.Expression
expressionMap :: Typed.TypedTerm Syntax.MapLiteral -> Typed.TypedTerm Syntax.Expression
expressionMap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the not variant of hydra.lisp.syntax.Expression
expressionNot :: Typed.TypedTerm Syntax.NotExpression -> Typed.TypedTerm Syntax.Expression
expressionNot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the or variant of hydra.lisp.syntax.Expression
expressionOr :: Typed.TypedTerm Syntax.OrExpression -> Typed.TypedTerm Syntax.Expression
expressionOr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the quasiquote variant of hydra.lisp.syntax.Expression
expressionQuasiquote :: Typed.TypedTerm Syntax.QuasiquoteExpression -> Typed.TypedTerm Syntax.Expression
expressionQuasiquote x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quasiquote"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the quote variant of hydra.lisp.syntax.Expression
expressionQuote :: Typed.TypedTerm Syntax.QuoteExpression -> Typed.TypedTerm Syntax.Expression
expressionQuote x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quote"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sExpression variant of hydra.lisp.syntax.Expression
expressionSExpression :: Typed.TypedTerm Syntax.SExpression -> Typed.TypedTerm Syntax.Expression
expressionSExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.lisp.syntax.Expression
expressionSet :: Typed.TypedTerm Syntax.SetLiteral -> Typed.TypedTerm Syntax.Expression
expressionSet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the splicingUnquote variant of hydra.lisp.syntax.Expression
expressionSplicingUnquote :: Typed.TypedTerm Syntax.SplicingUnquoteExpression -> Typed.TypedTerm Syntax.Expression
expressionSplicingUnquote x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "splicingUnquote"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeAnnotation variant of hydra.lisp.syntax.Expression
expressionTypeAnnotation :: Typed.TypedTerm Syntax.TypeAnnotation -> Typed.TypedTerm Syntax.Expression
expressionTypeAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unquote variant of hydra.lisp.syntax.Expression
expressionUnquote :: Typed.TypedTerm Syntax.UnquoteExpression -> Typed.TypedTerm Syntax.Expression
expressionUnquote x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unquote"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.lisp.syntax.Expression
expressionVariable :: Typed.TypedTerm Syntax.VariableReference -> Typed.TypedTerm Syntax.Expression
expressionVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the vector variant of hydra.lisp.syntax.Expression
expressionVector :: Typed.TypedTerm Syntax.VectorLiteral -> Typed.TypedTerm Syntax.Expression
expressionVector x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vector"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.FieldAccess
fieldAccess :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FieldAccess
fieldAccess recordType field target =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Typed.unTypedTerm recordType)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Typed.unTypedTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm target)}]}))
-- | DSL accessor for the field field of hydra.lisp.syntax.FieldAccess
fieldAccessField :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Symbol
fieldAccessField x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "field")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the recordType field of hydra.lisp.syntax.FieldAccess
fieldAccessRecordType :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Symbol
fieldAccessRecordType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "recordType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the target field of hydra.lisp.syntax.FieldAccess
fieldAccessTarget :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Expression
fieldAccessTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the field field of hydra.lisp.syntax.FieldAccess
fieldAccessWithField :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.FieldAccess
fieldAccessWithField original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "recordType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the recordType field of hydra.lisp.syntax.FieldAccess
fieldAccessWithRecordType :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.FieldAccess
fieldAccessWithRecordType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the target field of hydra.lisp.syntax.FieldAccess
fieldAccessWithTarget :: Typed.TypedTerm Syntax.FieldAccess -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FieldAccess
fieldAccessWithTarget original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "recordType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.FieldDefinition
fieldDefinition :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.FieldDefinition
fieldDefinition name defaultValue =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Typed.unTypedTerm defaultValue)}]}))
-- | DSL accessor for the defaultValue field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionDefaultValue :: Typed.TypedTerm Syntax.FieldDefinition -> Typed.TypedTerm (Maybe Syntax.Expression)
fieldDefinitionDefaultValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
        Core.projectionFieldName = (Core.Name "defaultValue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionName :: Typed.TypedTerm Syntax.FieldDefinition -> Typed.TypedTerm Syntax.Symbol
fieldDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the defaultValue field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionWithDefaultValue :: Typed.TypedTerm Syntax.FieldDefinition -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.FieldDefinition
fieldDefinitionWithDefaultValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionWithName :: Typed.TypedTerm Syntax.FieldDefinition -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.FieldDefinition
fieldDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.FloatLiteral
floatLiteral :: Typed.TypedTerm Double -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.FloatLiteral
floatLiteral value precision =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Typed.unTypedTerm precision)}]}))
-- | DSL accessor for the precision field of hydra.lisp.syntax.FloatLiteral
floatLiteralPrecision :: Typed.TypedTerm Syntax.FloatLiteral -> Typed.TypedTerm (Maybe String)
floatLiteralPrecision x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
        Core.projectionFieldName = (Core.Name "precision")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.FloatLiteral
floatLiteralValue :: Typed.TypedTerm Syntax.FloatLiteral -> Typed.TypedTerm Double
floatLiteralValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the precision field of hydra.lisp.syntax.FloatLiteral
floatLiteralWithPrecision :: Typed.TypedTerm Syntax.FloatLiteral -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.FloatLiteral
floatLiteralWithPrecision original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.FloatLiteral
floatLiteralWithValue :: Typed.TypedTerm Syntax.FloatLiteral -> Typed.TypedTerm Double -> Typed.TypedTerm Syntax.FloatLiteral
floatLiteralWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
              Core.projectionFieldName = (Core.Name "precision")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.FunctionDefinition
functionDefinition :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm [Syntax.TypeHint] -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinition name params restParam doc typeHints body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Typed.unTypedTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Typed.unTypedTerm typeHints)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionBody :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm [Syntax.Expression]
functionDefinitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionDoc :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring)
functionDefinitionDoc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionName :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm Syntax.Symbol
functionDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionParams :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm [Syntax.Symbol]
functionDefinitionParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the restParam field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionRestParam :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm (Maybe Syntax.Symbol)
functionDefinitionRestParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "restParam")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeHints field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionTypeHints :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm [Syntax.TypeHint]
functionDefinitionTypeHints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "typeHints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithBody :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the doc field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithDoc :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithDoc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithName :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithParams :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the restParam field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithRestParam :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithRestParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeHints field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithTypeHints :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm [Syntax.TypeHint] -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithTypeHints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.IfExpression
ifExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.IfExpression
ifExpression condition then_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the condition field of hydra.lisp.syntax.IfExpression
ifExpressionCondition :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
ifExpressionCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.lisp.syntax.IfExpression
ifExpressionElse :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
ifExpressionElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.lisp.syntax.IfExpression
ifExpressionThen :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
ifExpressionThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the condition field of hydra.lisp.syntax.IfExpression
ifExpressionWithCondition :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfExpression
ifExpressionWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.lisp.syntax.IfExpression
ifExpressionWithElse :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.IfExpression
ifExpressionWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the then field of hydra.lisp.syntax.IfExpression
ifExpressionWithThen :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfExpression
ifExpressionWithThen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.ImportDeclaration
importDeclaration :: Typed.TypedTerm Syntax.NamespaceName -> Typed.TypedTerm Syntax.ImportSpec -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclaration module_ spec =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Typed.unTypedTerm spec)}]}))
-- | DSL accessor for the module field of hydra.lisp.syntax.ImportDeclaration
importDeclarationModule :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.NamespaceName
importDeclarationModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the spec field of hydra.lisp.syntax.ImportDeclaration
importDeclarationSpec :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.ImportSpec
importDeclarationSpec x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
        Core.projectionFieldName = (Core.Name "spec")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the module field of hydra.lisp.syntax.ImportDeclaration
importDeclarationWithModule :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.NamespaceName -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the spec field of hydra.lisp.syntax.ImportDeclaration
importDeclarationWithSpec :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.ImportSpec -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationWithSpec original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the alias variant of hydra.lisp.syntax.ImportSpec
importSpecAlias :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.ImportSpec
importSpecAlias x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alias"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the all variant of hydra.lisp.syntax.ImportSpec
importSpecAll :: Typed.TypedTerm Syntax.ImportSpec
importSpecAll =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the only variant of hydra.lisp.syntax.ImportSpec
importSpecOnly :: Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.ImportSpec
importSpecOnly x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "only"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rename variant of hydra.lisp.syntax.ImportSpec
importSpecRename :: Typed.TypedTerm [[Syntax.Symbol]] -> Typed.TypedTerm Syntax.ImportSpec
importSpecRename x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.IntegerLiteral
integerLiteral :: Typed.TypedTerm Integer -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.IntegerLiteral
integerLiteral value bigint =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Typed.unTypedTerm bigint)}]}))
-- | DSL accessor for the bigint field of hydra.lisp.syntax.IntegerLiteral
integerLiteralBigint :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Bool
integerLiteralBigint x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
        Core.projectionFieldName = (Core.Name "bigint")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.IntegerLiteral
integerLiteralValue :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Integer
integerLiteralValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bigint field of hydra.lisp.syntax.IntegerLiteral
integerLiteralWithBigint :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.IntegerLiteral
integerLiteralWithBigint original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.IntegerLiteral
integerLiteralWithValue :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.IntegerLiteral
integerLiteralWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
              Core.projectionFieldName = (Core.Name "bigint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.Keyword
keyword :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Keyword
keyword name namespace =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Typed.unTypedTerm namespace)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.Keyword
keywordName :: Typed.TypedTerm Syntax.Keyword -> Typed.TypedTerm String
keywordName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the namespace field of hydra.lisp.syntax.Keyword
keywordNamespace :: Typed.TypedTerm Syntax.Keyword -> Typed.TypedTerm (Maybe String)
keywordNamespace x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.Keyword
keywordWithName :: Typed.TypedTerm Syntax.Keyword -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.Keyword
keywordWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the namespace field of hydra.lisp.syntax.Keyword
keywordWithNamespace :: Typed.TypedTerm Syntax.Keyword -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Keyword
keywordWithNamespace original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.Lambda
lambda :: Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Lambda
lambda name params restParam body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Typed.unTypedTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.Lambda
lambdaBody :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm [Syntax.Expression]
lambdaBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.Lambda
lambdaName :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm (Maybe Syntax.Symbol)
lambdaName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.lisp.syntax.Lambda
lambdaParams :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm [Syntax.Symbol]
lambdaParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the restParam field of hydra.lisp.syntax.Lambda
lambdaRestParam :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm (Maybe Syntax.Symbol)
lambdaRestParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "restParam")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.Lambda
lambdaWithBody :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Lambda
lambdaWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.Lambda
lambdaWithName :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm Syntax.Lambda
lambdaWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.lisp.syntax.Lambda
lambdaWithParams :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.Lambda
lambdaWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the restParam field of hydra.lisp.syntax.Lambda
lambdaWithRestParam :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm Syntax.Lambda
lambdaWithRestParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the destructuring variant of hydra.lisp.syntax.LetBinding
letBindingDestructuring :: Typed.TypedTerm Syntax.DestructuringBinding -> Typed.TypedTerm Syntax.LetBinding
letBindingDestructuring x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.lisp.syntax.LetBinding
letBindingSimple :: Typed.TypedTerm Syntax.SimpleBinding -> Typed.TypedTerm Syntax.LetBinding
letBindingSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.LetExpression
letExpression :: Typed.TypedTerm Syntax.LetKind -> Typed.TypedTerm [Syntax.LetBinding] -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.LetExpression
letExpression kind bindings body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the bindings field of hydra.lisp.syntax.LetExpression
letExpressionBindings :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm [Syntax.LetBinding]
letExpressionBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.lisp.syntax.LetExpression
letExpressionBody :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm [Syntax.Expression]
letExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kind field of hydra.lisp.syntax.LetExpression
letExpressionKind :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm Syntax.LetKind
letExpressionKind x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bindings field of hydra.lisp.syntax.LetExpression
letExpressionWithBindings :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm [Syntax.LetBinding] -> Typed.TypedTerm Syntax.LetExpression
letExpressionWithBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.lisp.syntax.LetExpression
letExpressionWithBody :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.LetExpression
letExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the kind field of hydra.lisp.syntax.LetExpression
letExpressionWithKind :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm Syntax.LetKind -> Typed.TypedTerm Syntax.LetExpression
letExpressionWithKind original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the parallel variant of hydra.lisp.syntax.LetKind
letKindParallel :: Typed.TypedTerm Syntax.LetKind
letKindParallel =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parallel"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the recursive variant of hydra.lisp.syntax.LetKind
letKindRecursive :: Typed.TypedTerm Syntax.LetKind
letKindRecursive =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recursive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sequential variant of hydra.lisp.syntax.LetKind
letKindSequential :: Typed.TypedTerm Syntax.LetKind
letKindSequential =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequential"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.ListLiteral
listLiteral :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ListLiteral
listLiteral elements quoted =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm elements)},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Typed.unTypedTerm quoted)}]}))
-- | DSL accessor for the elements field of hydra.lisp.syntax.ListLiteral
listLiteralElements :: Typed.TypedTerm Syntax.ListLiteral -> Typed.TypedTerm [Syntax.Expression]
listLiteralElements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the quoted field of hydra.lisp.syntax.ListLiteral
listLiteralQuoted :: Typed.TypedTerm Syntax.ListLiteral -> Typed.TypedTerm Bool
listLiteralQuoted x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
        Core.projectionFieldName = (Core.Name "quoted")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the elements field of hydra.lisp.syntax.ListLiteral
listLiteralWithElements :: Typed.TypedTerm Syntax.ListLiteral -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ListLiteral
listLiteralWithElements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
              Core.projectionFieldName = (Core.Name "quoted")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the quoted field of hydra.lisp.syntax.ListLiteral
listLiteralWithQuoted :: Typed.TypedTerm Syntax.ListLiteral -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ListLiteral
listLiteralWithQuoted original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
              Core.projectionFieldName = (Core.Name "elements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the boolean variant of hydra.lisp.syntax.Literal
literalBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Literal
literalBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the character variant of hydra.lisp.syntax.Literal
literalCharacter :: Typed.TypedTerm Syntax.CharacterLiteral -> Typed.TypedTerm Syntax.Literal
literalCharacter x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the float variant of hydra.lisp.syntax.Literal
literalFloat :: Typed.TypedTerm Syntax.FloatLiteral -> Typed.TypedTerm Syntax.Literal
literalFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.lisp.syntax.Literal
literalInteger :: Typed.TypedTerm Syntax.IntegerLiteral -> Typed.TypedTerm Syntax.Literal
literalInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the keyword variant of hydra.lisp.syntax.Literal
literalKeyword :: Typed.TypedTerm Syntax.Keyword -> Typed.TypedTerm Syntax.Literal
literalKeyword x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nil variant of hydra.lisp.syntax.Literal
literalNil :: Typed.TypedTerm Syntax.Literal
literalNil =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.LiteralPattern
literalPattern :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.LiteralPattern
literalPattern value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LiteralPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the value field of hydra.lisp.syntax.LiteralPattern
literalPatternValue :: Typed.TypedTerm Syntax.LiteralPattern -> Typed.TypedTerm Syntax.Literal
literalPatternValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LiteralPattern"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the value field of hydra.lisp.syntax.LiteralPattern
literalPatternWithValue :: Typed.TypedTerm Syntax.LiteralPattern -> Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.LiteralPattern
literalPatternWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LiteralPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the string variant of hydra.lisp.syntax.Literal
literalString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Literal
literalString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the symbol variant of hydra.lisp.syntax.Literal
literalSymbol :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Literal
literalSymbol x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.MacroDefinition
macroDefinition :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.MacroDefinition
macroDefinition name params restParam body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Typed.unTypedTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.MacroDefinition
macroDefinitionBody :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm [Syntax.Expression]
macroDefinitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.MacroDefinition
macroDefinitionName :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm Syntax.Symbol
macroDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.lisp.syntax.MacroDefinition
macroDefinitionParams :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm [Syntax.Symbol]
macroDefinitionParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the restParam field of hydra.lisp.syntax.MacroDefinition
macroDefinitionRestParam :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm (Maybe Syntax.Symbol)
macroDefinitionRestParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "restParam")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithBody :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.MacroDefinition
macroDefinitionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithName :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.MacroDefinition
macroDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithParams :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm [Syntax.Symbol] -> Typed.TypedTerm Syntax.MacroDefinition
macroDefinitionWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the restParam field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithRestParam :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm (Maybe Syntax.Symbol) -> Typed.TypedTerm Syntax.MacroDefinition
macroDefinitionWithRestParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.MapEntry
mapEntry :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.MapEntry
mapEntry key value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.lisp.syntax.MapEntry
mapEntryKey :: Typed.TypedTerm Syntax.MapEntry -> Typed.TypedTerm Syntax.Expression
mapEntryKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.MapEntry
mapEntryValue :: Typed.TypedTerm Syntax.MapEntry -> Typed.TypedTerm Syntax.Expression
mapEntryValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.lisp.syntax.MapEntry
mapEntryWithKey :: Typed.TypedTerm Syntax.MapEntry -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.MapEntry
mapEntryWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.MapEntry
mapEntryWithValue :: Typed.TypedTerm Syntax.MapEntry -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.MapEntry
mapEntryWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.MapLiteral
mapLiteral :: Typed.TypedTerm [Syntax.MapEntry] -> Typed.TypedTerm Syntax.MapLiteral
mapLiteral entries =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Typed.unTypedTerm entries)}]}))
-- | DSL accessor for the entries field of hydra.lisp.syntax.MapLiteral
mapLiteralEntries :: Typed.TypedTerm Syntax.MapLiteral -> Typed.TypedTerm [Syntax.MapEntry]
mapLiteralEntries x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapLiteral"),
        Core.projectionFieldName = (Core.Name "entries")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the entries field of hydra.lisp.syntax.MapLiteral
mapLiteralWithEntries :: Typed.TypedTerm Syntax.MapLiteral -> Typed.TypedTerm [Syntax.MapEntry] -> Typed.TypedTerm Syntax.MapLiteral
mapLiteralWithEntries original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ModuleDeclaration
moduleDeclaration :: Typed.TypedTerm Syntax.NamespaceName -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.ModuleDeclaration
moduleDeclaration name doc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationDoc :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm (Maybe Syntax.Docstring)
moduleDeclarationDoc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationName :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm Syntax.NamespaceName
moduleDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationWithDoc :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.ModuleDeclaration
moduleDeclarationWithDoc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationWithName :: Typed.TypedTerm Syntax.ModuleDeclaration -> Typed.TypedTerm Syntax.NamespaceName -> Typed.TypedTerm Syntax.ModuleDeclaration
moduleDeclarationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.lisp.syntax.NamespaceName wrapper
namespaceName :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.NamespaceName
namespaceName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.lisp.syntax.NamespaceName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the emptyList variant of hydra.lisp.syntax.NilStyle
nilStyleEmptyList :: Typed.TypedTerm Syntax.NilStyle
nilStyleEmptyList =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.NilStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyList"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nil variant of hydra.lisp.syntax.NilStyle
nilStyleNil :: Typed.TypedTerm Syntax.NilStyle
nilStyleNil =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.NilStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.NotExpression
notExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.NotExpression
notExpression expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.lisp.syntax.NotExpression
notExpressionExpression :: Typed.TypedTerm Syntax.NotExpression -> Typed.TypedTerm Syntax.Expression
notExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.NotExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.lisp.syntax.NotExpression
notExpressionWithExpression :: Typed.TypedTerm Syntax.NotExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.NotExpression
notExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.OrExpression
orExpression :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.OrExpression
orExpression expressions =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.OrExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.OrExpression
orExpressionExpressions :: Typed.TypedTerm Syntax.OrExpression -> Typed.TypedTerm [Syntax.Expression]
orExpressionExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.OrExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.OrExpression
orExpressionWithExpressions :: Typed.TypedTerm Syntax.OrExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.OrExpression
orExpressionWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.OrExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the constructor variant of hydra.lisp.syntax.Pattern
patternConstructor :: Typed.TypedTerm Syntax.ConstructorPattern -> Typed.TypedTerm Syntax.Pattern
patternConstructor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.lisp.syntax.Pattern
patternLiteral :: Typed.TypedTerm Syntax.LiteralPattern -> Typed.TypedTerm Syntax.Pattern
patternLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.lisp.syntax.Pattern
patternVariable :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Pattern
patternVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.lisp.syntax.Pattern
patternWildcard :: Typed.TypedTerm Syntax.WildcardPattern -> Typed.TypedTerm Syntax.Pattern
patternWildcard x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.Program
program :: Typed.TypedTerm Syntax.Dialect -> Typed.TypedTerm (Maybe Syntax.ModuleDeclaration) -> Typed.TypedTerm [Syntax.ImportDeclaration] -> Typed.TypedTerm [Syntax.ExportDeclaration] -> Typed.TypedTerm [Syntax.TopLevelFormWithComments] -> Typed.TypedTerm Syntax.Program
program dialect module_ imports exports forms =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Typed.unTypedTerm dialect)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Typed.unTypedTerm exports)},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Typed.unTypedTerm forms)}]}))
-- | DSL accessor for the dialect field of hydra.lisp.syntax.Program
programDialect :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm Syntax.Dialect
programDialect x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "dialect")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the exports field of hydra.lisp.syntax.Program
programExports :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.ExportDeclaration]
programExports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "exports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the forms field of hydra.lisp.syntax.Program
programForms :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.TopLevelFormWithComments]
programForms x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "forms")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the imports field of hydra.lisp.syntax.Program
programImports :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.ImportDeclaration]
programImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the module field of hydra.lisp.syntax.Program
programModule :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm (Maybe Syntax.ModuleDeclaration)
programModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dialect field of hydra.lisp.syntax.Program
programWithDialect :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm Syntax.Dialect -> Typed.TypedTerm Syntax.Program
programWithDialect original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the exports field of hydra.lisp.syntax.Program
programWithExports :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.ExportDeclaration] -> Typed.TypedTerm Syntax.Program
programWithExports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the forms field of hydra.lisp.syntax.Program
programWithForms :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.TopLevelFormWithComments] -> Typed.TypedTerm Syntax.Program
programWithForms original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the imports field of hydra.lisp.syntax.Program
programWithImports :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.ImportDeclaration] -> Typed.TypedTerm Syntax.Program
programWithImports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the module field of hydra.lisp.syntax.Program
programWithModule :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm (Maybe Syntax.ModuleDeclaration) -> Typed.TypedTerm Syntax.Program
programWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbol :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.QualifiedSymbol
qualifiedSymbol namespace name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Typed.unTypedTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolName :: Typed.TypedTerm Syntax.QualifiedSymbol -> Typed.TypedTerm String
qualifiedSymbolName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the namespace field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolNamespace :: Typed.TypedTerm Syntax.QualifiedSymbol -> Typed.TypedTerm String
qualifiedSymbolNamespace x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolWithName :: Typed.TypedTerm Syntax.QualifiedSymbol -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.QualifiedSymbol
qualifiedSymbolWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the namespace field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolWithNamespace :: Typed.TypedTerm Syntax.QualifiedSymbol -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.QualifiedSymbol
qualifiedSymbolWithNamespace original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.QuasiquoteExpression
quasiquoteExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.QuasiquoteExpression
quasiquoteExpression body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuasiquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.QuasiquoteExpression
quasiquoteExpressionBody :: Typed.TypedTerm Syntax.QuasiquoteExpression -> Typed.TypedTerm Syntax.Expression
quasiquoteExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QuasiquoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.QuasiquoteExpression
quasiquoteExpressionWithBody :: Typed.TypedTerm Syntax.QuasiquoteExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.QuasiquoteExpression
quasiquoteExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuasiquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.QuoteExpression
quoteExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.QuoteExpression
quoteExpression body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.QuoteExpression
quoteExpressionBody :: Typed.TypedTerm Syntax.QuoteExpression -> Typed.TypedTerm Syntax.Expression
quoteExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QuoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.QuoteExpression
quoteExpressionWithBody :: Typed.TypedTerm Syntax.QuoteExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.QuoteExpression
quoteExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinition :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm [Syntax.FieldDefinition] -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.RecordTypeDefinition
recordTypeDefinition name fields doc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionDoc :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring)
recordTypeDefinitionDoc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the fields field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionFields :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm [Syntax.FieldDefinition]
recordTypeDefinitionFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionName :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm Syntax.Symbol
recordTypeDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionWithDoc :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithDoc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the fields field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionWithFields :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm [Syntax.FieldDefinition] -> Typed.TypedTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionWithName :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the atom variant of hydra.lisp.syntax.SExpression
sExpressionAtom :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.SExpression
sExpressionAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.SExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.lisp.syntax.SExpression
sExpressionList :: Typed.TypedTerm [Syntax.SExpression] -> Typed.TypedTerm Syntax.SExpression
sExpressionList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.SExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.SetLiteral
setLiteral :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.SetLiteral
setLiteral elements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SetLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm elements)}]}))
-- | DSL accessor for the elements field of hydra.lisp.syntax.SetLiteral
setLiteralElements :: Typed.TypedTerm Syntax.SetLiteral -> Typed.TypedTerm [Syntax.Expression]
setLiteralElements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SetLiteral"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the elements field of hydra.lisp.syntax.SetLiteral
setLiteralWithElements :: Typed.TypedTerm Syntax.SetLiteral -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.SetLiteral
setLiteralWithElements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SetLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.SimpleBinding
simpleBinding :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SimpleBinding
simpleBinding name value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.SimpleBinding
simpleBindingName :: Typed.TypedTerm Syntax.SimpleBinding -> Typed.TypedTerm Syntax.Symbol
simpleBindingName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.SimpleBinding
simpleBindingValue :: Typed.TypedTerm Syntax.SimpleBinding -> Typed.TypedTerm Syntax.Expression
simpleBindingValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.SimpleBinding
simpleBindingWithName :: Typed.TypedTerm Syntax.SimpleBinding -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.SimpleBinding
simpleBindingWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.SimpleBinding
simpleBindingWithValue :: Typed.TypedTerm Syntax.SimpleBinding -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SimpleBinding
simpleBindingWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.SplicingUnquoteExpression
splicingUnquoteExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SplicingUnquoteExpression
splicingUnquoteExpression body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.SplicingUnquoteExpression
splicingUnquoteExpressionBody :: Typed.TypedTerm Syntax.SplicingUnquoteExpression -> Typed.TypedTerm Syntax.Expression
splicingUnquoteExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.SplicingUnquoteExpression
splicingUnquoteExpressionWithBody :: Typed.TypedTerm Syntax.SplicingUnquoteExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SplicingUnquoteExpression
splicingUnquoteExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.lisp.syntax.Symbol wrapper
symbol :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Symbol
symbol x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.lisp.syntax.Symbol"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the constant variant of hydra.lisp.syntax.TopLevelForm
topLevelFormConstant :: Typed.TypedTerm Syntax.ConstantDefinition -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormConstant x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.lisp.syntax.TopLevelForm
topLevelFormExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.lisp.syntax.TopLevelForm
topLevelFormFunction :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the macro variant of hydra.lisp.syntax.TopLevelForm
topLevelFormMacro :: Typed.TypedTerm Syntax.MacroDefinition -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormMacro x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the recordType variant of hydra.lisp.syntax.TopLevelForm
topLevelFormRecordType :: Typed.TypedTerm Syntax.RecordTypeDefinition -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormRecordType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordType"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.lisp.syntax.TopLevelForm
topLevelFormVariable :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithComments :: Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm (Maybe Syntax.Comment) -> Typed.TypedTerm Syntax.TopLevelForm -> Typed.TypedTerm Syntax.TopLevelFormWithComments
topLevelFormWithComments doc comment form =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Typed.unTypedTerm form)}]}))
-- | DSL accessor for the comment field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsComment :: Typed.TypedTerm Syntax.TopLevelFormWithComments -> Typed.TypedTerm (Maybe Syntax.Comment)
topLevelFormWithCommentsComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsDoc :: Typed.TypedTerm Syntax.TopLevelFormWithComments -> Typed.TypedTerm (Maybe Syntax.Docstring)
topLevelFormWithCommentsDoc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the form field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsForm :: Typed.TypedTerm Syntax.TopLevelFormWithComments -> Typed.TypedTerm Syntax.TopLevelForm
topLevelFormWithCommentsForm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionFieldName = (Core.Name "form")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comment field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithComment :: Typed.TypedTerm Syntax.TopLevelFormWithComments -> Typed.TypedTerm (Maybe Syntax.Comment) -> Typed.TypedTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "form")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the doc field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithDoc :: Typed.TypedTerm Syntax.TopLevelFormWithComments -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithDoc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "form")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the form field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithForm :: Typed.TypedTerm Syntax.TopLevelFormWithComments -> Typed.TypedTerm Syntax.TopLevelForm -> Typed.TypedTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithForm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.TypeAnnotation
typeAnnotation :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeAnnotation
typeAnnotation expression type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the expression field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationExpression :: Typed.TypedTerm Syntax.TypeAnnotation -> Typed.TypedTerm Syntax.Expression
typeAnnotationExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationType :: Typed.TypedTerm Syntax.TypeAnnotation -> Typed.TypedTerm Syntax.TypeSpecifier
typeAnnotationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationWithExpression :: Typed.TypedTerm Syntax.TypeAnnotation -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeAnnotation
typeAnnotationWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationWithType :: Typed.TypedTerm Syntax.TypeAnnotation -> Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeAnnotation
typeAnnotationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.TypeHint
typeHint :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeHint
typeHint name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.TypeHint
typeHintName :: Typed.TypedTerm Syntax.TypeHint -> Typed.TypedTerm Syntax.Symbol
typeHintName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.lisp.syntax.TypeHint
typeHintType :: Typed.TypedTerm Syntax.TypeHint -> Typed.TypedTerm Syntax.TypeSpecifier
typeHintType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.TypeHint
typeHintWithName :: Typed.TypedTerm Syntax.TypeHint -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.TypeHint
typeHintWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.lisp.syntax.TypeHint
typeHintWithType :: Typed.TypedTerm Syntax.TypeHint -> Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeHint
typeHintWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the either variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierEither :: Typed.TypedTerm [Syntax.TypeSpecifier] -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierEither x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierFunction :: Typed.TypedTerm [Syntax.TypeSpecifier] -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierList :: Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the map variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierMap :: Typed.TypedTerm [Syntax.TypeSpecifier] -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierMap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the maybe variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierMaybe :: Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierMaybe x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierNamed :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pair variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierPair :: Typed.TypedTerm [Syntax.TypeSpecifier] -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierSet :: Typed.TypedTerm Syntax.TypeSpecifier -> Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierSet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unit variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierUnit :: Typed.TypedTerm Syntax.TypeSpecifier
typeSpecifierUnit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the body of hydra.lisp.syntax.Docstring
unDocstring :: Typed.TypedTerm Syntax.Docstring -> Typed.TypedTerm String
unDocstring x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.lisp.syntax.Docstring")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.lisp.syntax.NamespaceName
unNamespaceName :: Typed.TypedTerm Syntax.NamespaceName -> Typed.TypedTerm String
unNamespaceName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.lisp.syntax.NamespaceName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.lisp.syntax.Symbol
unSymbol :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm String
unSymbol x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.lisp.syntax.Symbol")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.lisp.syntax.UnquoteExpression
unquoteExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.UnquoteExpression
unquoteExpression body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.UnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.UnquoteExpression
unquoteExpressionBody :: Typed.TypedTerm Syntax.UnquoteExpression -> Typed.TypedTerm Syntax.Expression
unquoteExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.UnquoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.UnquoteExpression
unquoteExpressionWithBody :: Typed.TypedTerm Syntax.UnquoteExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.UnquoteExpression
unquoteExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.UnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.VariableDefinition
variableDefinition :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.VariableDefinition
variableDefinition name value doc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.VariableDefinition
variableDefinitionDoc :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring)
variableDefinitionDoc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.VariableDefinition
variableDefinitionName :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm Syntax.Symbol
variableDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.VariableDefinition
variableDefinitionValue :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm Syntax.Expression
variableDefinitionValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.VariableDefinition
variableDefinitionWithDoc :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm (Maybe Syntax.Docstring) -> Typed.TypedTerm Syntax.VariableDefinition
variableDefinitionWithDoc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.VariableDefinition
variableDefinitionWithName :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.VariableDefinition
variableDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.VariableDefinition
variableDefinitionWithValue :: Typed.TypedTerm Syntax.VariableDefinition -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.VariableDefinition
variableDefinitionWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.VariableReference
variableReference :: Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.VariableReference
variableReference name functionNamespace =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Typed.unTypedTerm functionNamespace)}]}))
-- | DSL accessor for the functionNamespace field of hydra.lisp.syntax.VariableReference
variableReferenceFunctionNamespace :: Typed.TypedTerm Syntax.VariableReference -> Typed.TypedTerm Bool
variableReferenceFunctionNamespace x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
        Core.projectionFieldName = (Core.Name "functionNamespace")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.VariableReference
variableReferenceName :: Typed.TypedTerm Syntax.VariableReference -> Typed.TypedTerm Syntax.Symbol
variableReferenceName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the functionNamespace field of hydra.lisp.syntax.VariableReference
variableReferenceWithFunctionNamespace :: Typed.TypedTerm Syntax.VariableReference -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.VariableReference
variableReferenceWithFunctionNamespace original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.VariableReference
variableReferenceWithName :: Typed.TypedTerm Syntax.VariableReference -> Typed.TypedTerm Syntax.Symbol -> Typed.TypedTerm Syntax.VariableReference
variableReferenceWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
              Core.projectionFieldName = (Core.Name "functionNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.VectorLiteral
vectorLiteral :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.VectorLiteral
vectorLiteral elements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VectorLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm elements)}]}))
-- | DSL accessor for the elements field of hydra.lisp.syntax.VectorLiteral
vectorLiteralElements :: Typed.TypedTerm Syntax.VectorLiteral -> Typed.TypedTerm [Syntax.Expression]
vectorLiteralElements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VectorLiteral"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the elements field of hydra.lisp.syntax.VectorLiteral
vectorLiteralWithElements :: Typed.TypedTerm Syntax.VectorLiteral -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.VectorLiteral
vectorLiteralWithElements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VectorLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.WildcardPattern
wildcardPattern :: Typed.TypedTerm Syntax.WildcardPattern
wildcardPattern =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.WildcardPattern"),
      Core.recordFields = []}))
