-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.lisp.syntax

module Hydra.Dsl.Lisp.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Lisp.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.lisp.syntax.AndExpression
andExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.AndExpression
andExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.AndExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.AndExpression
andExpressionExpressions :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm [Syntax.Expression]
andExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.AndExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.AndExpression
andExpressionWithExpressions :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.AndExpression
andExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.AndExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.Application
application :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Application
application function arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.lisp.syntax.Application
applicationArguments :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm [Syntax.Expression]
applicationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the function field of hydra.lisp.syntax.Application
applicationFunction :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Expression
applicationFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.lisp.syntax.Application
applicationWithArguments :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Application
applicationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the function field of hydra.lisp.syntax.Application
applicationWithFunction :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Application
applicationWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Application"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Application"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.BeginExpression
beginExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.BeginExpression
beginExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.BeginExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.BeginExpression
beginExpressionExpressions :: Phantoms.TTerm Syntax.BeginExpression -> Phantoms.TTerm [Syntax.Expression]
beginExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.BeginExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.BeginExpression
beginExpressionWithExpressions :: Phantoms.TTerm Syntax.BeginExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.BeginExpression
beginExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.BeginExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the hashTF variant of hydra.lisp.syntax.BooleanStyle
booleanStyleHashTF :: Phantoms.TTerm Syntax.BooleanStyle
booleanStyleHashTF =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hashTF"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tNil variant of hydra.lisp.syntax.BooleanStyle
booleanStyleTNil :: Phantoms.TTerm Syntax.BooleanStyle
booleanStyleTNil =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tNil"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the trueFalse variant of hydra.lisp.syntax.BooleanStyle
booleanStyleTrueFalse :: Phantoms.TTerm Syntax.BooleanStyle
booleanStyleTrueFalse =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.BooleanStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trueFalse"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.CaseClause
caseClause :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseClause
caseClause keys body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm keys)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.CaseClause
caseClauseBody :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm Syntax.Expression
caseClauseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the keys field of hydra.lisp.syntax.CaseClause
caseClauseKeys :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm [Syntax.Expression]
caseClauseKeys x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
        Core.projectionFieldName = (Core.Name "keys")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.CaseClause
caseClauseWithBody :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseClause
caseClauseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
              Core.projectionFieldName = (Core.Name "keys")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the keys field of hydra.lisp.syntax.CaseClause
caseClauseWithKeys :: Phantoms.TTerm Syntax.CaseClause -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.CaseClause
caseClauseWithKeys original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keys"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseClause"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.CaseExpression
caseExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.CaseClause] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CaseExpression
caseExpression scrutinee clauses default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Phantoms.unTTerm scrutinee)},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm clauses)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the clauses field of hydra.lisp.syntax.CaseExpression
caseExpressionClauses :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.CaseClause]
caseExpressionClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "clauses")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the default field of hydra.lisp.syntax.CaseExpression
caseExpressionDefault :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
caseExpressionDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the scrutinee field of hydra.lisp.syntax.CaseExpression
caseExpressionScrutinee :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
caseExpressionScrutinee x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "scrutinee")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the clauses field of hydra.lisp.syntax.CaseExpression
caseExpressionWithClauses :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.CaseClause] -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "scrutinee")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the default field of hydra.lisp.syntax.CaseExpression
caseExpressionWithDefault :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "scrutinee")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "clauses")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the scrutinee field of hydra.lisp.syntax.CaseExpression
caseExpressionWithScrutinee :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithScrutinee original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scrutinee"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "clauses")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.CharacterLiteral
characterLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.CharacterLiteral
characterLiteral value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CharacterLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the value field of hydra.lisp.syntax.CharacterLiteral
characterLiteralValue :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm String
characterLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CharacterLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the value field of hydra.lisp.syntax.CharacterLiteral
characterLiteralWithValue :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.CharacterLiteral
characterLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CharacterLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.Comment
comment :: Phantoms.TTerm Syntax.CommentStyle -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
comment style text =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm style)},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Phantoms.unTTerm text)}]}))
-- | DSL accessor for the style field of hydra.lisp.syntax.Comment
commentStyle :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm Syntax.CommentStyle
commentStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
        Core.projectionFieldName = (Core.Name "style")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the block variant of hydra.lisp.syntax.CommentStyle
commentStyleBlock :: Phantoms.TTerm Syntax.CommentStyle
commentStyleBlock =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the datum variant of hydra.lisp.syntax.CommentStyle
commentStyleDatum :: Phantoms.TTerm Syntax.CommentStyle
commentStyleDatum =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datum"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the line variant of hydra.lisp.syntax.CommentStyle
commentStyleLine :: Phantoms.TTerm Syntax.CommentStyle
commentStyleLine =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.CommentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "line"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the text field of hydra.lisp.syntax.Comment
commentText :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String
commentText x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
        Core.projectionFieldName = (Core.Name "text")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the style field of hydra.lisp.syntax.Comment
commentWithStyle :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm Syntax.CommentStyle -> Phantoms.TTerm Syntax.Comment
commentWithStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
              Core.projectionFieldName = (Core.Name "text")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the text field of hydra.lisp.syntax.Comment
commentWithText :: Phantoms.TTerm Syntax.Comment -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
commentWithText original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Comment"),
              Core.projectionFieldName = (Core.Name "style")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.CondClause
condClause :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CondClause
condClause condition body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.CondClause
condClauseBody :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression
condClauseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the condition field of hydra.lisp.syntax.CondClause
condClauseCondition :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression
condClauseCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.CondClause
condClauseWithBody :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CondClause
condClauseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the condition field of hydra.lisp.syntax.CondClause
condClauseWithCondition :: Phantoms.TTerm Syntax.CondClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CondClause
condClauseWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondClause"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.CondExpression
condExpression :: Phantoms.TTerm [Syntax.CondClause] -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CondExpression
condExpression clauses default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm clauses)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the clauses field of hydra.lisp.syntax.CondExpression
condExpressionClauses :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm [Syntax.CondClause]
condExpressionClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
        Core.projectionFieldName = (Core.Name "clauses")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the default field of hydra.lisp.syntax.CondExpression
condExpressionDefault :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
condExpressionDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the clauses field of hydra.lisp.syntax.CondExpression
condExpressionWithClauses :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm [Syntax.CondClause] -> Phantoms.TTerm Syntax.CondExpression
condExpressionWithClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the default field of hydra.lisp.syntax.CondExpression
condExpressionWithDefault :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.CondExpression
condExpressionWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.CondExpression"),
              Core.projectionFieldName = (Core.Name "clauses")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ConsExpression
consExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConsExpression
consExpression head tail =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm tail)}]}))
-- | DSL accessor for the head field of hydra.lisp.syntax.ConsExpression
consExpressionHead :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression
consExpressionHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tail field of hydra.lisp.syntax.ConsExpression
consExpressionTail :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression
consExpressionTail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the head field of hydra.lisp.syntax.ConsExpression
consExpressionWithHead :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConsExpression
consExpressionWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
              Core.projectionFieldName = (Core.Name "tail")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.lisp.syntax.ConsExpression
consExpressionWithTail :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConsExpression
consExpressionWithTail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConsExpression"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ConstantDefinition
constantDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinition name value doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionDoc :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
constantDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionName :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Symbol
constantDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionValue :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Expression
constantDefinitionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionWithDoc :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionWithName :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.ConstantDefinition
constantDefinitionWithValue :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConstantDefinition
constantDefinitionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstantDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.ConstructorPattern
constructorPattern :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ConstructorPattern
constructorPattern constructor arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm constructor)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.lisp.syntax.ConstructorPattern
constructorPatternArguments :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm [Syntax.Pattern]
constructorPatternArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the constructor field of hydra.lisp.syntax.ConstructorPattern
constructorPatternConstructor :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm Syntax.Symbol
constructorPatternConstructor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
        Core.projectionFieldName = (Core.Name "constructor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.lisp.syntax.ConstructorPattern
constructorPatternWithArguments :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ConstructorPattern
constructorPatternWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
              Core.projectionFieldName = (Core.Name "constructor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the constructor field of hydra.lisp.syntax.ConstructorPattern
constructorPatternWithConstructor :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.ConstructorPattern
constructorPatternWithConstructor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constructor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ConstructorPattern"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.DestructuringBinding
destructuringBinding :: Phantoms.TTerm Syntax.DestructuringPattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DestructuringBinding
destructuringBinding pattern value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the pattern field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingPattern :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringBindingPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingValue :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.Expression
destructuringBindingValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the pattern field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingWithPattern :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.DestructuringPattern -> Phantoms.TTerm Syntax.DestructuringBinding
destructuringBindingWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.DestructuringBinding
destructuringBindingWithValue :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DestructuringBinding
destructuringBindingWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the associative variant of hydra.lisp.syntax.DestructuringPattern
destructuringPatternAssociative :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringPatternAssociative x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "associative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the rest variant of hydra.lisp.syntax.DestructuringPattern
destructuringPatternRest :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringPatternRest x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sequential variant of hydra.lisp.syntax.DestructuringPattern
destructuringPatternSequential :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.DestructuringPattern
destructuringPatternSequential x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.DestructuringPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequential"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the clojure variant of hydra.lisp.syntax.Dialect
dialectClojure :: Phantoms.TTerm Syntax.Dialect
dialectClojure =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "clojure"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the commonLisp variant of hydra.lisp.syntax.Dialect
dialectCommonLisp :: Phantoms.TTerm Syntax.Dialect
dialectCommonLisp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "commonLisp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the emacsLisp variant of hydra.lisp.syntax.Dialect
dialectEmacsLisp :: Phantoms.TTerm Syntax.Dialect
dialectEmacsLisp =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emacsLisp"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the scheme variant of hydra.lisp.syntax.Dialect
dialectScheme :: Phantoms.TTerm Syntax.Dialect
dialectScheme =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Dialect"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "scheme"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.DoExpression
doExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.DoExpression
doExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DoExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.DoExpression
doExpressionExpressions :: Phantoms.TTerm Syntax.DoExpression -> Phantoms.TTerm [Syntax.Expression]
doExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DoExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.DoExpression
doExpressionWithExpressions :: Phantoms.TTerm Syntax.DoExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.DoExpression
doExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DoExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.lisp.syntax.Docstring wrapper
docstring :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Docstring
docstring x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.lisp.syntax.Docstring"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.lisp.syntax.DottedPair
dottedPair :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DottedPair
dottedPair car cdr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Phantoms.unTTerm car)},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Phantoms.unTTerm cdr)}]}))
-- | DSL accessor for the car field of hydra.lisp.syntax.DottedPair
dottedPairCar :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression
dottedPairCar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
        Core.projectionFieldName = (Core.Name "car")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the cdr field of hydra.lisp.syntax.DottedPair
dottedPairCdr :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression
dottedPairCdr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
        Core.projectionFieldName = (Core.Name "cdr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the car field of hydra.lisp.syntax.DottedPair
dottedPairWithCar :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DottedPair
dottedPairWithCar original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
              Core.projectionFieldName = (Core.Name "cdr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the cdr field of hydra.lisp.syntax.DottedPair
dottedPairWithCdr :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DottedPair
dottedPairWithCdr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "car"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.DottedPair"),
              Core.projectionFieldName = (Core.Name "car")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cdr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ExportDeclaration
exportDeclaration :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclaration symbols =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ExportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Phantoms.unTTerm symbols)}]}))
-- | DSL accessor for the symbols field of hydra.lisp.syntax.ExportDeclaration
exportDeclarationSymbols :: Phantoms.TTerm Syntax.ExportDeclaration -> Phantoms.TTerm [Syntax.Symbol]
exportDeclarationSymbols x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ExportDeclaration"),
        Core.projectionFieldName = (Core.Name "symbols")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the symbols field of hydra.lisp.syntax.ExportDeclaration
exportDeclarationWithSymbols :: Phantoms.TTerm Syntax.ExportDeclaration -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclarationWithSymbols original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ExportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbols"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the and variant of hydra.lisp.syntax.Expression
expressionAnd :: Phantoms.TTerm Syntax.AndExpression -> Phantoms.TTerm Syntax.Expression
expressionAnd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the application variant of hydra.lisp.syntax.Expression
expressionApplication :: Phantoms.TTerm Syntax.Application -> Phantoms.TTerm Syntax.Expression
expressionApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the begin variant of hydra.lisp.syntax.Expression
expressionBegin :: Phantoms.TTerm Syntax.BeginExpression -> Phantoms.TTerm Syntax.Expression
expressionBegin x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "begin"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the case variant of hydra.lisp.syntax.Expression
expressionCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
expressionCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the cond variant of hydra.lisp.syntax.Expression
expressionCond :: Phantoms.TTerm Syntax.CondExpression -> Phantoms.TTerm Syntax.Expression
expressionCond x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cond"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the cons variant of hydra.lisp.syntax.Expression
expressionCons :: Phantoms.TTerm Syntax.ConsExpression -> Phantoms.TTerm Syntax.Expression
expressionCons x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "cons"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the do variant of hydra.lisp.syntax.Expression
expressionDo :: Phantoms.TTerm Syntax.DoExpression -> Phantoms.TTerm Syntax.Expression
expressionDo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the dottedPair variant of hydra.lisp.syntax.Expression
expressionDottedPair :: Phantoms.TTerm Syntax.DottedPair -> Phantoms.TTerm Syntax.Expression
expressionDottedPair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dottedPair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the fieldAccess variant of hydra.lisp.syntax.Expression
expressionFieldAccess :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Expression
expressionFieldAccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fieldAccess"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the if variant of hydra.lisp.syntax.Expression
expressionIf :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
expressionIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lambda variant of hydra.lisp.syntax.Expression
expressionLambda :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the let variant of hydra.lisp.syntax.Expression
expressionLet :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression
expressionLet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.lisp.syntax.Expression
expressionList :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm Syntax.Expression
expressionList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.lisp.syntax.Expression
expressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the map variant of hydra.lisp.syntax.Expression
expressionMap :: Phantoms.TTerm Syntax.MapLiteral -> Phantoms.TTerm Syntax.Expression
expressionMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the not variant of hydra.lisp.syntax.Expression
expressionNot :: Phantoms.TTerm Syntax.NotExpression -> Phantoms.TTerm Syntax.Expression
expressionNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the or variant of hydra.lisp.syntax.Expression
expressionOr :: Phantoms.TTerm Syntax.OrExpression -> Phantoms.TTerm Syntax.Expression
expressionOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the quasiquote variant of hydra.lisp.syntax.Expression
expressionQuasiquote :: Phantoms.TTerm Syntax.QuasiquoteExpression -> Phantoms.TTerm Syntax.Expression
expressionQuasiquote x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quasiquote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the quote variant of hydra.lisp.syntax.Expression
expressionQuote :: Phantoms.TTerm Syntax.QuoteExpression -> Phantoms.TTerm Syntax.Expression
expressionQuote x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sExpression variant of hydra.lisp.syntax.Expression
expressionSExpression :: Phantoms.TTerm Syntax.SExpression -> Phantoms.TTerm Syntax.Expression
expressionSExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the set variant of hydra.lisp.syntax.Expression
expressionSet :: Phantoms.TTerm Syntax.SetLiteral -> Phantoms.TTerm Syntax.Expression
expressionSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the splicingUnquote variant of hydra.lisp.syntax.Expression
expressionSplicingUnquote :: Phantoms.TTerm Syntax.SplicingUnquoteExpression -> Phantoms.TTerm Syntax.Expression
expressionSplicingUnquote x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "splicingUnquote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeAnnotation variant of hydra.lisp.syntax.Expression
expressionTypeAnnotation :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.Expression
expressionTypeAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unquote variant of hydra.lisp.syntax.Expression
expressionUnquote :: Phantoms.TTerm Syntax.UnquoteExpression -> Phantoms.TTerm Syntax.Expression
expressionUnquote x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unquote"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.lisp.syntax.Expression
expressionVariable :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Syntax.Expression
expressionVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the vector variant of hydra.lisp.syntax.Expression
expressionVector :: Phantoms.TTerm Syntax.VectorLiteral -> Phantoms.TTerm Syntax.Expression
expressionVector x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vector"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.FieldAccess
fieldAccess :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldAccess
fieldAccess recordType field target =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Phantoms.unTTerm recordType)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)}]}))
-- | DSL accessor for the field field of hydra.lisp.syntax.FieldAccess
fieldAccessField :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol
fieldAccessField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "field")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the recordType field of hydra.lisp.syntax.FieldAccess
fieldAccessRecordType :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol
fieldAccessRecordType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "recordType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the target field of hydra.lisp.syntax.FieldAccess
fieldAccessTarget :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Expression
fieldAccessTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the field field of hydra.lisp.syntax.FieldAccess
fieldAccessWithField :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "recordType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the recordType field of hydra.lisp.syntax.FieldAccess
fieldAccessWithRecordType :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithRecordType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the target field of hydra.lisp.syntax.FieldAccess
fieldAccessWithTarget :: Phantoms.TTerm Syntax.FieldAccess -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldAccess
fieldAccessWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "recordType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "recordType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldAccess"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.FieldDefinition
fieldDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinition name defaultValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm defaultValue)}]}))
-- | DSL accessor for the defaultValue field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionDefaultValue :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Expression)
fieldDefinitionDefaultValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
        Core.projectionFieldName = (Core.Name "defaultValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionName :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Symbol
fieldDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the defaultValue field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionWithDefaultValue :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithDefaultValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.FieldDefinition
fieldDefinitionWithName :: Phantoms.TTerm Syntax.FieldDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FieldDefinition
fieldDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FieldDefinition"),
              Core.projectionFieldName = (Core.Name "defaultValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.FloatLiteral
floatLiteral :: Phantoms.TTerm Double -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteral value precision =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Phantoms.unTTerm precision)}]}))
-- | DSL accessor for the precision field of hydra.lisp.syntax.FloatLiteral
floatLiteralPrecision :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm (Maybe String)
floatLiteralPrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
        Core.projectionFieldName = (Core.Name "precision")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.FloatLiteral
floatLiteralValue :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Double
floatLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the precision field of hydra.lisp.syntax.FloatLiteral
floatLiteralWithPrecision :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteralWithPrecision original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.FloatLiteral
floatLiteralWithValue :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatLiteral
floatLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FloatLiteral"),
              Core.projectionFieldName = (Core.Name "precision")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.FunctionDefinition
functionDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm [Syntax.TypeHint] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinition name params restParam doc typeHints body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Phantoms.unTTerm typeHints)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionBody :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Expression]
functionDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionDoc :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
functionDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionName :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.Symbol
functionDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionParams :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Symbol]
functionDefinitionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the restParam field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionRestParam :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol)
functionDefinitionRestParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "restParam")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeHints field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionTypeHints :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.TypeHint]
functionDefinitionTypeHints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "typeHints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithBody :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the doc field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithDoc :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithName :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the params field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithParams :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the restParam field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithRestParam :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithRestParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "typeHints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeHints field of hydra.lisp.syntax.FunctionDefinition
functionDefinitionWithTypeHints :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm [Syntax.TypeHint] -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithTypeHints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeHints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.IfExpression
ifExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfExpression
ifExpression condition then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the condition field of hydra.lisp.syntax.IfExpression
ifExpressionCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.lisp.syntax.IfExpression
ifExpressionElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
ifExpressionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the then field of hydra.lisp.syntax.IfExpression
ifExpressionThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the condition field of hydra.lisp.syntax.IfExpression
ifExpressionWithCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.lisp.syntax.IfExpression
ifExpressionWithElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the then field of hydra.lisp.syntax.IfExpression
ifExpressionWithThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.ImportDeclaration
importDeclaration :: Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclaration module_ spec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm spec)}]}))
-- | DSL accessor for the module field of hydra.lisp.syntax.ImportDeclaration
importDeclarationModule :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.NamespaceName
importDeclarationModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the spec field of hydra.lisp.syntax.ImportDeclaration
importDeclarationSpec :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.ImportSpec
importDeclarationSpec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
        Core.projectionFieldName = (Core.Name "spec")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the module field of hydra.lisp.syntax.ImportDeclaration
importDeclarationWithModule :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the spec field of hydra.lisp.syntax.ImportDeclaration
importDeclarationWithSpec :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationWithSpec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ImportDeclaration"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the alias variant of hydra.lisp.syntax.ImportSpec
importSpecAlias :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.ImportSpec
importSpecAlias x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the all variant of hydra.lisp.syntax.ImportSpec
importSpecAll :: Phantoms.TTerm Syntax.ImportSpec
importSpecAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the only variant of hydra.lisp.syntax.ImportSpec
importSpecOnly :: Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.ImportSpec
importSpecOnly x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "only"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the rename variant of hydra.lisp.syntax.ImportSpec
importSpecRename :: Phantoms.TTerm [[Syntax.Symbol]] -> Phantoms.TTerm Syntax.ImportSpec
importSpecRename x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.IntegerLiteral
integerLiteral :: Phantoms.TTerm Integer -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteral value bigint =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Phantoms.unTTerm bigint)}]}))
-- | DSL accessor for the bigint field of hydra.lisp.syntax.IntegerLiteral
integerLiteralBigint :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Bool
integerLiteralBigint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
        Core.projectionFieldName = (Core.Name "bigint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.IntegerLiteral
integerLiteralValue :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer
integerLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bigint field of hydra.lisp.syntax.IntegerLiteral
integerLiteralWithBigint :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralWithBigint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.IntegerLiteral
integerLiteralWithValue :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntegerLiteral
integerLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bigint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.IntegerLiteral"),
              Core.projectionFieldName = (Core.Name "bigint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.Keyword
keyword :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Keyword
keyword name namespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.Keyword
keywordName :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm String
keywordName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.lisp.syntax.Keyword
keywordNamespace :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm (Maybe String)
keywordNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.Keyword
keywordWithName :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.Keyword
keywordWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the namespace field of hydra.lisp.syntax.Keyword
keywordWithNamespace :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Keyword
keywordWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Keyword"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.Lambda
lambda :: Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Lambda
lambda name params restParam body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.Lambda
lambdaBody :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Expression]
lambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.Lambda
lambdaName :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol)
lambdaName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.lisp.syntax.Lambda
lambdaParams :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Symbol]
lambdaParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the restParam field of hydra.lisp.syntax.Lambda
lambdaRestParam :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol)
lambdaRestParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "restParam")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.Lambda
lambdaWithBody :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Lambda
lambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.Lambda
lambdaWithName :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.Lambda
lambdaWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the params field of hydra.lisp.syntax.Lambda
lambdaWithParams :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.Lambda
lambdaWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the restParam field of hydra.lisp.syntax.Lambda
lambdaWithRestParam :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.Lambda
lambdaWithRestParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the destructuring variant of hydra.lisp.syntax.LetBinding
letBindingDestructuring :: Phantoms.TTerm Syntax.DestructuringBinding -> Phantoms.TTerm Syntax.LetBinding
letBindingDestructuring x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "destructuring"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.lisp.syntax.LetBinding
letBindingSimple :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.LetBinding
letBindingSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.LetExpression
letExpression :: Phantoms.TTerm Syntax.LetKind -> Phantoms.TTerm [Syntax.LetBinding] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.LetExpression
letExpression kind bindings body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the bindings field of hydra.lisp.syntax.LetExpression
letExpressionBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LetBinding]
letExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.lisp.syntax.LetExpression
letExpressionBody :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.Expression]
letExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the kind field of hydra.lisp.syntax.LetExpression
letExpressionKind :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.LetKind
letExpressionKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bindings field of hydra.lisp.syntax.LetExpression
letExpressionWithBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LetBinding] -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.lisp.syntax.LetExpression
letExpressionWithBody :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the kind field of hydra.lisp.syntax.LetExpression
letExpressionWithKind :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.LetKind -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the parallel variant of hydra.lisp.syntax.LetKind
letKindParallel :: Phantoms.TTerm Syntax.LetKind
letKindParallel =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parallel"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the recursive variant of hydra.lisp.syntax.LetKind
letKindRecursive :: Phantoms.TTerm Syntax.LetKind
letKindRecursive =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recursive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sequential variant of hydra.lisp.syntax.LetKind
letKindSequential :: Phantoms.TTerm Syntax.LetKind
letKindSequential =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.LetKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequential"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.ListLiteral
listLiteral :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ListLiteral
listLiteral elements quoted =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Phantoms.unTTerm quoted)}]}))
-- | DSL accessor for the elements field of hydra.lisp.syntax.ListLiteral
listLiteralElements :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm [Syntax.Expression]
listLiteralElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the quoted field of hydra.lisp.syntax.ListLiteral
listLiteralQuoted :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm Bool
listLiteralQuoted x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
        Core.projectionFieldName = (Core.Name "quoted")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the elements field of hydra.lisp.syntax.ListLiteral
listLiteralWithElements :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ListLiteral
listLiteralWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
              Core.projectionFieldName = (Core.Name "quoted")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the quoted field of hydra.lisp.syntax.ListLiteral
listLiteralWithQuoted :: Phantoms.TTerm Syntax.ListLiteral -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ListLiteral
listLiteralWithQuoted original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ListLiteral"),
              Core.projectionFieldName = (Core.Name "elements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoted"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the boolean variant of hydra.lisp.syntax.Literal
literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the character variant of hydra.lisp.syntax.Literal
literalCharacter :: Phantoms.TTerm Syntax.CharacterLiteral -> Phantoms.TTerm Syntax.Literal
literalCharacter x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "character"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the float variant of hydra.lisp.syntax.Literal
literalFloat :: Phantoms.TTerm Syntax.FloatLiteral -> Phantoms.TTerm Syntax.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.lisp.syntax.Literal
literalInteger :: Phantoms.TTerm Syntax.IntegerLiteral -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the keyword variant of hydra.lisp.syntax.Literal
literalKeyword :: Phantoms.TTerm Syntax.Keyword -> Phantoms.TTerm Syntax.Literal
literalKeyword x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keyword"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nil variant of hydra.lisp.syntax.Literal
literalNil :: Phantoms.TTerm Syntax.Literal
literalNil =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.LiteralPattern
literalPattern :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.LiteralPattern
literalPattern value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LiteralPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the value field of hydra.lisp.syntax.LiteralPattern
literalPatternValue :: Phantoms.TTerm Syntax.LiteralPattern -> Phantoms.TTerm Syntax.Literal
literalPatternValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.LiteralPattern"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the value field of hydra.lisp.syntax.LiteralPattern
literalPatternWithValue :: Phantoms.TTerm Syntax.LiteralPattern -> Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.LiteralPattern
literalPatternWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.LiteralPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the string variant of hydra.lisp.syntax.Literal
literalString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the symbol variant of hydra.lisp.syntax.Literal
literalSymbol :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Literal
literalSymbol x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbol"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.MacroDefinition
macroDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinition name params restParam body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm restParam)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.MacroDefinition
macroDefinitionBody :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Expression]
macroDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.MacroDefinition
macroDefinitionName :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm Syntax.Symbol
macroDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.lisp.syntax.MacroDefinition
macroDefinitionParams :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Symbol]
macroDefinitionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the restParam field of hydra.lisp.syntax.MacroDefinition
macroDefinitionRestParam :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol)
macroDefinitionRestParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
        Core.projectionFieldName = (Core.Name "restParam")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithBody :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithName :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the params field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithParams :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm [Syntax.Symbol] -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "restParam")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the restParam field of hydra.lisp.syntax.MacroDefinition
macroDefinitionWithRestParam :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm (Maybe Syntax.Symbol) -> Phantoms.TTerm Syntax.MacroDefinition
macroDefinitionWithRestParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restParam"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MacroDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.MapEntry
mapEntry :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntry key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the key field of hydra.lisp.syntax.MapEntry
mapEntryKey :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression
mapEntryKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.MapEntry
mapEntryValue :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression
mapEntryValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.lisp.syntax.MapEntry
mapEntryWithKey :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntryWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.MapEntry
mapEntryWithValue :: Phantoms.TTerm Syntax.MapEntry -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MapEntry
mapEntryWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapEntry"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.MapLiteral
mapLiteral :: Phantoms.TTerm [Syntax.MapEntry] -> Phantoms.TTerm Syntax.MapLiteral
mapLiteral entries =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Phantoms.unTTerm entries)}]}))
-- | DSL accessor for the entries field of hydra.lisp.syntax.MapLiteral
mapLiteralEntries :: Phantoms.TTerm Syntax.MapLiteral -> Phantoms.TTerm [Syntax.MapEntry]
mapLiteralEntries x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.MapLiteral"),
        Core.projectionFieldName = (Core.Name "entries")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the entries field of hydra.lisp.syntax.MapLiteral
mapLiteralWithEntries :: Phantoms.TTerm Syntax.MapLiteral -> Phantoms.TTerm [Syntax.MapEntry] -> Phantoms.TTerm Syntax.MapLiteral
mapLiteralWithEntries original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.MapLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "entries"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.ModuleDeclaration
moduleDeclaration :: Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclaration name doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationDoc :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm (Maybe Syntax.Docstring)
moduleDeclarationDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationName :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.NamespaceName
moduleDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationWithDoc :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.ModuleDeclaration
moduleDeclarationWithName :: Phantoms.TTerm Syntax.ModuleDeclaration -> Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm Syntax.ModuleDeclaration
moduleDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.ModuleDeclaration"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.lisp.syntax.NamespaceName wrapper
namespaceName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.NamespaceName
namespaceName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.lisp.syntax.NamespaceName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the emptyList variant of hydra.lisp.syntax.NilStyle
nilStyleEmptyList :: Phantoms.TTerm Syntax.NilStyle
nilStyleEmptyList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.NilStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "emptyList"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nil variant of hydra.lisp.syntax.NilStyle
nilStyleNil :: Phantoms.TTerm Syntax.NilStyle
nilStyleNil =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.NilStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nil"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.lisp.syntax.NotExpression
notExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.NotExpression
notExpression expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.lisp.syntax.NotExpression
notExpressionExpression :: Phantoms.TTerm Syntax.NotExpression -> Phantoms.TTerm Syntax.Expression
notExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.NotExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.lisp.syntax.NotExpression
notExpressionWithExpression :: Phantoms.TTerm Syntax.NotExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.NotExpression
notExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.OrExpression
orExpression :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.OrExpression
orExpression expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.OrExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.lisp.syntax.OrExpression
orExpressionExpressions :: Phantoms.TTerm Syntax.OrExpression -> Phantoms.TTerm [Syntax.Expression]
orExpressionExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.OrExpression"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expressions field of hydra.lisp.syntax.OrExpression
orExpressionWithExpressions :: Phantoms.TTerm Syntax.OrExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.OrExpression
orExpressionWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.OrExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the constructor variant of hydra.lisp.syntax.Pattern
patternConstructor :: Phantoms.TTerm Syntax.ConstructorPattern -> Phantoms.TTerm Syntax.Pattern
patternConstructor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.lisp.syntax.Pattern
patternLiteral :: Phantoms.TTerm Syntax.LiteralPattern -> Phantoms.TTerm Syntax.Pattern
patternLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.lisp.syntax.Pattern
patternVariable :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Pattern
patternVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.lisp.syntax.Pattern
patternWildcard :: Phantoms.TTerm Syntax.WildcardPattern -> Phantoms.TTerm Syntax.Pattern
patternWildcard x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.Program
program :: Phantoms.TTerm Syntax.Dialect -> Phantoms.TTerm (Maybe Syntax.ModuleDeclaration) -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm [Syntax.ExportDeclaration] -> Phantoms.TTerm [Syntax.TopLevelFormWithComments] -> Phantoms.TTerm Syntax.Program
program dialect module_ imports exports forms =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Phantoms.unTTerm dialect)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm exports)},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Phantoms.unTTerm forms)}]}))
-- | DSL accessor for the dialect field of hydra.lisp.syntax.Program
programDialect :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm Syntax.Dialect
programDialect x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "dialect")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the exports field of hydra.lisp.syntax.Program
programExports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ExportDeclaration]
programExports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "exports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the forms field of hydra.lisp.syntax.Program
programForms :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.TopLevelFormWithComments]
programForms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "forms")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the imports field of hydra.lisp.syntax.Program
programImports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ImportDeclaration]
programImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the module field of hydra.lisp.syntax.Program
programModule :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm (Maybe Syntax.ModuleDeclaration)
programModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dialect field of hydra.lisp.syntax.Program
programWithDialect :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm Syntax.Dialect -> Phantoms.TTerm Syntax.Program
programWithDialect original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the exports field of hydra.lisp.syntax.Program
programWithExports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ExportDeclaration] -> Phantoms.TTerm Syntax.Program
programWithExports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the forms field of hydra.lisp.syntax.Program
programWithForms :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.TopLevelFormWithComments] -> Phantoms.TTerm Syntax.Program
programWithForms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the imports field of hydra.lisp.syntax.Program
programWithImports :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ImportDeclaration] -> Phantoms.TTerm Syntax.Program
programWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the module field of hydra.lisp.syntax.Program
programWithModule :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm (Maybe Syntax.ModuleDeclaration) -> Phantoms.TTerm Syntax.Program
programWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dialect"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "dialect")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.Program"),
              Core.projectionFieldName = (Core.Name "forms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbol :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedSymbol
qualifiedSymbol namespace name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolName :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String
qualifiedSymbolName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolNamespace :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String
qualifiedSymbolNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolWithName :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedSymbol
qualifiedSymbolWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the namespace field of hydra.lisp.syntax.QualifiedSymbol
qualifiedSymbolWithNamespace :: Phantoms.TTerm Syntax.QualifiedSymbol -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.QualifiedSymbol
qualifiedSymbolWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QualifiedSymbol"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.QuasiquoteExpression
quasiquoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuasiquoteExpression
quasiquoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuasiquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.QuasiquoteExpression
quasiquoteExpressionBody :: Phantoms.TTerm Syntax.QuasiquoteExpression -> Phantoms.TTerm Syntax.Expression
quasiquoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QuasiquoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.QuasiquoteExpression
quasiquoteExpressionWithBody :: Phantoms.TTerm Syntax.QuasiquoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuasiquoteExpression
quasiquoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuasiquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.QuoteExpression
quoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuoteExpression
quoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.QuoteExpression
quoteExpressionBody :: Phantoms.TTerm Syntax.QuoteExpression -> Phantoms.TTerm Syntax.Expression
quoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.QuoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.QuoteExpression
quoteExpressionWithBody :: Phantoms.TTerm Syntax.QuoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.QuoteExpression
quoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.QuoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm [Syntax.FieldDefinition] -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinition name fields doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionDoc :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
recordTypeDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the fields field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionFields :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm [Syntax.FieldDefinition]
recordTypeDefinitionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionName :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm Syntax.Symbol
recordTypeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionWithDoc :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the fields field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionWithFields :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm [Syntax.FieldDefinition] -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.RecordTypeDefinition
recordTypeDefinitionWithName :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.RecordTypeDefinition
recordTypeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.RecordTypeDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the atom variant of hydra.lisp.syntax.SExpression
sExpressionAtom :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.SExpression
sExpressionAtom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.SExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.lisp.syntax.SExpression
sExpressionList :: Phantoms.TTerm [Syntax.SExpression] -> Phantoms.TTerm Syntax.SExpression
sExpressionList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.SExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.SetLiteral
setLiteral :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.SetLiteral
setLiteral elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SetLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))
-- | DSL accessor for the elements field of hydra.lisp.syntax.SetLiteral
setLiteralElements :: Phantoms.TTerm Syntax.SetLiteral -> Phantoms.TTerm [Syntax.Expression]
setLiteralElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SetLiteral"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the elements field of hydra.lisp.syntax.SetLiteral
setLiteralWithElements :: Phantoms.TTerm Syntax.SetLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.SetLiteral
setLiteralWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SetLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.SimpleBinding
simpleBinding :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SimpleBinding
simpleBinding name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.SimpleBinding
simpleBindingName :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Symbol
simpleBindingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.SimpleBinding
simpleBindingValue :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Expression
simpleBindingValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.SimpleBinding
simpleBindingWithName :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.SimpleBinding
simpleBindingWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.SimpleBinding
simpleBindingWithValue :: Phantoms.TTerm Syntax.SimpleBinding -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SimpleBinding
simpleBindingWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SimpleBinding"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.SplicingUnquoteExpression
splicingUnquoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SplicingUnquoteExpression
splicingUnquoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.SplicingUnquoteExpression
splicingUnquoteExpressionBody :: Phantoms.TTerm Syntax.SplicingUnquoteExpression -> Phantoms.TTerm Syntax.Expression
splicingUnquoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.SplicingUnquoteExpression
splicingUnquoteExpressionWithBody :: Phantoms.TTerm Syntax.SplicingUnquoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SplicingUnquoteExpression
splicingUnquoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.lisp.syntax.Symbol wrapper
symbol :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Symbol
symbol x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.lisp.syntax.Symbol"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the constant variant of hydra.lisp.syntax.TopLevelForm
topLevelFormConstant :: Phantoms.TTerm Syntax.ConstantDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormConstant x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.lisp.syntax.TopLevelForm
topLevelFormExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.lisp.syntax.TopLevelForm
topLevelFormFunction :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the macro variant of hydra.lisp.syntax.TopLevelForm
topLevelFormMacro :: Phantoms.TTerm Syntax.MacroDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormMacro x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "macro"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the recordType variant of hydra.lisp.syntax.TopLevelForm
topLevelFormRecordType :: Phantoms.TTerm Syntax.RecordTypeDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormRecordType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recordType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.lisp.syntax.TopLevelForm
topLevelFormVariable :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelForm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithComments :: Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm (Maybe Syntax.Comment) -> Phantoms.TTerm Syntax.TopLevelForm -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithComments doc comment form =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Phantoms.unTTerm form)}]}))
-- | DSL accessor for the comment field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsComment :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Comment)
topLevelFormWithCommentsComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsDoc :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Docstring)
topLevelFormWithCommentsDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the form field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsForm :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm Syntax.TopLevelForm
topLevelFormWithCommentsForm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
        Core.projectionFieldName = (Core.Name "form")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comment field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithComment :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Comment) -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "form")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the doc field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithDoc :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "form")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the form field of hydra.lisp.syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithForm :: Phantoms.TTerm Syntax.TopLevelFormWithComments -> Phantoms.TTerm Syntax.TopLevelForm -> Phantoms.TTerm Syntax.TopLevelFormWithComments
topLevelFormWithCommentsWithForm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "form"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.TypeAnnotation
typeAnnotation :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotation expression type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the expression field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationExpression :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.Expression
typeAnnotationExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationType :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.TypeSpecifier
typeAnnotationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationWithExpression :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotationWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.lisp.syntax.TypeAnnotation
typeAnnotationWithType :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeAnnotation"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.TypeHint
typeHint :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeHint
typeHint name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the name field of hydra.lisp.syntax.TypeHint
typeHintName :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.Symbol
typeHintName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.lisp.syntax.TypeHint
typeHintType :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.TypeSpecifier
typeHintType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.lisp.syntax.TypeHint
typeHintWithName :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.TypeHint
typeHintWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.lisp.syntax.TypeHint
typeHintWithType :: Phantoms.TTerm Syntax.TypeHint -> Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeHint
typeHintWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.TypeHint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the either variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierEither :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierEither x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "either"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierFunction :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierList :: Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the map variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierMap :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the maybe variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierMaybe :: Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierMaybe x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maybe"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the named variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierNamed :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pair variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierPair :: Phantoms.TTerm [Syntax.TypeSpecifier] -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierPair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the set variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierSet :: Phantoms.TTerm Syntax.TypeSpecifier -> Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unit variant of hydra.lisp.syntax.TypeSpecifier
typeSpecifierUnit :: Phantoms.TTerm Syntax.TypeSpecifier
typeSpecifierUnit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.lisp.syntax.TypeSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unit"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the body of hydra.lisp.syntax.Docstring
unDocstring :: Phantoms.TTerm Syntax.Docstring -> Phantoms.TTerm String
unDocstring x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.lisp.syntax.Docstring")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.lisp.syntax.NamespaceName
unNamespaceName :: Phantoms.TTerm Syntax.NamespaceName -> Phantoms.TTerm String
unNamespaceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.lisp.syntax.NamespaceName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.lisp.syntax.Symbol
unSymbol :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm String
unSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.lisp.syntax.Symbol")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.lisp.syntax.UnquoteExpression
unquoteExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnquoteExpression
unquoteExpression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.UnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.lisp.syntax.UnquoteExpression
unquoteExpressionBody :: Phantoms.TTerm Syntax.UnquoteExpression -> Phantoms.TTerm Syntax.Expression
unquoteExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.UnquoteExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.lisp.syntax.UnquoteExpression
unquoteExpressionWithBody :: Phantoms.TTerm Syntax.UnquoteExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnquoteExpression
unquoteExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.UnquoteExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.VariableDefinition
variableDefinition :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinition name value doc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm doc)}]}))
-- | DSL accessor for the doc field of hydra.lisp.syntax.VariableDefinition
variableDefinitionDoc :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring)
variableDefinitionDoc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
        Core.projectionFieldName = (Core.Name "doc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.VariableDefinition
variableDefinitionName :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Symbol
variableDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.lisp.syntax.VariableDefinition
variableDefinitionValue :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Expression
variableDefinitionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the doc field of hydra.lisp.syntax.VariableDefinition
variableDefinitionWithDoc :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm (Maybe Syntax.Docstring) -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinitionWithDoc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.VariableDefinition
variableDefinitionWithName :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.lisp.syntax.VariableDefinition
variableDefinitionWithValue :: Phantoms.TTerm Syntax.VariableDefinition -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.VariableDefinition
variableDefinitionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableDefinition"),
              Core.projectionFieldName = (Core.Name "doc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.VariableReference
variableReference :: Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.VariableReference
variableReference name functionNamespace =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm functionNamespace)}]}))
-- | DSL accessor for the functionNamespace field of hydra.lisp.syntax.VariableReference
variableReferenceFunctionNamespace :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Bool
variableReferenceFunctionNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
        Core.projectionFieldName = (Core.Name "functionNamespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.lisp.syntax.VariableReference
variableReferenceName :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Syntax.Symbol
variableReferenceName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the functionNamespace field of hydra.lisp.syntax.VariableReference
variableReferenceWithFunctionNamespace :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.VariableReference
variableReferenceWithFunctionNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.lisp.syntax.VariableReference
variableReferenceWithName :: Phantoms.TTerm Syntax.VariableReference -> Phantoms.TTerm Syntax.Symbol -> Phantoms.TTerm Syntax.VariableReference
variableReferenceWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "functionNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VariableReference"),
              Core.projectionFieldName = (Core.Name "functionNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.lisp.syntax.VectorLiteral
vectorLiteral :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VectorLiteral
vectorLiteral elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VectorLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))
-- | DSL accessor for the elements field of hydra.lisp.syntax.VectorLiteral
vectorLiteralElements :: Phantoms.TTerm Syntax.VectorLiteral -> Phantoms.TTerm [Syntax.Expression]
vectorLiteralElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.lisp.syntax.VectorLiteral"),
        Core.projectionFieldName = (Core.Name "elements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the elements field of hydra.lisp.syntax.VectorLiteral
vectorLiteralWithElements :: Phantoms.TTerm Syntax.VectorLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VectorLiteral
vectorLiteralWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.VectorLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.lisp.syntax.WildcardPattern
wildcardPattern :: Phantoms.TTerm Syntax.WildcardPattern
wildcardPattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.lisp.syntax.WildcardPattern"),
      Core.recordFields = []}))
