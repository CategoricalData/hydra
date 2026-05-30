-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.python.syntax

module Hydra.Dsl.Python.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Python.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the star variant of hydra.python.syntax.AnnotatedRhs
annotatedRhsStar :: Typed.TypedTerm [Syntax.StarExpression] -> Typed.TypedTerm Syntax.AnnotatedRhs
annotatedRhsStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.python.syntax.AnnotatedRhs
annotatedRhsYield :: Typed.TypedTerm Syntax.YieldExpression -> Typed.TypedTerm Syntax.AnnotatedRhs
annotatedRhsYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.AnnotatedStatement
annotatedStatement :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.AnnotatedStatement
annotatedStatement comment statement =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm statement)}]}))
-- | DSL accessor for the comment field of hydra.python.syntax.AnnotatedStatement
annotatedStatementComment :: Typed.TypedTerm Syntax.AnnotatedStatement -> Typed.TypedTerm String
annotatedStatementComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.python.syntax.AnnotatedStatement
annotatedStatementStatement :: Typed.TypedTerm Syntax.AnnotatedStatement -> Typed.TypedTerm Syntax.Statement
annotatedStatementStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comment field of hydra.python.syntax.AnnotatedStatement
annotatedStatementWithComment :: Typed.TypedTerm Syntax.AnnotatedStatement -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.AnnotatedStatement
annotatedStatementWithComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
              Core.projectionFieldName = (Core.Name "statement")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.python.syntax.AnnotatedStatement
annotatedStatementWithStatement :: Typed.TypedTerm Syntax.AnnotatedStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.AnnotatedStatement
annotatedStatementWithStatement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.Annotation wrapper
annotation :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Annotation
annotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Annotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Args
args :: Typed.TypedTerm [Syntax.PosArg] -> Typed.TypedTerm [Syntax.KwargOrStarred] -> Typed.TypedTerm [Syntax.KwargOrDoubleStarred] -> Typed.TypedTerm Syntax.Args
args positional kwargOrStarred kwargOrDoubleStarred =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Typed.unTypedTerm positional)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Typed.unTypedTerm kwargOrStarred)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Typed.unTypedTerm kwargOrDoubleStarred)}]}))
-- | DSL accessor for the kwargOrDoubleStarred field of hydra.python.syntax.Args
argsKwargOrDoubleStarred :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm [Syntax.KwargOrDoubleStarred]
argsKwargOrDoubleStarred x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionFieldName = (Core.Name "kwargOrDoubleStarred")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kwargOrStarred field of hydra.python.syntax.Args
argsKwargOrStarred :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm [Syntax.KwargOrStarred]
argsKwargOrStarred x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionFieldName = (Core.Name "kwargOrStarred")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the positional field of hydra.python.syntax.Args
argsPositional :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm [Syntax.PosArg]
argsPositional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionFieldName = (Core.Name "positional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the kwargOrDoubleStarred field of hydra.python.syntax.Args
argsWithKwargOrDoubleStarred :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm [Syntax.KwargOrDoubleStarred] -> Typed.TypedTerm Syntax.Args
argsWithKwargOrDoubleStarred original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionFieldName = (Core.Name "positional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionFieldName = (Core.Name "kwargOrStarred")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the kwargOrStarred field of hydra.python.syntax.Args
argsWithKwargOrStarred :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm [Syntax.KwargOrStarred] -> Typed.TypedTerm Syntax.Args
argsWithKwargOrStarred original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionFieldName = (Core.Name "positional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionFieldName = (Core.Name "kwargOrDoubleStarred")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the positional field of hydra.python.syntax.Args
argsWithPositional :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm [Syntax.PosArg] -> Typed.TypedTerm Syntax.Args
argsWithPositional original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionFieldName = (Core.Name "kwargOrStarred")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionFieldName = (Core.Name "kwargOrDoubleStarred")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.AsPattern
asPattern :: Typed.TypedTerm Syntax.OrPattern -> Typed.TypedTerm Syntax.PatternCaptureTarget -> Typed.TypedTerm Syntax.AsPattern
asPattern pattern as =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.AsPattern
asPatternAs :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.PatternCaptureTarget
asPatternAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.python.syntax.AsPattern
asPatternPattern :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.OrPattern
asPatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.AsPattern
asPatternWithAs :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.PatternCaptureTarget -> Typed.TypedTerm Syntax.AsPattern
asPatternWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.python.syntax.AsPattern
asPatternWithPattern :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.OrPattern -> Typed.TypedTerm Syntax.AsPattern
asPatternWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.AssertStatement
assertStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.AssertStatement
assertStatement expression1 expression2 =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression1"),
          Core.fieldTerm = (Typed.unTypedTerm expression1)},
        Core.Field {
          Core.fieldName = (Core.Name "expression2"),
          Core.fieldTerm = (Typed.unTypedTerm expression2)}]}))
-- | DSL accessor for the expression1 field of hydra.python.syntax.AssertStatement
assertStatementExpression1 :: Typed.TypedTerm Syntax.AssertStatement -> Typed.TypedTerm Syntax.Expression
assertStatementExpression1 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
        Core.projectionFieldName = (Core.Name "expression1")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression2 field of hydra.python.syntax.AssertStatement
assertStatementExpression2 :: Typed.TypedTerm Syntax.AssertStatement -> Typed.TypedTerm (Maybe Syntax.Expression)
assertStatementExpression2 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
        Core.projectionFieldName = (Core.Name "expression2")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression1 field of hydra.python.syntax.AssertStatement
assertStatementWithExpression1 :: Typed.TypedTerm Syntax.AssertStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssertStatement
assertStatementWithExpression1 original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression1"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
              Core.projectionFieldName = (Core.Name "expression2")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the expression2 field of hydra.python.syntax.AssertStatement
assertStatementWithExpression2 :: Typed.TypedTerm Syntax.AssertStatement -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.AssertStatement
assertStatementWithExpression2 original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
              Core.projectionFieldName = (Core.Name "expression1")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression2"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the aug variant of hydra.python.syntax.Assignment
assignmentAug :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.Assignment
assignmentAug x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aug"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.AssignmentExpression
assignmentExpression :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpression name expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.AssignmentExpression
assignmentExpressionExpression :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Expression
assignmentExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.AssignmentExpression
assignmentExpressionName :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Name
assignmentExpressionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.AssignmentExpression
assignmentExpressionWithExpression :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.AssignmentExpression
assignmentExpressionWithName :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the typed variant of hydra.python.syntax.Assignment
assignmentTyped :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm Syntax.Assignment
assignmentTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untyped variant of hydra.python.syntax.Assignment
assignmentUntyped :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm Syntax.Assignment
assignmentUntyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untyped"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dict variant of hydra.python.syntax.Atom
atomDict :: Typed.TypedTerm Syntax.Dict -> Typed.TypedTerm Syntax.Atom
atomDict x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dict"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dictcomp variant of hydra.python.syntax.Atom
atomDictcomp :: Typed.TypedTerm Syntax.Dictcomp -> Typed.TypedTerm Syntax.Atom
atomDictcomp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dictcomp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ellipsis variant of hydra.python.syntax.Atom
atomEllipsis :: Typed.TypedTerm Syntax.Atom
atomEllipsis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ellipsis"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the false variant of hydra.python.syntax.Atom
atomFalse :: Typed.TypedTerm Syntax.Atom
atomFalse =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "false"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the genexp variant of hydra.python.syntax.Atom
atomGenexp :: Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.Atom
atomGenexp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genexp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the group variant of hydra.python.syntax.Atom
atomGroup :: Typed.TypedTerm Syntax.Group -> Typed.TypedTerm Syntax.Atom
atomGroup x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.python.syntax.Atom
atomList :: Typed.TypedTerm Syntax.List -> Typed.TypedTerm Syntax.Atom
atomList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the listcomp variant of hydra.python.syntax.Atom
atomListcomp :: Typed.TypedTerm Syntax.Listcomp -> Typed.TypedTerm Syntax.Atom
atomListcomp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listcomp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.Atom
atomName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Atom
atomName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.python.syntax.Atom
atomNone :: Typed.TypedTerm Syntax.Atom
atomNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.Atom
atomNumber :: Typed.TypedTerm Syntax.Number -> Typed.TypedTerm Syntax.Atom
atomNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.python.syntax.Atom
atomSet :: Typed.TypedTerm Syntax.Set -> Typed.TypedTerm Syntax.Atom
atomSet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the setcomp variant of hydra.python.syntax.Atom
atomSetcomp :: Typed.TypedTerm Syntax.Setcomp -> Typed.TypedTerm Syntax.Atom
atomSetcomp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setcomp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.python.syntax.Atom
atomString :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.Atom
atomString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the true variant of hydra.python.syntax.Atom
atomTrue :: Typed.TypedTerm Syntax.Atom
atomTrue =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "true"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.Atom
atomTuple :: Typed.TypedTerm Syntax.Tuple -> Typed.TypedTerm Syntax.Atom
atomTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Attribute wrapper
attribute :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.Attribute
attribute x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Attribute"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the ampersandEqual variant of hydra.python.syntax.AugAssign
augAssignAmpersandEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignAmpersandEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ampersandEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the atEqual variant of hydra.python.syntax.AugAssign
augAssignAtEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignAtEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the barEqual variant of hydra.python.syntax.AugAssign
augAssignBarEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignBarEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "barEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the caretEqual variant of hydra.python.syntax.AugAssign
augAssignCaretEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignCaretEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caretEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the doubleSlashEqual variant of hydra.python.syntax.AugAssign
augAssignDoubleSlashEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignDoubleSlashEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleSlashEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShiftEqual variant of hydra.python.syntax.AugAssign
augAssignLeftShiftEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignLeftShiftEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShiftEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minusEqual variant of hydra.python.syntax.AugAssign
augAssignMinusEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignMinusEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minusEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the percentEqual variant of hydra.python.syntax.AugAssign
augAssignPercentEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignPercentEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "percentEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plusEqual variant of hydra.python.syntax.AugAssign
augAssignPlusEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignPlusEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plusEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShiftEqual variant of hydra.python.syntax.AugAssign
augAssignRightShiftEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignRightShiftEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShiftEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the slashEqual variant of hydra.python.syntax.AugAssign
augAssignSlashEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignSlashEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the starStarEqual variant of hydra.python.syntax.AugAssign
augAssignStarStarEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignStarStarEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starStarEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the timesEqual variant of hydra.python.syntax.AugAssign
augAssignTimesEqual :: Typed.TypedTerm Syntax.AugAssign
augAssignTimesEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "timesEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.AugAssignment
augAssignment :: Typed.TypedTerm Syntax.SingleTarget -> Typed.TypedTerm Syntax.AugAssign -> Typed.TypedTerm Syntax.AnnotatedRhs -> Typed.TypedTerm Syntax.AugAssignment
augAssignment lhs augassign rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Typed.unTypedTerm augassign)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the augassign field of hydra.python.syntax.AugAssignment
augAssignmentAugassign :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.AugAssign
augAssignmentAugassign x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionFieldName = (Core.Name "augassign")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lhs field of hydra.python.syntax.AugAssignment
augAssignmentLhs :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.SingleTarget
augAssignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.AugAssignment
augAssignmentRhs :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.AnnotatedRhs
augAssignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the augassign field of hydra.python.syntax.AugAssignment
augAssignmentWithAugassign :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.AugAssign -> Typed.TypedTerm Syntax.AugAssignment
augAssignmentWithAugassign original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the lhs field of hydra.python.syntax.AugAssignment
augAssignmentWithLhs :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.SingleTarget -> Typed.TypedTerm Syntax.AugAssignment
augAssignmentWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionFieldName = (Core.Name "augassign")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.AugAssignment
augAssignmentWithRhs :: Typed.TypedTerm Syntax.AugAssignment -> Typed.TypedTerm Syntax.AnnotatedRhs -> Typed.TypedTerm Syntax.AugAssignment
augAssignmentWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionFieldName = (Core.Name "augassign")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.AwaitPrimary
awaitPrimary :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.AwaitPrimary
awaitPrimary await primary =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Typed.unTypedTerm await)},
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)}]}))
-- | DSL accessor for the await field of hydra.python.syntax.AwaitPrimary
awaitPrimaryAwait :: Typed.TypedTerm Syntax.AwaitPrimary -> Typed.TypedTerm Bool
awaitPrimaryAwait x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
        Core.projectionFieldName = (Core.Name "await")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.AwaitPrimary
awaitPrimaryPrimary :: Typed.TypedTerm Syntax.AwaitPrimary -> Typed.TypedTerm Syntax.Primary
awaitPrimaryPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the await field of hydra.python.syntax.AwaitPrimary
awaitPrimaryWithAwait :: Typed.TypedTerm Syntax.AwaitPrimary -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.AwaitPrimary
awaitPrimaryWithAwait original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the primary field of hydra.python.syntax.AwaitPrimary
awaitPrimaryWithPrimary :: Typed.TypedTerm Syntax.AwaitPrimary -> Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.AwaitPrimary
awaitPrimaryWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
              Core.projectionFieldName = (Core.Name "await")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.BitwiseAnd
bitwiseAnd :: Typed.TypedTerm (Maybe Syntax.BitwiseAnd) -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.BitwiseAnd
bitwiseAnd lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndLhs :: Typed.TypedTerm Syntax.BitwiseAnd -> Typed.TypedTerm (Maybe Syntax.BitwiseAnd)
bitwiseAndLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndRhs :: Typed.TypedTerm Syntax.BitwiseAnd -> Typed.TypedTerm Syntax.ShiftExpression
bitwiseAndRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndWithLhs :: Typed.TypedTerm Syntax.BitwiseAnd -> Typed.TypedTerm (Maybe Syntax.BitwiseAnd) -> Typed.TypedTerm Syntax.BitwiseAnd
bitwiseAndWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndWithRhs :: Typed.TypedTerm Syntax.BitwiseAnd -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.BitwiseAnd
bitwiseAndWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.BitwiseOr
bitwiseOr :: Typed.TypedTerm (Maybe Syntax.BitwiseOr) -> Typed.TypedTerm Syntax.BitwiseXor -> Typed.TypedTerm Syntax.BitwiseOr
bitwiseOr lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.BitwiseOr
bitwiseOrLhs :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm (Maybe Syntax.BitwiseOr)
bitwiseOrLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseOr
bitwiseOrRhs :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.BitwiseXor
bitwiseOrRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseOr
bitwiseOrWithLhs :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm (Maybe Syntax.BitwiseOr) -> Typed.TypedTerm Syntax.BitwiseOr
bitwiseOrWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.BitwiseOr
bitwiseOrWithRhs :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.BitwiseXor -> Typed.TypedTerm Syntax.BitwiseOr
bitwiseOrWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.BitwiseXor
bitwiseXor :: Typed.TypedTerm (Maybe Syntax.BitwiseXor) -> Typed.TypedTerm Syntax.BitwiseAnd -> Typed.TypedTerm Syntax.BitwiseXor
bitwiseXor lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.BitwiseXor
bitwiseXorLhs :: Typed.TypedTerm Syntax.BitwiseXor -> Typed.TypedTerm (Maybe Syntax.BitwiseXor)
bitwiseXorLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseXor
bitwiseXorRhs :: Typed.TypedTerm Syntax.BitwiseXor -> Typed.TypedTerm Syntax.BitwiseAnd
bitwiseXorRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseXor
bitwiseXorWithLhs :: Typed.TypedTerm Syntax.BitwiseXor -> Typed.TypedTerm (Maybe Syntax.BitwiseXor) -> Typed.TypedTerm Syntax.BitwiseXor
bitwiseXorWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.BitwiseXor
bitwiseXorWithRhs :: Typed.TypedTerm Syntax.BitwiseXor -> Typed.TypedTerm Syntax.BitwiseAnd -> Typed.TypedTerm Syntax.BitwiseXor
bitwiseXorWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the indented variant of hydra.python.syntax.Block
blockIndented :: Typed.TypedTerm [[Syntax.Statement]] -> Typed.TypedTerm Syntax.Block
blockIndented x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Block"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indented"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Block
blockSimple :: Typed.TypedTerm [Syntax.SimpleStatement] -> Typed.TypedTerm Syntax.Block
blockSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Block"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.CapturePattern wrapper
capturePattern :: Typed.TypedTerm Syntax.PatternCaptureTarget -> Typed.TypedTerm Syntax.CapturePattern
capturePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.CapturePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.CaseBlock
caseBlock :: Typed.TypedTerm Syntax.Patterns -> Typed.TypedTerm (Maybe Syntax.Guard) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.CaseBlock
caseBlock patterns guard body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm patterns)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Typed.unTypedTerm guard)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.CaseBlock
caseBlockBody :: Typed.TypedTerm Syntax.CaseBlock -> Typed.TypedTerm Syntax.Block
caseBlockBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the guard field of hydra.python.syntax.CaseBlock
caseBlockGuard :: Typed.TypedTerm Syntax.CaseBlock -> Typed.TypedTerm (Maybe Syntax.Guard)
caseBlockGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionFieldName = (Core.Name "guard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the patterns field of hydra.python.syntax.CaseBlock
caseBlockPatterns :: Typed.TypedTerm Syntax.CaseBlock -> Typed.TypedTerm Syntax.Patterns
caseBlockPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.CaseBlock
caseBlockWithBody :: Typed.TypedTerm Syntax.CaseBlock -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.CaseBlock
caseBlockWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the guard field of hydra.python.syntax.CaseBlock
caseBlockWithGuard :: Typed.TypedTerm Syntax.CaseBlock -> Typed.TypedTerm (Maybe Syntax.Guard) -> Typed.TypedTerm Syntax.CaseBlock
caseBlockWithGuard original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionFieldName = (Core.Name "patterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the patterns field of hydra.python.syntax.CaseBlock
caseBlockWithPatterns :: Typed.TypedTerm Syntax.CaseBlock -> Typed.TypedTerm Syntax.Patterns -> Typed.TypedTerm Syntax.CaseBlock
caseBlockWithPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ClassDefinition
classDefinition :: Typed.TypedTerm (Maybe Syntax.Decorators) -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm (Maybe Syntax.Args) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ClassDefinition
classDefinition decorators name typeParams arguments body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Typed.unTypedTerm decorators)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the arguments field of hydra.python.syntax.ClassDefinition
classDefinitionArguments :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm (Maybe Syntax.Args)
classDefinitionArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ClassDefinition
classDefinitionBody :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm Syntax.Block
classDefinitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the decorators field of hydra.python.syntax.ClassDefinition
classDefinitionDecorators :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm (Maybe Syntax.Decorators)
classDefinitionDecorators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "decorators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ClassDefinition
classDefinitionName :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm Syntax.Name
classDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.ClassDefinition
classDefinitionTypeParams :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm [Syntax.TypeParameter]
classDefinitionTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.python.syntax.ClassDefinition
classDefinitionWithArguments :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm (Maybe Syntax.Args) -> Typed.TypedTerm Syntax.ClassDefinition
classDefinitionWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "decorators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.ClassDefinition
classDefinitionWithBody :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ClassDefinition
classDefinitionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "decorators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the decorators field of hydra.python.syntax.ClassDefinition
classDefinitionWithDecorators :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm (Maybe Syntax.Decorators) -> Typed.TypedTerm Syntax.ClassDefinition
classDefinitionWithDecorators original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.python.syntax.ClassDefinition
classDefinitionWithName :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ClassDefinition
classDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "decorators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.python.syntax.ClassDefinition
classDefinitionWithTypeParams :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.ClassDefinition
classDefinitionWithTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "decorators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ClassPattern
classPattern :: Typed.TypedTerm Syntax.NameOrAttribute -> Typed.TypedTerm (Maybe Syntax.PositionalPatterns) -> Typed.TypedTerm (Maybe Syntax.KeywordPatterns) -> Typed.TypedTerm Syntax.ClassPattern
classPattern nameOrAttribute positionalPatterns keywordPatterns =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Typed.unTypedTerm nameOrAttribute)},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Typed.unTypedTerm positionalPatterns)},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Typed.unTypedTerm keywordPatterns)}]}))
-- | DSL accessor for the keywordPatterns field of hydra.python.syntax.ClassPattern
classPatternKeywordPatterns :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm (Maybe Syntax.KeywordPatterns)
classPatternKeywordPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionFieldName = (Core.Name "keywordPatterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the nameOrAttribute field of hydra.python.syntax.ClassPattern
classPatternNameOrAttribute :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm Syntax.NameOrAttribute
classPatternNameOrAttribute x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionFieldName = (Core.Name "nameOrAttribute")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the positionalPatterns field of hydra.python.syntax.ClassPattern
classPatternPositionalPatterns :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm (Maybe Syntax.PositionalPatterns)
classPatternPositionalPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionFieldName = (Core.Name "positionalPatterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywordPatterns field of hydra.python.syntax.ClassPattern
classPatternWithKeywordPatterns :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm (Maybe Syntax.KeywordPatterns) -> Typed.TypedTerm Syntax.ClassPattern
classPatternWithKeywordPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionFieldName = (Core.Name "nameOrAttribute")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionFieldName = (Core.Name "positionalPatterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the nameOrAttribute field of hydra.python.syntax.ClassPattern
classPatternWithNameOrAttribute :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm Syntax.NameOrAttribute -> Typed.TypedTerm Syntax.ClassPattern
classPatternWithNameOrAttribute original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionFieldName = (Core.Name "positionalPatterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionFieldName = (Core.Name "keywordPatterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the positionalPatterns field of hydra.python.syntax.ClassPattern
classPatternWithPositionalPatterns :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm (Maybe Syntax.PositionalPatterns) -> Typed.TypedTerm Syntax.ClassPattern
classPatternWithPositionalPatterns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionFieldName = (Core.Name "nameOrAttribute")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionFieldName = (Core.Name "keywordPatterns")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the capture variant of hydra.python.syntax.ClosedPattern
closedPatternCapture :: Typed.TypedTerm Syntax.CapturePattern -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternCapture x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "capture"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.python.syntax.ClosedPattern
closedPatternClass :: Typed.TypedTerm Syntax.ClassPattern -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the group variant of hydra.python.syntax.ClosedPattern
closedPatternGroup :: Typed.TypedTerm Syntax.GroupPattern -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternGroup x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.python.syntax.ClosedPattern
closedPatternLiteral :: Typed.TypedTerm Syntax.LiteralExpression -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mapping variant of hydra.python.syntax.ClosedPattern
closedPatternMapping :: Typed.TypedTerm Syntax.MappingPattern -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternMapping x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapping"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequence variant of hydra.python.syntax.ClosedPattern
closedPatternSequence :: Typed.TypedTerm Syntax.SequencePattern -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternSequence x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the value variant of hydra.python.syntax.ClosedPattern
closedPatternValue :: Typed.TypedTerm Syntax.ValuePattern -> Typed.TypedTerm Syntax.ClosedPattern
closedPatternValue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.python.syntax.ClosedPattern
closedPatternWildcard :: Typed.TypedTerm Syntax.ClosedPattern
closedPatternWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.CommaStarEtc
commaStarEtc :: Typed.TypedTerm [Syntax.ParamMaybeDefault] -> Typed.TypedTerm (Maybe Syntax.Keywords) -> Typed.TypedTerm Syntax.CommaStarEtc
commaStarEtc paramMaybeDefault keywords =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramMaybeDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Typed.unTypedTerm keywords)}]}))
-- | DSL accessor for the keywords field of hydra.python.syntax.CommaStarEtc
commaStarEtcKeywords :: Typed.TypedTerm Syntax.CommaStarEtc -> Typed.TypedTerm (Maybe Syntax.Keywords)
commaStarEtcKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
        Core.projectionFieldName = (Core.Name "keywords")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.CommaStarEtc
commaStarEtcParamMaybeDefault :: Typed.TypedTerm Syntax.CommaStarEtc -> Typed.TypedTerm [Syntax.ParamMaybeDefault]
commaStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
        Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.CommaStarEtc
commaStarEtcWithKeywords :: Typed.TypedTerm Syntax.CommaStarEtc -> Typed.TypedTerm (Maybe Syntax.Keywords) -> Typed.TypedTerm Syntax.CommaStarEtc
commaStarEtcWithKeywords original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
              Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the paramMaybeDefault field of hydra.python.syntax.CommaStarEtc
commaStarEtcWithParamMaybeDefault :: Typed.TypedTerm Syntax.CommaStarEtc -> Typed.TypedTerm [Syntax.ParamMaybeDefault] -> Typed.TypedTerm Syntax.CommaStarEtc
commaStarEtcWithParamMaybeDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
              Core.projectionFieldName = (Core.Name "keywords")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPair :: Typed.TypedTerm Syntax.CompareOp -> Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPair operator rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the operator field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairOperator :: Typed.TypedTerm Syntax.CompareOpBitwiseOrPair -> Typed.TypedTerm Syntax.CompareOp
compareOpBitwiseOrPairOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairRhs :: Typed.TypedTerm Syntax.CompareOpBitwiseOrPair -> Typed.TypedTerm Syntax.BitwiseOr
compareOpBitwiseOrPairRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operator field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithOperator :: Typed.TypedTerm Syntax.CompareOpBitwiseOrPair -> Typed.TypedTerm Syntax.CompareOp -> Typed.TypedTerm Syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithRhs :: Typed.TypedTerm Syntax.CompareOpBitwiseOrPair -> Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the eq variant of hydra.python.syntax.CompareOp
compareOpEq :: Typed.TypedTerm Syntax.CompareOp
compareOpEq =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the gt variant of hydra.python.syntax.CompareOp
compareOpGt :: Typed.TypedTerm Syntax.CompareOp
compareOpGt =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the gte variant of hydra.python.syntax.CompareOp
compareOpGte :: Typed.TypedTerm Syntax.CompareOp
compareOpGte =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.python.syntax.CompareOp
compareOpIn :: Typed.TypedTerm Syntax.CompareOp
compareOpIn =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the is variant of hydra.python.syntax.CompareOp
compareOpIs :: Typed.TypedTerm Syntax.CompareOp
compareOpIs =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "is"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the isnot variant of hydra.python.syntax.CompareOp
compareOpIsnot :: Typed.TypedTerm Syntax.CompareOp
compareOpIsnot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "isnot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lt variant of hydra.python.syntax.CompareOp
compareOpLt :: Typed.TypedTerm Syntax.CompareOp
compareOpLt =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lte variant of hydra.python.syntax.CompareOp
compareOpLte :: Typed.TypedTerm Syntax.CompareOp
compareOpLte =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the noteq variant of hydra.python.syntax.CompareOp
compareOpNoteq :: Typed.TypedTerm Syntax.CompareOp
compareOpNoteq =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noteq"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notin variant of hydra.python.syntax.CompareOp
compareOpNotin :: Typed.TypedTerm Syntax.CompareOp
compareOpNotin =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notin"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.Comparison
comparison :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm [Syntax.CompareOpBitwiseOrPair] -> Typed.TypedTerm Syntax.Comparison
comparison lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Comparison"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Comparison
comparisonLhs :: Typed.TypedTerm Syntax.Comparison -> Typed.TypedTerm Syntax.BitwiseOr
comparisonLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Comparison
comparisonRhs :: Typed.TypedTerm Syntax.Comparison -> Typed.TypedTerm [Syntax.CompareOpBitwiseOrPair]
comparisonRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Comparison
comparisonWithLhs :: Typed.TypedTerm Syntax.Comparison -> Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.Comparison
comparisonWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Comparison"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Comparison
comparisonWithRhs :: Typed.TypedTerm Syntax.Comparison -> Typed.TypedTerm [Syntax.CompareOpBitwiseOrPair] -> Typed.TypedTerm Syntax.Comparison
comparisonWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Comparison"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ComplexNumber
complexNumber :: Typed.TypedTerm Syntax.SignedRealNumber -> Typed.TypedTerm Syntax.PlusOrMinus -> Typed.TypedTerm Syntax.ImaginaryNumber -> Typed.TypedTerm Syntax.ComplexNumber
complexNumber real plusOrMinus imaginary =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Typed.unTypedTerm real)},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Typed.unTypedTerm plusOrMinus)},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Typed.unTypedTerm imaginary)}]}))
-- | DSL accessor for the imaginary field of hydra.python.syntax.ComplexNumber
complexNumberImaginary :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.ImaginaryNumber
complexNumberImaginary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionFieldName = (Core.Name "imaginary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the plusOrMinus field of hydra.python.syntax.ComplexNumber
complexNumberPlusOrMinus :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.PlusOrMinus
complexNumberPlusOrMinus x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionFieldName = (Core.Name "plusOrMinus")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the real field of hydra.python.syntax.ComplexNumber
complexNumberReal :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.SignedRealNumber
complexNumberReal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionFieldName = (Core.Name "real")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imaginary field of hydra.python.syntax.ComplexNumber
complexNumberWithImaginary :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.ImaginaryNumber -> Typed.TypedTerm Syntax.ComplexNumber
complexNumberWithImaginary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionFieldName = (Core.Name "real")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionFieldName = (Core.Name "plusOrMinus")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the plusOrMinus field of hydra.python.syntax.ComplexNumber
complexNumberWithPlusOrMinus :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.PlusOrMinus -> Typed.TypedTerm Syntax.ComplexNumber
complexNumberWithPlusOrMinus original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionFieldName = (Core.Name "real")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionFieldName = (Core.Name "imaginary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the real field of hydra.python.syntax.ComplexNumber
complexNumberWithReal :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.SignedRealNumber -> Typed.TypedTerm Syntax.ComplexNumber
complexNumberWithReal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionFieldName = (Core.Name "plusOrMinus")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionFieldName = (Core.Name "imaginary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the classDef variant of hydra.python.syntax.CompoundStatement
compoundStatementClassDef :: Typed.TypedTerm Syntax.ClassDefinition -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementClassDef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classDef"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.python.syntax.CompoundStatement
compoundStatementFor :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.python.syntax.CompoundStatement
compoundStatementFunction :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.python.syntax.CompoundStatement
compoundStatementIf :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the match variant of hydra.python.syntax.CompoundStatement
compoundStatementMatch :: Typed.TypedTerm Syntax.MatchStatement -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementMatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the try variant of hydra.python.syntax.CompoundStatement
compoundStatementTry :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementTry x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.python.syntax.CompoundStatement
compoundStatementWhile :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.python.syntax.CompoundStatement
compoundStatementWith :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm Syntax.CompoundStatement
compoundStatementWith x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Conditional
conditional :: Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Conditional
conditional body if_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Typed.unTypedTerm if_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.Conditional
conditionalBody :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Disjunction
conditionalBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.Conditional
conditionalElse :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Expression
conditionalElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the if field of hydra.python.syntax.Conditional
conditionalIf :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Disjunction
conditionalIf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionFieldName = (Core.Name "if")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.Conditional
conditionalWithBody :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm Syntax.Conditional
conditionalWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionFieldName = (Core.Name "if")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.Conditional
conditionalWithElse :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Conditional
conditionalWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionFieldName = (Core.Name "if")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the if field of hydra.python.syntax.Conditional
conditionalWithIf :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm Syntax.Conditional
conditionalWithIf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.Conjunction wrapper
conjunction :: Typed.TypedTerm [Syntax.Inversion] -> Typed.TypedTerm Syntax.Conjunction
conjunction x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Conjunction"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Decorators wrapper
decorators :: Typed.TypedTerm [Syntax.NamedExpression] -> Typed.TypedTerm Syntax.Decorators
decorators x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Decorators"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Default wrapper
default_ :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Default
default_ x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Default"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.DelStatement wrapper
delStatement :: Typed.TypedTerm Syntax.DelTargets -> Typed.TypedTerm Syntax.DelStatement
delStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DelStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the name variant of hydra.python.syntax.DelTAtom
delTAtomName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.DelTAtom
delTAtomName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the target variant of hydra.python.syntax.DelTAtom
delTAtomTarget :: Typed.TypedTerm Syntax.DelTarget -> Typed.TypedTerm Syntax.DelTAtom
delTAtomTarget x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the targets variant of hydra.python.syntax.DelTAtom
delTAtomTargets :: Typed.TypedTerm Syntax.DelTargets -> Typed.TypedTerm Syntax.DelTAtom
delTAtomTargets x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targets"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the delTAtom variant of hydra.python.syntax.DelTarget
delTargetDelTAtom :: Typed.TypedTerm Syntax.DelTAtom -> Typed.TypedTerm Syntax.DelTarget
delTargetDelTAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delTAtom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.DelTarget
delTargetPrimaryAndName :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.DelTarget
delTargetPrimaryAndName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.DelTarget
delTargetPrimaryAndSlices :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.DelTarget
delTargetPrimaryAndSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.DelTargets wrapper
delTargets :: Typed.TypedTerm [Syntax.DelTarget] -> Typed.TypedTerm Syntax.DelTargets
delTargets x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DelTargets"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Dict wrapper
dict :: Typed.TypedTerm [Syntax.DoubleStarredKvpair] -> Typed.TypedTerm Syntax.Dict
dict x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Dict"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Dictcomp
dictcomp :: Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Dictcomp
dictcomp kvpair forIfClauses =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kvpair"),
          Core.fieldTerm = (Typed.unTypedTerm kvpair)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Typed.unTypedTerm forIfClauses)}]}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Dictcomp
dictcompForIfClauses :: Typed.TypedTerm Syntax.Dictcomp -> Typed.TypedTerm Syntax.ForIfClauses
dictcompForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
        Core.projectionFieldName = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kvpair field of hydra.python.syntax.Dictcomp
dictcompKvpair :: Typed.TypedTerm Syntax.Dictcomp -> Typed.TypedTerm Syntax.Kvpair
dictcompKvpair x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
        Core.projectionFieldName = (Core.Name "kvpair")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Dictcomp
dictcompWithForIfClauses :: Typed.TypedTerm Syntax.Dictcomp -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Dictcomp
dictcompWithForIfClauses original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kvpair"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
              Core.projectionFieldName = (Core.Name "kvpair")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the kvpair field of hydra.python.syntax.Dictcomp
dictcompWithKvpair :: Typed.TypedTerm Syntax.Dictcomp -> Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.Dictcomp
dictcompWithKvpair original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kvpair"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
              Core.projectionFieldName = (Core.Name "forIfClauses")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.Disjunction wrapper
disjunction :: Typed.TypedTerm [Syntax.Conjunction] -> Typed.TypedTerm Syntax.Disjunction
disjunction x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Disjunction"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.DottedAsName
dottedAsName :: Typed.TypedTerm Syntax.DottedName -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.DottedAsName
dottedAsName name as =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.DottedAsName
dottedAsNameAs :: Typed.TypedTerm Syntax.DottedAsName -> Typed.TypedTerm (Maybe Syntax.Name)
dottedAsNameAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.DottedAsName
dottedAsNameName :: Typed.TypedTerm Syntax.DottedAsName -> Typed.TypedTerm Syntax.DottedName
dottedAsNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.DottedAsName
dottedAsNameWithAs :: Typed.TypedTerm Syntax.DottedAsName -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.DottedAsName
dottedAsNameWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.DottedAsName
dottedAsNameWithName :: Typed.TypedTerm Syntax.DottedAsName -> Typed.TypedTerm Syntax.DottedName -> Typed.TypedTerm Syntax.DottedAsName
dottedAsNameWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.DottedName wrapper
dottedName :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.DottedName
dottedName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DottedName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.DoubleStarPattern wrapper
doubleStarPattern :: Typed.TypedTerm Syntax.PatternCaptureTarget -> Typed.TypedTerm Syntax.DoubleStarPattern
doubleStarPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DoubleStarPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameter :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.DoubleStarTypeParameter
doubleStarTypeParameter name default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterDefault :: Typed.TypedTerm Syntax.DoubleStarTypeParameter -> Typed.TypedTerm (Maybe Syntax.Expression)
doubleStarTypeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterName :: Typed.TypedTerm Syntax.DoubleStarTypeParameter -> Typed.TypedTerm Syntax.Name
doubleStarTypeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithDefault :: Typed.TypedTerm Syntax.DoubleStarTypeParameter -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithName :: Typed.TypedTerm Syntax.DoubleStarTypeParameter -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the pair variant of hydra.python.syntax.DoubleStarredKvpair
doubleStarredKvpairPair :: Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.DoubleStarredKvpair
doubleStarredKvpairPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarredKvpair"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.DoubleStarredKvpair
doubleStarredKvpairStarred :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.DoubleStarredKvpair
doubleStarredKvpairStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarredKvpair"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Eval wrapper
eval :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Eval
eval x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Eval"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ExceptBlock
exceptBlock :: Typed.TypedTerm (Maybe Syntax.ExceptExpression) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ExceptBlock
exceptBlock expression body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.ExceptBlock
exceptBlockBody :: Typed.TypedTerm Syntax.ExceptBlock -> Typed.TypedTerm Syntax.Block
exceptBlockBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptBlock
exceptBlockExpression :: Typed.TypedTerm Syntax.ExceptBlock -> Typed.TypedTerm (Maybe Syntax.ExceptExpression)
exceptBlockExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.ExceptBlock
exceptBlockWithBody :: Typed.TypedTerm Syntax.ExceptBlock -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ExceptBlock
exceptBlockWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.ExceptBlock
exceptBlockWithExpression :: Typed.TypedTerm Syntax.ExceptBlock -> Typed.TypedTerm (Maybe Syntax.ExceptExpression) -> Typed.TypedTerm Syntax.ExceptBlock
exceptBlockWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ExceptExpression
exceptExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.ExceptExpression
exceptExpression expression as =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.ExceptExpression
exceptExpressionAs :: Typed.TypedTerm Syntax.ExceptExpression -> Typed.TypedTerm (Maybe Syntax.Name)
exceptExpressionAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptExpression
exceptExpressionExpression :: Typed.TypedTerm Syntax.ExceptExpression -> Typed.TypedTerm Syntax.Expression
exceptExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ExceptExpression
exceptExpressionWithAs :: Typed.TypedTerm Syntax.ExceptExpression -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.ExceptExpression
exceptExpressionWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.ExceptExpression
exceptExpressionWithExpression :: Typed.TypedTerm Syntax.ExceptExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ExceptExpression
exceptExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ExceptStarBlock
exceptStarBlock :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ExceptStarBlock
exceptStarBlock expression as body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockAs :: Typed.TypedTerm Syntax.ExceptStarBlock -> Typed.TypedTerm (Maybe Syntax.Name)
exceptStarBlockAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockBody :: Typed.TypedTerm Syntax.ExceptStarBlock -> Typed.TypedTerm Syntax.Block
exceptStarBlockBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockExpression :: Typed.TypedTerm Syntax.ExceptStarBlock -> Typed.TypedTerm Syntax.Expression
exceptStarBlockExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithAs :: Typed.TypedTerm Syntax.ExceptStarBlock -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.ExceptStarBlock
exceptStarBlockWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithBody :: Typed.TypedTerm Syntax.ExceptStarBlock -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ExceptStarBlock
exceptStarBlockWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithExpression :: Typed.TypedTerm Syntax.ExceptStarBlock -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ExceptStarBlock
exceptStarBlockWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the conditional variant of hydra.python.syntax.Expression
expressionConditional :: Typed.TypedTerm Syntax.Conditional -> Typed.TypedTerm Syntax.Expression
expressionConditional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.python.syntax.Expression
expressionLambda :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm Syntax.Expression
expressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Expression
expressionSimple :: Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm Syntax.Expression
expressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the complement variant of hydra.python.syntax.Factor
factorComplement :: Typed.TypedTerm Syntax.Factor -> Typed.TypedTerm Syntax.Factor
factorComplement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the negative variant of hydra.python.syntax.Factor
factorNegative :: Typed.TypedTerm Syntax.Factor -> Typed.TypedTerm Syntax.Factor
factorNegative x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negative"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the positive variant of hydra.python.syntax.Factor
factorPositive :: Typed.TypedTerm Syntax.Factor -> Typed.TypedTerm Syntax.Factor
factorPositive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "positive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Factor
factorSimple :: Typed.TypedTerm Syntax.Power -> Typed.TypedTerm Syntax.Factor
factorSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.File wrapper
file :: Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.File
file x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.File"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ForIfClause
forIfClause :: Typed.TypedTerm Bool -> Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm [Syntax.Disjunction] -> Typed.TypedTerm Syntax.ForIfClause
forIfClause async targets in_ ifs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm targets)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Typed.unTypedTerm ifs)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.ForIfClause
forIfClauseAsync :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm Bool
forIfClauseAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifs field of hydra.python.syntax.ForIfClause
forIfClauseIfs :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm [Syntax.Disjunction]
forIfClauseIfs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "ifs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.python.syntax.ForIfClause
forIfClauseIn :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm Syntax.Disjunction
forIfClauseIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ForIfClause
forIfClauseTargets :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm [Syntax.StarTarget]
forIfClauseTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.ForIfClause
forIfClauseWithAsync :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ForIfClause
forIfClauseWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "ifs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the ifs field of hydra.python.syntax.ForIfClause
forIfClauseWithIfs :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm [Syntax.Disjunction] -> Typed.TypedTerm Syntax.ForIfClause
forIfClauseWithIfs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the in field of hydra.python.syntax.ForIfClause
forIfClauseWithIn :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm Syntax.ForIfClause
forIfClauseWithIn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "ifs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.ForIfClause
forIfClauseWithTargets :: Typed.TypedTerm Syntax.ForIfClause -> Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.ForIfClause
forIfClauseWithTargets original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionFieldName = (Core.Name "ifs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.ForIfClauses wrapper
forIfClauses :: Typed.TypedTerm [Syntax.ForIfClause] -> Typed.TypedTerm Syntax.ForIfClauses
forIfClauses x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ForIfClauses"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ForStatement
forStatement :: Typed.TypedTerm Bool -> Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm [Syntax.StarExpression] -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.ForStatement
forStatement async targets expressions typeComment body else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm targets)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.ForStatement
forStatementAsync :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Bool
forStatementAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ForStatement
forStatementBody :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.Block
forStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.ForStatement
forStatementElse :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.Block)
forStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expressions field of hydra.python.syntax.ForStatement
forStatementExpressions :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm [Syntax.StarExpression]
forStatementExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ForStatement
forStatementTargets :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm [Syntax.StarTarget]
forStatementTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ForStatement
forStatementTypeComment :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.TypeComment)
forStatementTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.ForStatement
forStatementWithAsync :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ForStatement
forStatementWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.ForStatement
forStatementWithBody :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ForStatement
forStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.ForStatement
forStatementWithElse :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.ForStatement
forStatementWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expressions field of hydra.python.syntax.ForStatement
forStatementWithExpressions :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm [Syntax.StarExpression] -> Typed.TypedTerm Syntax.ForStatement
forStatementWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.ForStatement
forStatementWithTargets :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.ForStatement
forStatementWithTargets original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ForStatement
forStatementWithTypeComment :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ForStatement
forStatementWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.FuncTypeComment wrapper
funcTypeComment :: Typed.TypedTerm Syntax.TypeComment -> Typed.TypedTerm Syntax.FuncTypeComment
funcTypeComment x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.FuncTypeComment"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.FunctionDefRaw
functionDefRaw :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm (Maybe Syntax.Parameters) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.FuncTypeComment) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRaw async name typeParams params returnType funcTypeComment block =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Typed.unTypedTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Typed.unTypedTerm funcTypeComment)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.FunctionDefRaw
functionDefRawAsync :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Bool
functionDefRawAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the block field of hydra.python.syntax.FunctionDefRaw
functionDefRawBlock :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Syntax.Block
functionDefRawBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the funcTypeComment field of hydra.python.syntax.FunctionDefRaw
functionDefRawFuncTypeComment :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm (Maybe Syntax.FuncTypeComment)
functionDefRawFuncTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "funcTypeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.FunctionDefRaw
functionDefRawName :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Syntax.Name
functionDefRawName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.python.syntax.FunctionDefRaw
functionDefRawParams :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm (Maybe Syntax.Parameters)
functionDefRawParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the returnType field of hydra.python.syntax.FunctionDefRaw
functionDefRawReturnType :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm (Maybe Syntax.Expression)
functionDefRawReturnType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "returnType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.FunctionDefRaw
functionDefRawTypeParams :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm [Syntax.TypeParameter]
functionDefRawTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithAsync :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the block field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithBlock :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the funcTypeComment field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithFuncTypeComment :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm (Maybe Syntax.FuncTypeComment) -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithFuncTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithName :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithParams :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm (Maybe Syntax.Parameters) -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the returnType field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithReturnType :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithReturnType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithTypeParams :: Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefRawWithTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.FunctionDefinition
functionDefinition :: Typed.TypedTerm (Maybe Syntax.Decorators) -> Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinition decorators raw =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Typed.unTypedTerm decorators)},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = (Typed.unTypedTerm raw)}]}))
-- | DSL accessor for the decorators field of hydra.python.syntax.FunctionDefinition
functionDefinitionDecorators :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm (Maybe Syntax.Decorators)
functionDefinitionDecorators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "decorators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the raw field of hydra.python.syntax.FunctionDefinition
functionDefinitionRaw :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm Syntax.FunctionDefRaw
functionDefinitionRaw x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "raw")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decorators field of hydra.python.syntax.FunctionDefinition
functionDefinitionWithDecorators :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm (Maybe Syntax.Decorators) -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithDecorators original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "raw")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the raw field of hydra.python.syntax.FunctionDefinition
functionDefinitionWithRaw :: Typed.TypedTerm Syntax.FunctionDefinition -> Typed.TypedTerm Syntax.FunctionDefRaw -> Typed.TypedTerm Syntax.FunctionDefinition
functionDefinitionWithRaw original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
              Core.projectionFieldName = (Core.Name "decorators")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.Genexp
genexp :: Typed.TypedTerm Syntax.GenexpHead -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Genexp
genexp head tail =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Genexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm tail)}]}))
-- | DSL accessor for the head field of hydra.python.syntax.Genexp
genexpHead :: Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.GenexpHead
genexpHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.python.syntax.GenexpHead
genexpHeadAssignment :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.GenexpHead
genexpHeadAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.GenexpHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.GenexpHead
genexpHeadExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.GenexpHead
genexpHeadExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.GenexpHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the tail field of hydra.python.syntax.Genexp
genexpTail :: Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.ForIfClauses
genexpTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.Genexp
genexpWithHead :: Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.GenexpHead -> Typed.TypedTerm Syntax.Genexp
genexpWithHead original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Genexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
              Core.projectionFieldName = (Core.Name "tail")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.python.syntax.Genexp
genexpWithTail :: Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Genexp
genexpWithTail original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Genexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the expression variant of hydra.python.syntax.Group
groupExpression :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Group
groupExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Group"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.GroupPattern wrapper
groupPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.GroupPattern
groupPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.GroupPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the yield variant of hydra.python.syntax.Group
groupYield :: Typed.TypedTerm Syntax.YieldExpression -> Typed.TypedTerm Syntax.Group
groupYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Group"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Guard wrapper
guard :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Guard
guard x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Guard"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.IfStatement
ifStatement :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm (Maybe Syntax.IfTail) -> Typed.TypedTerm Syntax.IfStatement
ifStatement condition body continuation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Typed.unTypedTerm continuation)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.IfStatement
ifStatementBody :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Block
ifStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the condition field of hydra.python.syntax.IfStatement
ifStatementCondition :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.NamedExpression
ifStatementCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the continuation field of hydra.python.syntax.IfStatement
ifStatementContinuation :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm (Maybe Syntax.IfTail)
ifStatementContinuation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "continuation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.IfStatement
ifStatementWithBody :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.IfStatement
ifStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "continuation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the condition field of hydra.python.syntax.IfStatement
ifStatementWithCondition :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.IfStatement
ifStatementWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "continuation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the continuation field of hydra.python.syntax.IfStatement
ifStatementWithContinuation :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm (Maybe Syntax.IfTail) -> Typed.TypedTerm Syntax.IfStatement
ifStatementWithContinuation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the elif variant of hydra.python.syntax.IfTail
ifTailElif :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.IfTail
ifTailElif x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.IfTail"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elif"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the else variant of hydra.python.syntax.IfTail
ifTailElse :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.IfTail
ifTailElse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.IfTail"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "else"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.ImaginaryNumber wrapper
imaginaryNumber :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.ImaginaryNumber
imaginaryNumber x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ImaginaryNumber"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ImportFrom
importFrom :: Typed.TypedTerm [Syntax.RelativeImportPrefix] -> Typed.TypedTerm (Maybe Syntax.DottedName) -> Typed.TypedTerm Syntax.ImportFromTargets -> Typed.TypedTerm Syntax.ImportFrom
importFrom prefixes dottedName targets =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Typed.unTypedTerm prefixes)},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Typed.unTypedTerm dottedName)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm targets)}]}))
-- | DSL constructor for hydra.python.syntax.ImportFromAsName
importFromAsName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.ImportFromAsName
importFromAsName name as =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.ImportFromAsName
importFromAsNameAs :: Typed.TypedTerm Syntax.ImportFromAsName -> Typed.TypedTerm (Maybe Syntax.Name)
importFromAsNameAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ImportFromAsName
importFromAsNameName :: Typed.TypedTerm Syntax.ImportFromAsName -> Typed.TypedTerm Syntax.Name
importFromAsNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ImportFromAsName
importFromAsNameWithAs :: Typed.TypedTerm Syntax.ImportFromAsName -> Typed.TypedTerm (Maybe Syntax.Name) -> Typed.TypedTerm Syntax.ImportFromAsName
importFromAsNameWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.ImportFromAsName
importFromAsNameWithName :: Typed.TypedTerm Syntax.ImportFromAsName -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ImportFromAsName
importFromAsNameWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the dottedName field of hydra.python.syntax.ImportFrom
importFromDottedName :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm (Maybe Syntax.DottedName)
importFromDottedName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionFieldName = (Core.Name "dottedName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the prefixes field of hydra.python.syntax.ImportFrom
importFromPrefixes :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm [Syntax.RelativeImportPrefix]
importFromPrefixes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionFieldName = (Core.Name "prefixes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ImportFrom
importFromTargets :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm Syntax.ImportFromTargets
importFromTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the parens variant of hydra.python.syntax.ImportFromTargets
importFromTargetsParens :: Typed.TypedTerm [Syntax.ImportFromAsName] -> Typed.TypedTerm Syntax.ImportFromTargets
importFromTargetsParens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.ImportFromTargets
importFromTargetsSimple :: Typed.TypedTerm [Syntax.ImportFromAsName] -> Typed.TypedTerm Syntax.ImportFromTargets
importFromTargetsSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.ImportFromTargets
importFromTargetsStar :: Typed.TypedTerm Syntax.ImportFromTargets
importFromTargetsStar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the dottedName field of hydra.python.syntax.ImportFrom
importFromWithDottedName :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm (Maybe Syntax.DottedName) -> Typed.TypedTerm Syntax.ImportFrom
importFromWithDottedName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionFieldName = (Core.Name "prefixes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the prefixes field of hydra.python.syntax.ImportFrom
importFromWithPrefixes :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm [Syntax.RelativeImportPrefix] -> Typed.TypedTerm Syntax.ImportFrom
importFromWithPrefixes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionFieldName = (Core.Name "dottedName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.ImportFrom
importFromWithTargets :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm Syntax.ImportFromTargets -> Typed.TypedTerm Syntax.ImportFrom
importFromWithTargets original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionFieldName = (Core.Name "prefixes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionFieldName = (Core.Name "dottedName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.ImportName wrapper
importName :: Typed.TypedTerm [Syntax.DottedAsName] -> Typed.TypedTerm Syntax.ImportName
importName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ImportName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the from variant of hydra.python.syntax.ImportStatement
importStatementFrom :: Typed.TypedTerm Syntax.ImportFrom -> Typed.TypedTerm Syntax.ImportStatement
importStatementFrom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.ImportStatement
importStatementName :: Typed.TypedTerm Syntax.ImportName -> Typed.TypedTerm Syntax.ImportStatement
importStatementName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Interactive wrapper
interactive :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.Interactive
interactive x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Interactive"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the not variant of hydra.python.syntax.Inversion
inversionNot :: Typed.TypedTerm Syntax.Inversion -> Typed.TypedTerm Syntax.Inversion
inversionNot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Inversion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Inversion
inversionSimple :: Typed.TypedTerm Syntax.Comparison -> Typed.TypedTerm Syntax.Inversion
inversionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Inversion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.ItemsPattern wrapper
itemsPattern :: Typed.TypedTerm [Syntax.KeyValuePattern] -> Typed.TypedTerm Syntax.ItemsPattern
itemsPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ItemsPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.KeyValuePattern
keyValuePattern :: Typed.TypedTerm Syntax.LiteralExpressionOrAttribute -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.KeyValuePattern
keyValuePattern key value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.python.syntax.KeyValuePattern
keyValuePatternKey :: Typed.TypedTerm Syntax.KeyValuePattern -> Typed.TypedTerm Syntax.LiteralExpressionOrAttribute
keyValuePatternKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.KeyValuePattern
keyValuePatternValue :: Typed.TypedTerm Syntax.KeyValuePattern -> Typed.TypedTerm Syntax.Pattern
keyValuePatternValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.python.syntax.KeyValuePattern
keyValuePatternWithKey :: Typed.TypedTerm Syntax.KeyValuePattern -> Typed.TypedTerm Syntax.LiteralExpressionOrAttribute -> Typed.TypedTerm Syntax.KeyValuePattern
keyValuePatternWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.python.syntax.KeyValuePattern
keyValuePatternWithValue :: Typed.TypedTerm Syntax.KeyValuePattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.KeyValuePattern
keyValuePatternWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.KeywordPattern
keywordPattern :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.KeywordPattern
keywordPattern name pattern =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.KeywordPattern
keywordPatternName :: Typed.TypedTerm Syntax.KeywordPattern -> Typed.TypedTerm Syntax.Name
keywordPatternName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.python.syntax.KeywordPattern
keywordPatternPattern :: Typed.TypedTerm Syntax.KeywordPattern -> Typed.TypedTerm Syntax.Pattern
keywordPatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.KeywordPattern
keywordPatternWithName :: Typed.TypedTerm Syntax.KeywordPattern -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.KeywordPattern
keywordPatternWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pattern field of hydra.python.syntax.KeywordPattern
keywordPatternWithPattern :: Typed.TypedTerm Syntax.KeywordPattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.KeywordPattern
keywordPatternWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.KeywordPatterns wrapper
keywordPatterns :: Typed.TypedTerm [Syntax.KeywordPattern] -> Typed.TypedTerm Syntax.KeywordPatterns
keywordPatterns x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.KeywordPatterns"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Keywords wrapper
keywords :: Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm Syntax.Keywords
keywords x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Keywords"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Kvpair
kvpair :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Kvpair
kvpair key value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.python.syntax.Kvpair
kvpairKey :: Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.Expression
kvpairKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.Kvpair
kvpairValue :: Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.Expression
kvpairValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.python.syntax.Kvpair
kvpairWithKey :: Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Kvpair
kvpairWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.python.syntax.Kvpair
kvpairWithValue :: Typed.TypedTerm Syntax.Kvpair -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Kvpair
kvpairWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.Kwarg
kwarg :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Kwarg
kwarg name value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.Kwarg
kwargName :: Typed.TypedTerm Syntax.Kwarg -> Typed.TypedTerm Syntax.Name
kwargName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the doubleStarred variant of hydra.python.syntax.KwargOrDoubleStarred
kwargOrDoubleStarredDoubleStarred :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.KwargOrDoubleStarred
kwargOrDoubleStarredDoubleStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrDoubleStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStarred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the kwarg variant of hydra.python.syntax.KwargOrDoubleStarred
kwargOrDoubleStarredKwarg :: Typed.TypedTerm Syntax.Kwarg -> Typed.TypedTerm Syntax.KwargOrDoubleStarred
kwargOrDoubleStarredKwarg x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrDoubleStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwarg"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the kwarg variant of hydra.python.syntax.KwargOrStarred
kwargOrStarredKwarg :: Typed.TypedTerm Syntax.Kwarg -> Typed.TypedTerm Syntax.KwargOrStarred
kwargOrStarredKwarg x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwarg"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.KwargOrStarred
kwargOrStarredStarred :: Typed.TypedTerm Syntax.StarredExpression -> Typed.TypedTerm Syntax.KwargOrStarred
kwargOrStarredStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the value field of hydra.python.syntax.Kwarg
kwargValue :: Typed.TypedTerm Syntax.Kwarg -> Typed.TypedTerm Syntax.Expression
kwargValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.Kwarg
kwargWithName :: Typed.TypedTerm Syntax.Kwarg -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Kwarg
kwargWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.python.syntax.Kwarg
kwargWithValue :: Typed.TypedTerm Syntax.Kwarg -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Kwarg
kwargWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.Lambda
lambda :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Lambda
lambda params body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.Lambda
lambdaBody :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm Syntax.Expression
lambdaBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.LambdaKwds wrapper
lambdaKwds :: Typed.TypedTerm Syntax.LambdaParamNoDefault -> Typed.TypedTerm Syntax.LambdaKwds
lambdaKwds x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.LambdaKwds"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefault :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Default) -> Typed.TypedTerm Syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefault param default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultDefault :: Typed.TypedTerm Syntax.LambdaParamMaybeDefault -> Typed.TypedTerm (Maybe Syntax.Default)
lambdaParamMaybeDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultParam :: Typed.TypedTerm Syntax.LambdaParamMaybeDefault -> Typed.TypedTerm Syntax.Name
lambdaParamMaybeDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithDefault :: Typed.TypedTerm Syntax.LambdaParamMaybeDefault -> Typed.TypedTerm (Maybe Syntax.Default) -> Typed.TypedTerm Syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the param field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithParam :: Typed.TypedTerm Syntax.LambdaParamMaybeDefault -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.LambdaParamNoDefault wrapper
lambdaParamNoDefault :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.LambdaParamNoDefault
lambdaParamNoDefault x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.LambdaParamNoDefault"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefault :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Default) -> Typed.TypedTerm Syntax.LambdaParamWithDefault
lambdaParamWithDefault param default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultDefault :: Typed.TypedTerm Syntax.LambdaParamWithDefault -> Typed.TypedTerm (Maybe Syntax.Default)
lambdaParamWithDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultParam :: Typed.TypedTerm Syntax.LambdaParamWithDefault -> Typed.TypedTerm Syntax.Name
lambdaParamWithDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithDefault :: Typed.TypedTerm Syntax.LambdaParamWithDefault -> Typed.TypedTerm (Maybe Syntax.Default) -> Typed.TypedTerm Syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the param field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithParam :: Typed.TypedTerm Syntax.LambdaParamWithDefault -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.LambdaParameters
lambdaParameters :: Typed.TypedTerm (Maybe Syntax.LambdaSlashNoDefault) -> Typed.TypedTerm [Syntax.LambdaParamNoDefault] -> Typed.TypedTerm [Syntax.LambdaParamWithDefault] -> Typed.TypedTerm (Maybe Syntax.LambdaStarEtc) -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParameters slashNoDefault paramNoDefault paramWithDefault starEtc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm slashNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersParamNoDefault :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm [Syntax.LambdaParamNoDefault]
lambdaParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersParamWithDefault :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm [Syntax.LambdaParamWithDefault]
lambdaParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slashNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersSlashNoDefault :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm (Maybe Syntax.LambdaSlashNoDefault)
lambdaParametersSlashNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "slashNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.LambdaParameters
lambdaParametersStarEtc :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm (Maybe Syntax.LambdaStarEtc)
lambdaParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithParamNoDefault :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm [Syntax.LambdaParamNoDefault] -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParametersWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "slashNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithParamWithDefault :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm [Syntax.LambdaParamWithDefault] -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParametersWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "slashNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the slashNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithSlashNoDefault :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm (Maybe Syntax.LambdaSlashNoDefault) -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParametersWithSlashNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.LambdaParameters
lambdaParametersWithStarEtc :: Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm (Maybe Syntax.LambdaStarEtc) -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParametersWithStarEtc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "slashNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the params field of hydra.python.syntax.Lambda
lambdaParams :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm Syntax.LambdaParameters
lambdaParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefault :: Typed.TypedTerm [Syntax.LambdaParamNoDefault] -> Typed.TypedTerm Syntax.LambdaSlashNoDefault
lambdaSlashNoDefault parameters =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)}]}))
-- | DSL accessor for the parameters field of hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultParameters :: Typed.TypedTerm Syntax.LambdaSlashNoDefault -> Typed.TypedTerm [Syntax.LambdaParamNoDefault]
lambdaSlashNoDefaultParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the parameters field of hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultWithParameters :: Typed.TypedTerm Syntax.LambdaSlashNoDefault -> Typed.TypedTerm [Syntax.LambdaParamNoDefault] -> Typed.TypedTerm Syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefault :: Typed.TypedTerm [Syntax.LambdaParamNoDefault] -> Typed.TypedTerm [Syntax.LambdaParamWithDefault] -> Typed.TypedTerm Syntax.LambdaSlashWithDefault
lambdaSlashWithDefault paramNoDefault paramWithDefault =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultParamNoDefault :: Typed.TypedTerm Syntax.LambdaSlashWithDefault -> Typed.TypedTerm [Syntax.LambdaParamNoDefault]
lambdaSlashWithDefaultParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultParamWithDefault :: Typed.TypedTerm Syntax.LambdaSlashWithDefault -> Typed.TypedTerm [Syntax.LambdaParamWithDefault]
lambdaSlashWithDefaultParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamNoDefault :: Typed.TypedTerm Syntax.LambdaSlashWithDefault -> Typed.TypedTerm [Syntax.LambdaParamNoDefault] -> Typed.TypedTerm Syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamWithDefault :: Typed.TypedTerm Syntax.LambdaSlashWithDefault -> Typed.TypedTerm [Syntax.LambdaParamWithDefault] -> Typed.TypedTerm Syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the kwds variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcKwds :: Typed.TypedTerm Syntax.LambdaKwds -> Typed.TypedTerm Syntax.LambdaStarEtc
lambdaStarEtcKwds x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwds"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the paramMaybeDefault variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcParamMaybeDefault :: Typed.TypedTerm [Syntax.LambdaParamMaybeDefault] -> Typed.TypedTerm Syntax.LambdaStarEtc
lambdaStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramMaybeDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the paramNoDefault variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcParamNoDefault :: Typed.TypedTerm Syntax.LambdaParamNoDefault -> Typed.TypedTerm Syntax.LambdaStarEtc
lambdaStarEtcParamNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcStar :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.LambdaStarEtc
lambdaStarEtcStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the body field of hydra.python.syntax.Lambda
lambdaWithBody :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Lambda
lambdaWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the params field of hydra.python.syntax.Lambda
lambdaWithParams :: Typed.TypedTerm Syntax.Lambda -> Typed.TypedTerm Syntax.LambdaParameters -> Typed.TypedTerm Syntax.Lambda
lambdaWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.List wrapper
list :: Typed.TypedTerm [Syntax.StarNamedExpression] -> Typed.TypedTerm Syntax.List
list x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.List"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Listcomp
listcomp :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Listcomp
listcomp expression forIfClauses =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Typed.unTypedTerm forIfClauses)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.Listcomp
listcompExpression :: Typed.TypedTerm Syntax.Listcomp -> Typed.TypedTerm Syntax.NamedExpression
listcompExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Listcomp
listcompForIfClauses :: Typed.TypedTerm Syntax.Listcomp -> Typed.TypedTerm Syntax.ForIfClauses
listcompForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
        Core.projectionFieldName = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.Listcomp
listcompWithExpression :: Typed.TypedTerm Syntax.Listcomp -> Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Listcomp
listcompWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
              Core.projectionFieldName = (Core.Name "forIfClauses")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Listcomp
listcompWithForIfClauses :: Typed.TypedTerm Syntax.Listcomp -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Listcomp
listcompWithForIfClauses original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the complex variant of hydra.python.syntax.LiteralExpression
literalExpressionComplex :: Typed.TypedTerm Syntax.ComplexNumber -> Typed.TypedTerm Syntax.LiteralExpression
literalExpressionComplex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the false variant of hydra.python.syntax.LiteralExpression
literalExpressionFalse :: Typed.TypedTerm Syntax.LiteralExpression
literalExpressionFalse =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "false"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.python.syntax.LiteralExpression
literalExpressionNone :: Typed.TypedTerm Syntax.LiteralExpression
literalExpressionNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.LiteralExpression
literalExpressionNumber :: Typed.TypedTerm Syntax.SignedNumber -> Typed.TypedTerm Syntax.LiteralExpression
literalExpressionNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the attribute variant of hydra.python.syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeAttribute :: Typed.TypedTerm Syntax.Attribute -> Typed.TypedTerm Syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeAttribute x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpressionOrAttribute"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "attribute"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.python.syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeLiteral :: Typed.TypedTerm Syntax.LiteralExpression -> Typed.TypedTerm Syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpressionOrAttribute"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.python.syntax.LiteralExpression
literalExpressionString :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.LiteralExpression
literalExpressionString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the true variant of hydra.python.syntax.LiteralExpression
literalExpressionTrue :: Typed.TypedTerm Syntax.LiteralExpression
literalExpressionTrue =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "true"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.MappingPattern
mappingPattern :: Typed.TypedTerm (Maybe Syntax.ItemsPattern) -> Typed.TypedTerm (Maybe Syntax.DoubleStarPattern) -> Typed.TypedTerm Syntax.MappingPattern
mappingPattern items doubleStar =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Typed.unTypedTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "doubleStar"),
          Core.fieldTerm = (Typed.unTypedTerm doubleStar)}]}))
-- | DSL accessor for the doubleStar field of hydra.python.syntax.MappingPattern
mappingPatternDoubleStar :: Typed.TypedTerm Syntax.MappingPattern -> Typed.TypedTerm (Maybe Syntax.DoubleStarPattern)
mappingPatternDoubleStar x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
        Core.projectionFieldName = (Core.Name "doubleStar")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the items field of hydra.python.syntax.MappingPattern
mappingPatternItems :: Typed.TypedTerm Syntax.MappingPattern -> Typed.TypedTerm (Maybe Syntax.ItemsPattern)
mappingPatternItems x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
        Core.projectionFieldName = (Core.Name "items")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the doubleStar field of hydra.python.syntax.MappingPattern
mappingPatternWithDoubleStar :: Typed.TypedTerm Syntax.MappingPattern -> Typed.TypedTerm (Maybe Syntax.DoubleStarPattern) -> Typed.TypedTerm Syntax.MappingPattern
mappingPatternWithDoubleStar original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
              Core.projectionFieldName = (Core.Name "items")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doubleStar"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the items field of hydra.python.syntax.MappingPattern
mappingPatternWithItems :: Typed.TypedTerm Syntax.MappingPattern -> Typed.TypedTerm (Maybe Syntax.ItemsPattern) -> Typed.TypedTerm Syntax.MappingPattern
mappingPatternWithItems original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doubleStar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
              Core.projectionFieldName = (Core.Name "doubleStar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.MatchStatement
matchStatement :: Typed.TypedTerm Syntax.SubjectExpression -> Typed.TypedTerm [Syntax.CaseBlock] -> Typed.TypedTerm Syntax.MatchStatement
matchStatement subject cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.python.syntax.MatchStatement
matchStatementCases :: Typed.TypedTerm Syntax.MatchStatement -> Typed.TypedTerm [Syntax.CaseBlock]
matchStatementCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.python.syntax.MatchStatement
matchStatementSubject :: Typed.TypedTerm Syntax.MatchStatement -> Typed.TypedTerm Syntax.SubjectExpression
matchStatementSubject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.python.syntax.MatchStatement
matchStatementWithCases :: Typed.TypedTerm Syntax.MatchStatement -> Typed.TypedTerm [Syntax.CaseBlock] -> Typed.TypedTerm Syntax.MatchStatement
matchStatementWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
              Core.projectionFieldName = (Core.Name "subject")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the subject field of hydra.python.syntax.MatchStatement
matchStatementWithSubject :: Typed.TypedTerm Syntax.MatchStatement -> Typed.TypedTerm Syntax.SubjectExpression -> Typed.TypedTerm Syntax.MatchStatement
matchStatementWithSubject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.MaybeSequencePattern wrapper
maybeSequencePattern :: Typed.TypedTerm [Syntax.MaybeStarPattern] -> Typed.TypedTerm Syntax.MaybeSequencePattern
maybeSequencePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.MaybeSequencePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the pattern variant of hydra.python.syntax.MaybeStarPattern
maybeStarPatternPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.MaybeStarPattern
maybeStarPatternPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.MaybeStarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.MaybeStarPattern
maybeStarPatternStar :: Typed.TypedTerm Syntax.StarPattern -> Typed.TypedTerm Syntax.MaybeStarPattern
maybeStarPatternStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.MaybeStarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Module wrapper
module_ :: Typed.TypedTerm [[Syntax.Statement]] -> Typed.TypedTerm Syntax.Module
module_ x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Module"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Name wrapper
name :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Name
name x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Name"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.NameOrAttribute wrapper
nameOrAttribute :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.NameOrAttribute
nameOrAttribute x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.NameOrAttribute"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.python.syntax.NamedExpression
namedExpressionAssignment :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.NamedExpression
namedExpressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.NamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.NamedExpression
namedExpressionSimple :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.NamedExpression
namedExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.NamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtc :: Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation -> Typed.TypedTerm [Syntax.ParamMaybeDefault] -> Typed.TypedTerm (Maybe Syntax.Keywords) -> Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtc paramNoDefaultStarAnnotation paramMaybeDefault keywords =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefaultStarAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramMaybeDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Typed.unTypedTerm keywords)}]}))
-- | DSL accessor for the keywords field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcKeywords :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm (Maybe Syntax.Keywords)
noDefaultStarAnnotationStarEtcKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionFieldName = (Core.Name "keywords")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcParamMaybeDefault :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm [Syntax.ParamMaybeDefault]
noDefaultStarAnnotationStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramNoDefaultStarAnnotation field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionFieldName = (Core.Name "paramNoDefaultStarAnnotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithKeywords :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm (Maybe Syntax.Keywords) -> Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithKeywords original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionFieldName = (Core.Name "paramNoDefaultStarAnnotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamMaybeDefault :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm [Syntax.ParamMaybeDefault] -> Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamMaybeDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionFieldName = (Core.Name "paramNoDefaultStarAnnotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionFieldName = (Core.Name "keywords")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramNoDefaultStarAnnotation field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation -> Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionFieldName = (Core.Name "keywords")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtc :: Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm [Syntax.ParamMaybeDefault] -> Typed.TypedTerm (Maybe Syntax.Keywords) -> Typed.TypedTerm Syntax.NoDefaultStarEtc
noDefaultStarEtc paramNoDefault paramMaybeDefault keywords =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramMaybeDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Typed.unTypedTerm keywords)}]}))
-- | DSL accessor for the keywords field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcKeywords :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm (Maybe Syntax.Keywords)
noDefaultStarEtcKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionFieldName = (Core.Name "keywords")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcParamMaybeDefault :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm [Syntax.ParamMaybeDefault]
noDefaultStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcParamNoDefault :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm Syntax.ParamNoDefault
noDefaultStarEtcParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithKeywords :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm (Maybe Syntax.Keywords) -> Typed.TypedTerm Syntax.NoDefaultStarEtc
noDefaultStarEtcWithKeywords original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamMaybeDefault :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm [Syntax.ParamMaybeDefault] -> Typed.TypedTerm Syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamMaybeDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionFieldName = (Core.Name "keywords")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamNoDefault :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm Syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionFieldName = (Core.Name "keywords")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the float variant of hydra.python.syntax.Number
numberFloat :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.Number
numberFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the imaginary variant of hydra.python.syntax.Number
numberImaginary :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.Number
numberImaginary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "imaginary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.python.syntax.Number
numberInteger :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.Number
numberInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.OpenSequencePattern
openSequencePattern :: Typed.TypedTerm Syntax.MaybeStarPattern -> Typed.TypedTerm (Maybe Syntax.MaybeSequencePattern) -> Typed.TypedTerm Syntax.OpenSequencePattern
openSequencePattern head tail =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm tail)}]}))
-- | DSL accessor for the head field of hydra.python.syntax.OpenSequencePattern
openSequencePatternHead :: Typed.TypedTerm Syntax.OpenSequencePattern -> Typed.TypedTerm Syntax.MaybeStarPattern
openSequencePatternHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tail field of hydra.python.syntax.OpenSequencePattern
openSequencePatternTail :: Typed.TypedTerm Syntax.OpenSequencePattern -> Typed.TypedTerm (Maybe Syntax.MaybeSequencePattern)
openSequencePatternTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.OpenSequencePattern
openSequencePatternWithHead :: Typed.TypedTerm Syntax.OpenSequencePattern -> Typed.TypedTerm Syntax.MaybeStarPattern -> Typed.TypedTerm Syntax.OpenSequencePattern
openSequencePatternWithHead original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
              Core.projectionFieldName = (Core.Name "tail")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.python.syntax.OpenSequencePattern
openSequencePatternWithTail :: Typed.TypedTerm Syntax.OpenSequencePattern -> Typed.TypedTerm (Maybe Syntax.MaybeSequencePattern) -> Typed.TypedTerm Syntax.OpenSequencePattern
openSequencePatternWithTail original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.OrPattern wrapper
orPattern :: Typed.TypedTerm [Syntax.ClosedPattern] -> Typed.TypedTerm Syntax.OrPattern
orPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.OrPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Param
param :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Annotation) -> Typed.TypedTerm Syntax.Param
param name annotation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.python.syntax.Param
paramAnnotation :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm (Maybe Syntax.Annotation)
paramAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamMaybeDefault
paramMaybeDefault :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm (Maybe Syntax.Default) -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamMaybeDefault
paramMaybeDefault param default_ typeComment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultDefault :: Typed.TypedTerm Syntax.ParamMaybeDefault -> Typed.TypedTerm (Maybe Syntax.Default)
paramMaybeDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultParam :: Typed.TypedTerm Syntax.ParamMaybeDefault -> Typed.TypedTerm Syntax.Param
paramMaybeDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultTypeComment :: Typed.TypedTerm Syntax.ParamMaybeDefault -> Typed.TypedTerm (Maybe Syntax.TypeComment)
paramMaybeDefaultTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithDefault :: Typed.TypedTerm Syntax.ParamMaybeDefault -> Typed.TypedTerm (Maybe Syntax.Default) -> Typed.TypedTerm Syntax.ParamMaybeDefault
paramMaybeDefaultWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the param field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithParam :: Typed.TypedTerm Syntax.ParamMaybeDefault -> Typed.TypedTerm Syntax.Param -> Typed.TypedTerm Syntax.ParamMaybeDefault
paramMaybeDefaultWithParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithTypeComment :: Typed.TypedTerm Syntax.ParamMaybeDefault -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamMaybeDefault
paramMaybeDefaultWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.Param
paramName :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm Syntax.Name
paramName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefault
paramNoDefault :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamNoDefault
paramNoDefault param typeComment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)}]}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamNoDefault
paramNoDefaultParam :: Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm Syntax.Param
paramNoDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParameters :: Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParameters paramNoDefault paramWithDefault starEtc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersParamNoDefault :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamNoDefault]
paramNoDefaultParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersParamWithDefault :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault]
paramNoDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersStarEtc :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc)
paramNoDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamNoDefault :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamWithDefault :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithStarEtc :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithStarEtc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotation :: Typed.TypedTerm Syntax.ParamStarAnnotation -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotation paramStarAnnotation typeComment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramStarAnnotation"),
          Core.fieldTerm = (Typed.unTypedTerm paramStarAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)}]}))
-- | DSL accessor for the paramStarAnnotation field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationParamStarAnnotation :: Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation -> Typed.TypedTerm Syntax.ParamStarAnnotation
paramNoDefaultStarAnnotationParamStarAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
        Core.projectionFieldName = (Core.Name "paramStarAnnotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationTypeComment :: Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation -> Typed.TypedTerm (Maybe Syntax.TypeComment)
paramNoDefaultStarAnnotationTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramStarAnnotation field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithParamStarAnnotation :: Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation -> Typed.TypedTerm Syntax.ParamStarAnnotation -> Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithParamStarAnnotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramStarAnnotation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithTypeComment :: Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramStarAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
              Core.projectionFieldName = (Core.Name "paramStarAnnotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamNoDefault
paramNoDefaultTypeComment :: Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm (Maybe Syntax.TypeComment)
paramNoDefaultTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the param field of hydra.python.syntax.ParamNoDefault
paramNoDefaultWithParam :: Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm Syntax.Param -> Typed.TypedTerm Syntax.ParamNoDefault
paramNoDefaultWithParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamNoDefault
paramNoDefaultWithTypeComment :: Typed.TypedTerm Syntax.ParamNoDefault -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamNoDefault
paramNoDefaultWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ParamStarAnnotation
paramStarAnnotation :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.StarAnnotation -> Typed.TypedTerm Syntax.ParamStarAnnotation
paramStarAnnotation name annotation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationAnnotation :: Typed.TypedTerm Syntax.ParamStarAnnotation -> Typed.TypedTerm Syntax.StarAnnotation
paramStarAnnotationAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationName :: Typed.TypedTerm Syntax.ParamStarAnnotation -> Typed.TypedTerm Syntax.Name
paramStarAnnotationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotation field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationWithAnnotation :: Typed.TypedTerm Syntax.ParamStarAnnotation -> Typed.TypedTerm Syntax.StarAnnotation -> Typed.TypedTerm Syntax.ParamStarAnnotation
paramStarAnnotationWithAnnotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationWithName :: Typed.TypedTerm Syntax.ParamStarAnnotation -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ParamStarAnnotation
paramStarAnnotationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
              Core.projectionFieldName = (Core.Name "annotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the annotation field of hydra.python.syntax.Param
paramWithAnnotation :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm (Maybe Syntax.Annotation) -> Typed.TypedTerm Syntax.Param
paramWithAnnotation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ParamWithDefault
paramWithDefault :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm Syntax.Default -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamWithDefault
paramWithDefault param default_ typeComment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.ParamWithDefault
paramWithDefaultDefault :: Typed.TypedTerm Syntax.ParamWithDefault -> Typed.TypedTerm Syntax.Default
paramWithDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamWithDefault
paramWithDefaultParam :: Typed.TypedTerm Syntax.ParamWithDefault -> Typed.TypedTerm Syntax.Param
paramWithDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParameters :: Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.ParamWithDefaultParameters
paramWithDefaultParameters paramWithDefault starEtc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm starEtc)}]}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersParamWithDefault :: Typed.TypedTerm Syntax.ParamWithDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault]
paramWithDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersStarEtc :: Typed.TypedTerm Syntax.ParamWithDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc)
paramWithDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithParamWithDefault :: Typed.TypedTerm Syntax.ParamWithDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm Syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithStarEtc :: Typed.TypedTerm Syntax.ParamWithDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithStarEtc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamWithDefault
paramWithDefaultTypeComment :: Typed.TypedTerm Syntax.ParamWithDefault -> Typed.TypedTerm (Maybe Syntax.TypeComment)
paramWithDefaultTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithDefault :: Typed.TypedTerm Syntax.ParamWithDefault -> Typed.TypedTerm Syntax.Default -> Typed.TypedTerm Syntax.ParamWithDefault
paramWithDefaultWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the param field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithParam :: Typed.TypedTerm Syntax.ParamWithDefault -> Typed.TypedTerm Syntax.Param -> Typed.TypedTerm Syntax.ParamWithDefault
paramWithDefaultWithParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithTypeComment :: Typed.TypedTerm Syntax.ParamWithDefault -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.ParamWithDefault
paramWithDefaultWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.Param
paramWithName :: Typed.TypedTerm Syntax.Param -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Param
paramWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
              Core.projectionFieldName = (Core.Name "annotation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the paramNoDefault variant of hydra.python.syntax.Parameters
parametersParamNoDefault :: Typed.TypedTerm Syntax.ParamNoDefaultParameters -> Typed.TypedTerm Syntax.Parameters
parametersParamNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the paramWithDefault variant of hydra.python.syntax.Parameters
parametersParamWithDefault :: Typed.TypedTerm Syntax.ParamWithDefaultParameters -> Typed.TypedTerm Syntax.Parameters
parametersParamWithDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramWithDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slashNoDefault variant of hydra.python.syntax.Parameters
parametersSlashNoDefault :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm Syntax.Parameters
parametersSlashNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slashWithDefault variant of hydra.python.syntax.Parameters
parametersSlashWithDefault :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm Syntax.Parameters
parametersSlashWithDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashWithDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starEtc variant of hydra.python.syntax.Parameters
parametersStarEtc :: Typed.TypedTerm Syntax.StarEtc -> Typed.TypedTerm Syntax.Parameters
parametersStarEtc x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starEtc"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the as variant of hydra.python.syntax.Pattern
patternAs :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.Pattern
patternAs x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.PatternCaptureTarget wrapper
patternCaptureTarget :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.PatternCaptureTarget
patternCaptureTarget x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.PatternCaptureTarget"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the or variant of hydra.python.syntax.Pattern
patternOr :: Typed.TypedTerm Syntax.OrPattern -> Typed.TypedTerm Syntax.Pattern
patternOr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.python.syntax.Patterns
patternsPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Patterns
patternsPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Patterns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequence variant of hydra.python.syntax.Patterns
patternsSequence :: Typed.TypedTerm Syntax.OpenSequencePattern -> Typed.TypedTerm Syntax.Patterns
patternsSequence x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Patterns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the minus variant of hydra.python.syntax.PlusOrMinus
plusOrMinusMinus :: Typed.TypedTerm Syntax.PlusOrMinus
plusOrMinusMinus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PlusOrMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.python.syntax.PlusOrMinus
plusOrMinusPlus :: Typed.TypedTerm Syntax.PlusOrMinus
plusOrMinusPlus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PlusOrMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assignment variant of hydra.python.syntax.PosArg
posArgAssignment :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.PosArg
posArgAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.PosArg
posArgExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.PosArg
posArgExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.PosArg
posArgStarred :: Typed.TypedTerm Syntax.StarredExpression -> Typed.TypedTerm Syntax.PosArg
posArgStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.PositionalPatterns wrapper
positionalPatterns :: Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.PositionalPatterns
positionalPatterns x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.PositionalPatterns"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Power
power :: Typed.TypedTerm Syntax.AwaitPrimary -> Typed.TypedTerm (Maybe Syntax.Factor) -> Typed.TypedTerm Syntax.Power
power lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Power"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Power
powerLhs :: Typed.TypedTerm Syntax.Power -> Typed.TypedTerm Syntax.AwaitPrimary
powerLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Power
powerRhs :: Typed.TypedTerm Syntax.Power -> Typed.TypedTerm (Maybe Syntax.Factor)
powerRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Power
powerWithLhs :: Typed.TypedTerm Syntax.Power -> Typed.TypedTerm Syntax.AwaitPrimary -> Typed.TypedTerm Syntax.Power
powerWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Power"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Power
powerWithRhs :: Typed.TypedTerm Syntax.Power -> Typed.TypedTerm (Maybe Syntax.Factor) -> Typed.TypedTerm Syntax.Power
powerWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Power"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the compound variant of hydra.python.syntax.Primary
primaryCompound :: Typed.TypedTerm Syntax.PrimaryWithRhs -> Typed.TypedTerm Syntax.Primary
primaryCompound x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the call variant of hydra.python.syntax.PrimaryRhs
primaryRhsCall :: Typed.TypedTerm Syntax.Args -> Typed.TypedTerm Syntax.PrimaryRhs
primaryRhsCall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the genexp variant of hydra.python.syntax.PrimaryRhs
primaryRhsGenexp :: Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.PrimaryRhs
primaryRhsGenexp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genexp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.python.syntax.PrimaryRhs
primaryRhsProject :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.PrimaryRhs
primaryRhsProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slices variant of hydra.python.syntax.PrimaryRhs
primaryRhsSlices :: Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm Syntax.PrimaryRhs
primaryRhsSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Primary
primarySimple :: Typed.TypedTerm Syntax.Atom -> Typed.TypedTerm Syntax.Primary
primarySimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.PrimaryWithRhs
primaryWithRhs :: Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.PrimaryRhs -> Typed.TypedTerm Syntax.PrimaryWithRhs
primaryWithRhs primary rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the primary field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsPrimary :: Typed.TypedTerm Syntax.PrimaryWithRhs -> Typed.TypedTerm Syntax.Primary
primaryWithRhsPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsRhs :: Typed.TypedTerm Syntax.PrimaryWithRhs -> Typed.TypedTerm Syntax.PrimaryRhs
primaryWithRhsRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the primary field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsWithPrimary :: Typed.TypedTerm Syntax.PrimaryWithRhs -> Typed.TypedTerm Syntax.Primary -> Typed.TypedTerm Syntax.PrimaryWithRhs
primaryWithRhsWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsWithRhs :: Typed.TypedTerm Syntax.PrimaryWithRhs -> Typed.TypedTerm Syntax.PrimaryRhs -> Typed.TypedTerm Syntax.PrimaryWithRhs
primaryWithRhsWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the double variant of hydra.python.syntax.QuoteStyle
quoteStyleDouble :: Typed.TypedTerm Syntax.QuoteStyle
quoteStyleDouble =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the single variant of hydra.python.syntax.QuoteStyle
quoteStyleSingle :: Typed.TypedTerm Syntax.QuoteStyle
quoteStyleSingle =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tripleDouble variant of hydra.python.syntax.QuoteStyle
quoteStyleTripleDouble :: Typed.TypedTerm Syntax.QuoteStyle
quoteStyleTripleDouble =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tripleDouble"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tripleSingle variant of hydra.python.syntax.QuoteStyle
quoteStyleTripleSingle :: Typed.TypedTerm Syntax.QuoteStyle
quoteStyleTripleSingle =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tripleSingle"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.RaiseExpression
raiseExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.RaiseExpression
raiseExpression expression from =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Typed.unTypedTerm from)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.RaiseExpression
raiseExpressionExpression :: Typed.TypedTerm Syntax.RaiseExpression -> Typed.TypedTerm Syntax.Expression
raiseExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the from field of hydra.python.syntax.RaiseExpression
raiseExpressionFrom :: Typed.TypedTerm Syntax.RaiseExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
raiseExpressionFrom x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
        Core.projectionFieldName = (Core.Name "from")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.RaiseExpression
raiseExpressionWithExpression :: Typed.TypedTerm Syntax.RaiseExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.RaiseExpression
raiseExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
              Core.projectionFieldName = (Core.Name "from")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the from field of hydra.python.syntax.RaiseExpression
raiseExpressionWithFrom :: Typed.TypedTerm Syntax.RaiseExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.RaiseExpression
raiseExpressionWithFrom original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.RaiseStatement wrapper
raiseStatement :: Typed.TypedTerm (Maybe Syntax.RaiseExpression) -> Typed.TypedTerm Syntax.RaiseStatement
raiseStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.RaiseStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the float variant of hydra.python.syntax.RealNumber
realNumberFloat :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.RealNumber
realNumberFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.python.syntax.RealNumber
realNumberInteger :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.RealNumber
realNumberInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dot variant of hydra.python.syntax.RelativeImportPrefix
relativeImportPrefixDot :: Typed.TypedTerm Syntax.RelativeImportPrefix
relativeImportPrefixDot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RelativeImportPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the ellipsis variant of hydra.python.syntax.RelativeImportPrefix
relativeImportPrefixEllipsis :: Typed.TypedTerm Syntax.RelativeImportPrefix
relativeImportPrefixEllipsis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RelativeImportPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ellipsis"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.python.syntax.ReturnStatement wrapper
returnStatement :: Typed.TypedTerm [Syntax.StarExpression] -> Typed.TypedTerm Syntax.ReturnStatement
returnStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the list variant of hydra.python.syntax.SequencePattern
sequencePatternList :: Typed.TypedTerm (Maybe Syntax.MaybeSequencePattern) -> Typed.TypedTerm Syntax.SequencePattern
sequencePatternList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SequencePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.SequencePattern
sequencePatternTuple :: Typed.TypedTerm (Maybe Syntax.OpenSequencePattern) -> Typed.TypedTerm Syntax.SequencePattern
sequencePatternTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SequencePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Set wrapper
set :: Typed.TypedTerm [Syntax.StarNamedExpression] -> Typed.TypedTerm Syntax.Set
set x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Set"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Setcomp
setcomp :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Setcomp
setcomp expression forIfClauses =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Typed.unTypedTerm forIfClauses)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.Setcomp
setcompExpression :: Typed.TypedTerm Syntax.Setcomp -> Typed.TypedTerm Syntax.NamedExpression
setcompExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Setcomp
setcompForIfClauses :: Typed.TypedTerm Syntax.Setcomp -> Typed.TypedTerm Syntax.ForIfClauses
setcompForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
        Core.projectionFieldName = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.Setcomp
setcompWithExpression :: Typed.TypedTerm Syntax.Setcomp -> Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Setcomp
setcompWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
              Core.projectionFieldName = (Core.Name "forIfClauses")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Setcomp
setcompWithForIfClauses :: Typed.TypedTerm Syntax.Setcomp -> Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm Syntax.Setcomp
setcompWithForIfClauses original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ShiftExpression
shiftExpression :: Typed.TypedTerm (Maybe Syntax.ShiftLhs) -> Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpression lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.ShiftExpression
shiftExpressionLhs :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm (Maybe Syntax.ShiftLhs)
shiftExpressionLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.ShiftExpression
shiftExpressionRhs :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.Sum
shiftExpressionRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.ShiftExpression
shiftExpressionWithLhs :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm (Maybe Syntax.ShiftLhs) -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpressionWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.ShiftExpression
shiftExpressionWithRhs :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm Syntax.ShiftExpression
shiftExpressionWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ShiftLhs
shiftLhs :: Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.ShiftOp -> Typed.TypedTerm Syntax.ShiftLhs
shiftLhs operand operator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm operand)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)}]}))
-- | DSL accessor for the operand field of hydra.python.syntax.ShiftLhs
shiftLhsOperand :: Typed.TypedTerm Syntax.ShiftLhs -> Typed.TypedTerm Syntax.ShiftExpression
shiftLhsOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.ShiftLhs
shiftLhsOperator :: Typed.TypedTerm Syntax.ShiftLhs -> Typed.TypedTerm Syntax.ShiftOp
shiftLhsOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.ShiftLhs
shiftLhsWithOperand :: Typed.TypedTerm Syntax.ShiftLhs -> Typed.TypedTerm Syntax.ShiftExpression -> Typed.TypedTerm Syntax.ShiftLhs
shiftLhsWithOperand original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.python.syntax.ShiftLhs
shiftLhsWithOperator :: Typed.TypedTerm Syntax.ShiftLhs -> Typed.TypedTerm Syntax.ShiftOp -> Typed.TypedTerm Syntax.ShiftLhs
shiftLhsWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the left variant of hydra.python.syntax.ShiftOp
shiftOpLeft :: Typed.TypedTerm Syntax.ShiftOp
shiftOpLeft =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ShiftOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "left"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the right variant of hydra.python.syntax.ShiftOp
shiftOpRight :: Typed.TypedTerm Syntax.ShiftOp
shiftOpRight =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ShiftOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "right"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.SignedNumber
signedNumberNumber :: Typed.TypedTerm Syntax.Number -> Typed.TypedTerm Syntax.SignedNumber
signedNumberNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sign variant of hydra.python.syntax.SignedNumber
signedNumberSign :: Typed.TypedTerm Syntax.PlusOrMinus -> Typed.TypedTerm Syntax.SignedNumber
signedNumberSign x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sign"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the number variant of hydra.python.syntax.SignedRealNumber
signedRealNumberNumber :: Typed.TypedTerm Syntax.RealNumber -> Typed.TypedTerm Syntax.SignedRealNumber
signedRealNumberNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedRealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sign variant of hydra.python.syntax.SignedRealNumber
signedRealNumberSign :: Typed.TypedTerm Syntax.PlusOrMinus -> Typed.TypedTerm Syntax.SignedRealNumber
signedRealNumberSign x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedRealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sign"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assert variant of hydra.python.syntax.SimpleStatement
simpleStatementAssert :: Typed.TypedTerm Syntax.AssertStatement -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementAssert x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assignment variant of hydra.python.syntax.SimpleStatement
simpleStatementAssignment :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the break variant of hydra.python.syntax.SimpleStatement
simpleStatementBreak :: Typed.TypedTerm Syntax.SimpleStatement
simpleStatementBreak =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the continue variant of hydra.python.syntax.SimpleStatement
simpleStatementContinue :: Typed.TypedTerm Syntax.SimpleStatement
simpleStatementContinue =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the del variant of hydra.python.syntax.SimpleStatement
simpleStatementDel :: Typed.TypedTerm Syntax.DelStatement -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementDel x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "del"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the global variant of hydra.python.syntax.SimpleStatement
simpleStatementGlobal :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementGlobal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the import variant of hydra.python.syntax.SimpleStatement
simpleStatementImport :: Typed.TypedTerm Syntax.ImportStatement -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonlocal variant of hydra.python.syntax.SimpleStatement
simpleStatementNonlocal :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementNonlocal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonlocal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pass variant of hydra.python.syntax.SimpleStatement
simpleStatementPass :: Typed.TypedTerm Syntax.SimpleStatement
simpleStatementPass =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pass"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the raise variant of hydra.python.syntax.SimpleStatement
simpleStatementRaise :: Typed.TypedTerm Syntax.RaiseStatement -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementRaise x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raise"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.python.syntax.SimpleStatement
simpleStatementReturn :: Typed.TypedTerm Syntax.ReturnStatement -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starExpressions variant of hydra.python.syntax.SimpleStatement
simpleStatementStarExpressions :: Typed.TypedTerm [Syntax.StarExpression] -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementStarExpressions x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starExpressions"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeAlias variant of hydra.python.syntax.SimpleStatement
simpleStatementTypeAlias :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementTypeAlias x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAlias"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.python.syntax.SimpleStatement
simpleStatementYield :: Typed.TypedTerm Syntax.YieldStatement -> Typed.TypedTerm Syntax.SimpleStatement
simpleStatementYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.SimpleTypeParameter
simpleTypeParameter :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SimpleTypeParameter
simpleTypeParameter name bound default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Typed.unTypedTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the bound field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterBound :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm (Maybe Syntax.Expression)
simpleTypeParameterBound x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionFieldName = (Core.Name "bound")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterDefault :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm (Maybe Syntax.Expression)
simpleTypeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterName :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm Syntax.Name
simpleTypeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bound field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithBound :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SimpleTypeParameter
simpleTypeParameterWithBound original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the default field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithDefault :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SimpleTypeParameter
simpleTypeParameterWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionFieldName = (Core.Name "bound")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithName :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.SimpleTypeParameter
simpleTypeParameterWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionFieldName = (Core.Name "bound")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndName :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleSubscriptAttributeTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndSlices :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleSubscriptAttributeTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.SingleTarget
singleTargetName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.SingleTarget
singleTargetName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.python.syntax.SingleTarget
singleTargetParens :: Typed.TypedTerm Syntax.SingleTarget -> Typed.TypedTerm Syntax.SingleTarget
singleTargetParens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the subscriptAttributeTarget variant of hydra.python.syntax.SingleTarget
singleTargetSubscriptAttributeTarget :: Typed.TypedTerm Syntax.SingleSubscriptAttributeTarget -> Typed.TypedTerm Syntax.SingleTarget
singleTargetSubscriptAttributeTarget x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subscriptAttributeTarget"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.SlashNoDefault wrapper
slashNoDefault :: Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm Syntax.SlashNoDefault
slashNoDefault x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.SlashNoDefault"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParameters :: Typed.TypedTerm Syntax.SlashNoDefault -> Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParameters slash paramNoDefault paramWithDefault starEtc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Typed.unTypedTerm slash)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersParamNoDefault :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamNoDefault]
slashNoDefaultParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersParamWithDefault :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault]
slashNoDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slash field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersSlash :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm Syntax.SlashNoDefault
slashNoDefaultParametersSlash x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "slash")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersStarEtc :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc)
slashNoDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamNoDefault :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "slash")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamWithDefault :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "slash")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the slash field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithSlash :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm Syntax.SlashNoDefault -> Typed.TypedTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithSlash original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithStarEtc :: Typed.TypedTerm Syntax.SlashNoDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithStarEtc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "slash")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.SlashWithDefault
slashWithDefault :: Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm Syntax.SlashWithDefault
slashWithDefault paramNoDefault paramWithDefault =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultParamNoDefault :: Typed.TypedTerm Syntax.SlashWithDefault -> Typed.TypedTerm [Syntax.ParamNoDefault]
slashWithDefaultParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultParamWithDefault :: Typed.TypedTerm Syntax.SlashWithDefault -> Typed.TypedTerm [Syntax.ParamWithDefault]
slashWithDefaultParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParameters :: Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParameters paramNoDefault paramWithDefault starEtc =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersParamNoDefault :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm [Syntax.ParamNoDefault]
slashWithDefaultParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersParamWithDefault :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault]
slashWithDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersStarEtc :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc)
slashWithDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamNoDefault :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamWithDefault :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "starEtc")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithStarEtc :: Typed.TypedTerm Syntax.SlashWithDefaultParameters -> Typed.TypedTerm (Maybe Syntax.StarEtc) -> Typed.TypedTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithStarEtc original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultWithParamNoDefault :: Typed.TypedTerm Syntax.SlashWithDefault -> Typed.TypedTerm [Syntax.ParamNoDefault] -> Typed.TypedTerm Syntax.SlashWithDefault
slashWithDefaultWithParamNoDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
              Core.projectionFieldName = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultWithParamWithDefault :: Typed.TypedTerm Syntax.SlashWithDefault -> Typed.TypedTerm [Syntax.ParamWithDefault] -> Typed.TypedTerm Syntax.SlashWithDefault
slashWithDefaultWithParamWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
              Core.projectionFieldName = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.SliceExpression
sliceExpression :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SliceExpression
sliceExpression start stop step =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Typed.unTypedTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Typed.unTypedTerm stop)},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Typed.unTypedTerm step)}]}))
-- | DSL accessor for the start field of hydra.python.syntax.SliceExpression
sliceExpressionStart :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
sliceExpressionStart x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionFieldName = (Core.Name "start")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the step field of hydra.python.syntax.SliceExpression
sliceExpressionStep :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
sliceExpressionStep x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionFieldName = (Core.Name "step")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the stop field of hydra.python.syntax.SliceExpression
sliceExpressionStop :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm (Maybe Syntax.Expression)
sliceExpressionStop x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionFieldName = (Core.Name "stop")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the start field of hydra.python.syntax.SliceExpression
sliceExpressionWithStart :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SliceExpression
sliceExpressionWithStart original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionFieldName = (Core.Name "stop")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionFieldName = (Core.Name "step")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the step field of hydra.python.syntax.SliceExpression
sliceExpressionWithStep :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SliceExpression
sliceExpressionWithStep original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionFieldName = (Core.Name "start")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionFieldName = (Core.Name "stop")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the stop field of hydra.python.syntax.SliceExpression
sliceExpressionWithStop :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SliceExpression
sliceExpressionWithStop original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionFieldName = (Core.Name "start")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionFieldName = (Core.Name "step")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the named variant of hydra.python.syntax.Slice
sliceNamed :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Slice
sliceNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice variant of hydra.python.syntax.SliceOrStarredExpression
sliceOrStarredExpressionSlice :: Typed.TypedTerm Syntax.Slice -> Typed.TypedTerm Syntax.SliceOrStarredExpression
sliceOrStarredExpressionSlice x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SliceOrStarredExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.SliceOrStarredExpression
sliceOrStarredExpressionStarred :: Typed.TypedTerm Syntax.StarredExpression -> Typed.TypedTerm Syntax.SliceOrStarredExpression
sliceOrStarredExpressionStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SliceOrStarredExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice_ variant of hydra.python.syntax.Slice
sliceSlice_ :: Typed.TypedTerm Syntax.SliceExpression -> Typed.TypedTerm Syntax.Slice
sliceSlice_ x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice_"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Slices
slices :: Typed.TypedTerm Syntax.Slice -> Typed.TypedTerm [Syntax.SliceOrStarredExpression] -> Typed.TypedTerm Syntax.Slices
slices head tail =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Slices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm tail)}]}))
-- | DSL accessor for the head field of hydra.python.syntax.Slices
slicesHead :: Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm Syntax.Slice
slicesHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tail field of hydra.python.syntax.Slices
slicesTail :: Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm [Syntax.SliceOrStarredExpression]
slicesTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.Slices
slicesWithHead :: Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm Syntax.Slice -> Typed.TypedTerm Syntax.Slices
slicesWithHead original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Slices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
              Core.projectionFieldName = (Core.Name "tail")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.python.syntax.Slices
slicesWithTail :: Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm [Syntax.SliceOrStarredExpression] -> Typed.TypedTerm Syntax.Slices
slicesWithTail original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Slices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.StarAnnotation wrapper
starAnnotation :: Typed.TypedTerm Syntax.StarExpression -> Typed.TypedTerm Syntax.StarAnnotation
starAnnotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarAnnotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the name variant of hydra.python.syntax.StarAtom
starAtomName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.StarAtom
starAtomName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starTargetsListSeq variant of hydra.python.syntax.StarAtom
starAtomStarTargetsListSeq :: Typed.TypedTerm (Maybe Syntax.StarTargetsListSeq) -> Typed.TypedTerm Syntax.StarAtom
starAtomStarTargetsListSeq x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starTargetsListSeq"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starTargetsTupleSeq variant of hydra.python.syntax.StarAtom
starAtomStarTargetsTupleSeq :: Typed.TypedTerm (Maybe Syntax.StarTargetsTupleSeq) -> Typed.TypedTerm Syntax.StarAtom
starAtomStarTargetsTupleSeq x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starTargetsTupleSeq"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the targetWithStarAtom variant of hydra.python.syntax.StarAtom
starAtomTargetWithStarAtom :: Typed.TypedTerm Syntax.TargetWithStarAtom -> Typed.TypedTerm Syntax.StarAtom
starAtomTargetWithStarAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetWithStarAtom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the keywords variant of hydra.python.syntax.StarEtc
starEtcKeywords :: Typed.TypedTerm Syntax.Keywords -> Typed.TypedTerm Syntax.StarEtc
starEtcKeywords x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keywords"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starComma variant of hydra.python.syntax.StarEtc
starEtcStarComma :: Typed.TypedTerm Syntax.CommaStarEtc -> Typed.TypedTerm Syntax.StarEtc
starEtcStarComma x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starComma"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starNoDefault variant of hydra.python.syntax.StarEtc
starEtcStarNoDefault :: Typed.TypedTerm Syntax.NoDefaultStarEtc -> Typed.TypedTerm Syntax.StarEtc
starEtcStarNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starNoDefaultStarAnnotation variant of hydra.python.syntax.StarEtc
starEtcStarNoDefaultStarAnnotation :: Typed.TypedTerm Syntax.NoDefaultStarAnnotationStarEtc -> Typed.TypedTerm Syntax.StarEtc
starEtcStarNoDefaultStarAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starNoDefaultStarAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.StarExpression
starExpressionSimple :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.StarExpression
starExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.StarExpression
starExpressionStar :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.StarExpression
starExpressionStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.StarNamedExpression
starNamedExpressionSimple :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.StarNamedExpression
starNamedExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarNamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.StarNamedExpression
starNamedExpressionStar :: Typed.TypedTerm Syntax.BitwiseOr -> Typed.TypedTerm Syntax.StarNamedExpression
starNamedExpressionStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarNamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.StarNamedExpressions wrapper
starNamedExpressions :: Typed.TypedTerm [Syntax.StarNamedExpression] -> Typed.TypedTerm Syntax.StarNamedExpressions
starNamedExpressions x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarNamedExpressions"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the capture variant of hydra.python.syntax.StarPattern
starPatternCapture :: Typed.TypedTerm Syntax.PatternCaptureTarget -> Typed.TypedTerm Syntax.StarPattern
starPatternCapture x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "capture"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.python.syntax.StarPattern
starPatternWildcard :: Typed.TypedTerm Syntax.StarPattern
starPatternWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the starred variant of hydra.python.syntax.StarTarget
starTargetStarred :: Typed.TypedTerm Syntax.StarTarget -> Typed.TypedTerm Syntax.StarTarget
starTargetStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unstarred variant of hydra.python.syntax.StarTarget
starTargetUnstarred :: Typed.TypedTerm Syntax.TargetWithStarAtom -> Typed.TypedTerm Syntax.StarTarget
starTargetUnstarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unstarred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.StarTargetsListSeq wrapper
starTargetsListSeq :: Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.StarTargetsListSeq
starTargetsListSeq x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarTargetsListSeq"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.StarTargetsTupleSeq wrapper
starTargetsTupleSeq :: Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.StarTargetsTupleSeq
starTargetsTupleSeq x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarTargetsTupleSeq"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.StarTypeParameter
starTypeParameter :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.StarExpression) -> Typed.TypedTerm Syntax.StarTypeParameter
starTypeParameter name default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.StarTypeParameter
starTypeParameterDefault :: Typed.TypedTerm Syntax.StarTypeParameter -> Typed.TypedTerm (Maybe Syntax.StarExpression)
starTypeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.StarTypeParameter
starTypeParameterName :: Typed.TypedTerm Syntax.StarTypeParameter -> Typed.TypedTerm Syntax.Name
starTypeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.StarTypeParameter
starTypeParameterWithDefault :: Typed.TypedTerm Syntax.StarTypeParameter -> Typed.TypedTerm (Maybe Syntax.StarExpression) -> Typed.TypedTerm Syntax.StarTypeParameter
starTypeParameterWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.StarTypeParameter
starTypeParameterWithName :: Typed.TypedTerm Syntax.StarTypeParameter -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.StarTypeParameter
starTypeParameterWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.StarredExpression wrapper
starredExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.StarredExpression
starredExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarredExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotated variant of hydra.python.syntax.Statement
statementAnnotated :: Typed.TypedTerm Syntax.AnnotatedStatement -> Typed.TypedTerm Syntax.Statement
statementAnnotated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the compound variant of hydra.python.syntax.Statement
statementCompound :: Typed.TypedTerm Syntax.CompoundStatement -> Typed.TypedTerm Syntax.Statement
statementCompound x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Statement
statementSimple :: Typed.TypedTerm [Syntax.SimpleStatement] -> Typed.TypedTerm Syntax.Statement
statementSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.String
string :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Syntax.StringPrefix) -> Typed.TypedTerm Syntax.QuoteStyle -> Typed.TypedTerm Syntax.String_
string value prefix quoteStyle =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Typed.unTypedTerm quoteStyle)}]}))
-- | DSL accessor for the prefix field of hydra.python.syntax.String
stringPrefix :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm (Maybe Syntax.StringPrefix)
stringPrefix x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the bytes variant of hydra.python.syntax.StringPrefix
stringPrefixBytes :: Typed.TypedTerm Syntax.StringPrefix
stringPrefixBytes =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the raw variant of hydra.python.syntax.StringPrefix
stringPrefixRaw :: Typed.TypedTerm Syntax.StringPrefix
stringPrefixRaw =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raw"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rawBytes variant of hydra.python.syntax.StringPrefix
stringPrefixRawBytes :: Typed.TypedTerm Syntax.StringPrefix
stringPrefixRawBytes =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rawBytes"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unicode variant of hydra.python.syntax.StringPrefix
stringPrefixUnicode :: Typed.TypedTerm Syntax.StringPrefix
stringPrefixUnicode =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unicode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the quoteStyle field of hydra.python.syntax.String
stringQuoteStyle :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.QuoteStyle
stringQuoteStyle x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionFieldName = (Core.Name "quoteStyle")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.String
stringValue :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm String
stringValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the prefix field of hydra.python.syntax.String
stringWithPrefix :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm (Maybe Syntax.StringPrefix) -> Typed.TypedTerm Syntax.String_
stringWithPrefix original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionFieldName = (Core.Name "quoteStyle")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the quoteStyle field of hydra.python.syntax.String
stringWithQuoteStyle :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm Syntax.QuoteStyle -> Typed.TypedTerm Syntax.String_
stringWithQuoteStyle original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.python.syntax.String
stringWithValue :: Typed.TypedTerm Syntax.String_ -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.String_
stringWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionFieldName = (Core.Name "quoteStyle")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the simple variant of hydra.python.syntax.SubjectExpression
subjectExpressionSimple :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.SubjectExpression
subjectExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SubjectExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.SubjectExpression
subjectExpressionTuple :: Typed.TypedTerm [Syntax.StarNamedExpression] -> Typed.TypedTerm Syntax.SubjectExpression
subjectExpressionTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SubjectExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Sum
sum :: Typed.TypedTerm (Maybe Syntax.SumLhs) -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Sum
sum lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Sum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Sum
sumLhs :: Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm (Maybe Syntax.SumLhs)
sumLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.SumLhs
sumLhs2 :: Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm Syntax.SumOp -> Typed.TypedTerm Syntax.SumLhs
sumLhs2 operand operator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm operand)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)}]}))
-- | DSL accessor for the operand field of hydra.python.syntax.SumLhs
sumLhsOperand :: Typed.TypedTerm Syntax.SumLhs -> Typed.TypedTerm Syntax.Sum
sumLhsOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.SumLhs
sumLhsOperator :: Typed.TypedTerm Syntax.SumLhs -> Typed.TypedTerm Syntax.SumOp
sumLhsOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.SumLhs
sumLhsWithOperand :: Typed.TypedTerm Syntax.SumLhs -> Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm Syntax.SumLhs
sumLhsWithOperand original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.python.syntax.SumLhs
sumLhsWithOperator :: Typed.TypedTerm Syntax.SumLhs -> Typed.TypedTerm Syntax.SumOp -> Typed.TypedTerm Syntax.SumLhs
sumLhsWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the add variant of hydra.python.syntax.SumOp
sumOpAdd :: Typed.TypedTerm Syntax.SumOp
sumOpAdd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SumOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sub variant of hydra.python.syntax.SumOp
sumOpSub :: Typed.TypedTerm Syntax.SumOp
sumOpSub =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SumOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sub"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Sum
sumRhs :: Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm Syntax.Term
sumRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Sum
sumWithLhs :: Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm (Maybe Syntax.SumLhs) -> Typed.TypedTerm Syntax.Sum
sumWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Sum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Sum
sumWithRhs :: Typed.TypedTerm Syntax.Sum -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Sum
sumWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Sum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArguments :: Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm (Maybe Syntax.Args) -> Typed.TypedTerm Syntax.TPrimaryAndArguments
tPrimaryAndArguments primary arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsArguments :: Typed.TypedTerm Syntax.TPrimaryAndArguments -> Typed.TypedTerm (Maybe Syntax.Args)
tPrimaryAndArgumentsArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsPrimary :: Typed.TypedTerm Syntax.TPrimaryAndArguments -> Typed.TypedTerm Syntax.TPrimary
tPrimaryAndArgumentsPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithArguments :: Typed.TypedTerm Syntax.TPrimaryAndArguments -> Typed.TypedTerm (Maybe Syntax.Args) -> Typed.TypedTerm Syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithPrimary :: Typed.TypedTerm Syntax.TPrimaryAndArguments -> Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexp :: Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.TPrimaryAndGenexp
tPrimaryAndGenexp primary genexp =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "genexp"),
          Core.fieldTerm = (Typed.unTypedTerm genexp)}]}))
-- | DSL accessor for the genexp field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpGenexp :: Typed.TypedTerm Syntax.TPrimaryAndGenexp -> Typed.TypedTerm Syntax.Genexp
tPrimaryAndGenexpGenexp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
        Core.projectionFieldName = (Core.Name "genexp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpPrimary :: Typed.TypedTerm Syntax.TPrimaryAndGenexp -> Typed.TypedTerm Syntax.TPrimary
tPrimaryAndGenexpPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the genexp field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithGenexp :: Typed.TypedTerm Syntax.TPrimaryAndGenexp -> Typed.TypedTerm Syntax.Genexp -> Typed.TypedTerm Syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithGenexp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "genexp"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithPrimary :: Typed.TypedTerm Syntax.TPrimaryAndGenexp -> Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "genexp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
              Core.projectionFieldName = (Core.Name "genexp")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndName
tPrimaryAndName :: Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.TPrimaryAndName
tPrimaryAndName primary name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameName :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.Name
tPrimaryAndNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNamePrimary :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.TPrimary
tPrimaryAndNamePrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameWithName :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.TPrimaryAndName
tPrimaryAndNameWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameWithPrimary :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.TPrimaryAndName
tPrimaryAndNameWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlices :: Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm Syntax.TPrimaryAndSlices
tPrimaryAndSlices primary slices =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "slices"),
          Core.fieldTerm = (Typed.unTypedTerm slices)}]}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesPrimary :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.TPrimary
tPrimaryAndSlicesPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slices field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesSlices :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.Slices
tPrimaryAndSlicesSlices x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
        Core.projectionFieldName = (Core.Name "slices")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithPrimary :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.TPrimary -> Typed.TypedTerm Syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithPrimary original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "slices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
              Core.projectionFieldName = (Core.Name "slices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the slices field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithSlices :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.Slices -> Typed.TypedTerm Syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithSlices original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
              Core.projectionFieldName = (Core.Name "primary")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "slices"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the atom variant of hydra.python.syntax.TPrimary
tPrimaryAtom :: Typed.TypedTerm Syntax.Atom -> Typed.TypedTerm Syntax.TPrimary
tPrimaryAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndArguments variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndArguments :: Typed.TypedTerm Syntax.TPrimaryAndArguments -> Typed.TypedTerm Syntax.TPrimary
tPrimaryPrimaryAndArguments x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndArguments"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndGenexp variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndGenexp :: Typed.TypedTerm Syntax.TPrimaryAndGenexp -> Typed.TypedTerm Syntax.TPrimary
tPrimaryPrimaryAndGenexp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndGenexp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndName :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.TPrimary
tPrimaryPrimaryAndName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndSlices :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.TPrimary
tPrimaryPrimaryAndSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the atom variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomAtom :: Typed.TypedTerm Syntax.StarAtom -> Typed.TypedTerm Syntax.TargetWithStarAtom
targetWithStarAtomAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomProject :: Typed.TypedTerm Syntax.TPrimaryAndName -> Typed.TypedTerm Syntax.TargetWithStarAtom
targetWithStarAtomProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slices variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomSlices :: Typed.TypedTerm Syntax.TPrimaryAndSlices -> Typed.TypedTerm Syntax.TargetWithStarAtom
targetWithStarAtomSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Term
term :: Typed.TypedTerm (Maybe Syntax.TermLhs) -> Typed.TypedTerm Syntax.Factor -> Typed.TypedTerm Syntax.Term
term lhs rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Term"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Term
termLhs :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm (Maybe Syntax.TermLhs)
termLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.TermLhs
termLhs2 :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.TermOp -> Typed.TypedTerm Syntax.TermLhs
termLhs2 operand operator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm operand)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)}]}))
-- | DSL accessor for the operand field of hydra.python.syntax.TermLhs
termLhsOperand :: Typed.TypedTerm Syntax.TermLhs -> Typed.TypedTerm Syntax.Term
termLhsOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.TermLhs
termLhsOperator :: Typed.TypedTerm Syntax.TermLhs -> Typed.TypedTerm Syntax.TermOp
termLhsOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.TermLhs
termLhsWithOperand :: Typed.TypedTerm Syntax.TermLhs -> Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.TermLhs
termLhsWithOperand original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.python.syntax.TermLhs
termLhsWithOperator :: Typed.TypedTerm Syntax.TermLhs -> Typed.TypedTerm Syntax.TermOp -> Typed.TypedTerm Syntax.TermLhs
termLhsWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the div variant of hydra.python.syntax.TermOp
termOpDiv :: Typed.TypedTerm Syntax.TermOp
termOpDiv =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the floordiv variant of hydra.python.syntax.TermOp
termOpFloordiv :: Typed.TypedTerm Syntax.TermOp
termOpFloordiv =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floordiv"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the matmul variant of hydra.python.syntax.TermOp
termOpMatmul :: Typed.TypedTerm Syntax.TermOp
termOpMatmul =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "matmul"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mod variant of hydra.python.syntax.TermOp
termOpMod :: Typed.TypedTerm Syntax.TermOp
termOpMod =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mul variant of hydra.python.syntax.TermOp
termOpMul :: Typed.TypedTerm Syntax.TermOp
termOpMul =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mul"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Term
termRhs :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Factor
termRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Term
termWithLhs :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm (Maybe Syntax.TermLhs) -> Typed.TypedTerm Syntax.Term
termWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Term"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Term
termWithRhs :: Typed.TypedTerm Syntax.Term -> Typed.TypedTerm Syntax.Factor -> Typed.TypedTerm Syntax.Term
termWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Term"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatement :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm [Syntax.ExceptStarBlock] -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.TryExceptStarStatement
tryExceptStarStatement body excepts else_ finally =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Typed.unTypedTerm excepts)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm finally)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementBody :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm Syntax.Block
tryExceptStarStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementElse :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm (Maybe Syntax.Block)
tryExceptStarStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the excepts field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementExcepts :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm [Syntax.ExceptStarBlock]
tryExceptStarStatementExcepts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "excepts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementFinally :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm (Maybe Syntax.Block)
tryExceptStarStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithBody :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "excepts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithElse :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "excepts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the excepts field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithExcepts :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm [Syntax.ExceptStarBlock] -> Typed.TypedTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithExcepts original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithFinally :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithFinally original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "excepts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TryExceptStatement
tryExceptStatement :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm [Syntax.ExceptBlock] -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.TryExceptStatement
tryExceptStatement body excepts else_ finally =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Typed.unTypedTerm excepts)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm finally)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.TryExceptStatement
tryExceptStatementBody :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm Syntax.Block
tryExceptStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.TryExceptStatement
tryExceptStatementElse :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm (Maybe Syntax.Block)
tryExceptStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the excepts field of hydra.python.syntax.TryExceptStatement
tryExceptStatementExcepts :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm [Syntax.ExceptBlock]
tryExceptStatementExcepts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "excepts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryExceptStatement
tryExceptStatementFinally :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm (Maybe Syntax.Block)
tryExceptStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithBody :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryExceptStatement
tryExceptStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "excepts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithElse :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.TryExceptStatement
tryExceptStatementWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "excepts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the excepts field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithExcepts :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm [Syntax.ExceptBlock] -> Typed.TypedTerm Syntax.TryExceptStatement
tryExceptStatementWithExcepts original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithFinally :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.TryExceptStatement
tryExceptStatementWithFinally original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "excepts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TryFinallyStatement
tryFinallyStatement :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryFinallyStatement
tryFinallyStatement body finally =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm finally)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementBody :: Typed.TypedTerm Syntax.TryFinallyStatement -> Typed.TypedTerm Syntax.Block
tryFinallyStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementFinally :: Typed.TypedTerm Syntax.TryFinallyStatement -> Typed.TypedTerm Syntax.Block
tryFinallyStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementWithBody :: Typed.TypedTerm Syntax.TryFinallyStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryFinallyStatement
tryFinallyStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
              Core.projectionFieldName = (Core.Name "finally")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementWithFinally :: Typed.TypedTerm Syntax.TryFinallyStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.TryFinallyStatement
tryFinallyStatementWithFinally original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the except variant of hydra.python.syntax.TryStatement
tryStatementExcept :: Typed.TypedTerm Syntax.TryExceptStatement -> Typed.TypedTerm Syntax.TryStatement
tryStatementExcept x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "except"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the exceptStar variant of hydra.python.syntax.TryStatement
tryStatementExceptStar :: Typed.TypedTerm Syntax.TryExceptStarStatement -> Typed.TypedTerm Syntax.TryStatement
tryStatementExceptStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exceptStar"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the finally variant of hydra.python.syntax.TryStatement
tryStatementFinally :: Typed.TypedTerm Syntax.TryFinallyStatement -> Typed.TypedTerm Syntax.TryStatement
tryStatementFinally x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finally"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Tuple wrapper
tuple :: Typed.TypedTerm [Syntax.StarNamedExpression] -> Typed.TypedTerm Syntax.Tuple
tuple x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Tuple"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.TypeAlias
typeAlias :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeAlias
typeAlias name typeParams expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.TypeAlias
typeAliasExpression :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm Syntax.Expression
typeAliasExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.TypeAlias
typeAliasName :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm Syntax.Name
typeAliasName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.TypeAlias
typeAliasTypeParams :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm [Syntax.TypeParameter]
typeAliasTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.TypeAlias
typeAliasWithExpression :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeAlias
typeAliasWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.TypeAlias
typeAliasWithName :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.TypeAlias
typeAliasWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.python.syntax.TypeAlias
typeAliasWithTypeParams :: Typed.TypedTerm Syntax.TypeAlias -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.TypeAlias
typeAliasWithTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.TypeComment wrapper
typeComment :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.TypeComment
typeComment x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.TypeComment"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the doubleStarredExpression variant of hydra.python.syntax.TypeExpression
typeExpressionDoubleStarredExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionDoubleStarredExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStarredExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.TypeExpression
typeExpressionExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starredExpression variant of hydra.python.syntax.TypeExpression
typeExpressionStarredExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionStarredExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starredExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the doubleStar variant of hydra.python.syntax.TypeParameter
typeParameterDoubleStar :: Typed.TypedTerm Syntax.DoubleStarTypeParameter -> Typed.TypedTerm Syntax.TypeParameter
typeParameterDoubleStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStar"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.TypeParameter
typeParameterSimple :: Typed.TypedTerm Syntax.SimpleTypeParameter -> Typed.TypedTerm Syntax.TypeParameter
typeParameterSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.TypeParameter
typeParameterStar :: Typed.TypedTerm Syntax.StarTypeParameter -> Typed.TypedTerm Syntax.TypeParameter
typeParameterStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.TypedAssignment
typedAssignment :: Typed.TypedTerm Syntax.SingleTarget -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.AnnotatedRhs) -> Typed.TypedTerm Syntax.TypedAssignment
typedAssignment lhs type_ rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.TypedAssignment
typedAssignmentLhs :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm Syntax.SingleTarget
typedAssignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.TypedAssignment
typedAssignmentRhs :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm (Maybe Syntax.AnnotatedRhs)
typedAssignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.python.syntax.TypedAssignment
typedAssignmentType :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm Syntax.Expression
typedAssignmentType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.TypedAssignment
typedAssignmentWithLhs :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm Syntax.SingleTarget -> Typed.TypedTerm Syntax.TypedAssignment
typedAssignmentWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.TypedAssignment
typedAssignmentWithRhs :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm (Maybe Syntax.AnnotatedRhs) -> Typed.TypedTerm Syntax.TypedAssignment
typedAssignmentWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.python.syntax.TypedAssignment
typedAssignmentWithType :: Typed.TypedTerm Syntax.TypedAssignment -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypedAssignment
typedAssignmentWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.python.syntax.Annotation
unAnnotation :: Typed.TypedTerm Syntax.Annotation -> Typed.TypedTerm Syntax.Expression
unAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Annotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Attribute
unAttribute :: Typed.TypedTerm Syntax.Attribute -> Typed.TypedTerm [Syntax.Name]
unAttribute x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Attribute")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.CapturePattern
unCapturePattern :: Typed.TypedTerm Syntax.CapturePattern -> Typed.TypedTerm Syntax.PatternCaptureTarget
unCapturePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.CapturePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Conjunction
unConjunction :: Typed.TypedTerm Syntax.Conjunction -> Typed.TypedTerm [Syntax.Inversion]
unConjunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Conjunction")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Decorators
unDecorators :: Typed.TypedTerm Syntax.Decorators -> Typed.TypedTerm [Syntax.NamedExpression]
unDecorators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Decorators")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Default
unDefault :: Typed.TypedTerm Syntax.Default -> Typed.TypedTerm Syntax.Expression
unDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Default")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DelStatement
unDelStatement :: Typed.TypedTerm Syntax.DelStatement -> Typed.TypedTerm Syntax.DelTargets
unDelStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DelStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DelTargets
unDelTargets :: Typed.TypedTerm Syntax.DelTargets -> Typed.TypedTerm [Syntax.DelTarget]
unDelTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DelTargets")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Dict
unDict :: Typed.TypedTerm Syntax.Dict -> Typed.TypedTerm [Syntax.DoubleStarredKvpair]
unDict x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Dict")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Disjunction
unDisjunction :: Typed.TypedTerm Syntax.Disjunction -> Typed.TypedTerm [Syntax.Conjunction]
unDisjunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Disjunction")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DottedName
unDottedName :: Typed.TypedTerm Syntax.DottedName -> Typed.TypedTerm [Syntax.Name]
unDottedName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DottedName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DoubleStarPattern
unDoubleStarPattern :: Typed.TypedTerm Syntax.DoubleStarPattern -> Typed.TypedTerm Syntax.PatternCaptureTarget
unDoubleStarPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DoubleStarPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Eval
unEval :: Typed.TypedTerm Syntax.Eval -> Typed.TypedTerm [Syntax.Expression]
unEval x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Eval")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.File
unFile :: Typed.TypedTerm Syntax.File -> Typed.TypedTerm [Syntax.Statement]
unFile x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.File")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ForIfClauses
unForIfClauses :: Typed.TypedTerm Syntax.ForIfClauses -> Typed.TypedTerm [Syntax.ForIfClause]
unForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ForIfClauses")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.FuncTypeComment
unFuncTypeComment :: Typed.TypedTerm Syntax.FuncTypeComment -> Typed.TypedTerm Syntax.TypeComment
unFuncTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.FuncTypeComment")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.GroupPattern
unGroupPattern :: Typed.TypedTerm Syntax.GroupPattern -> Typed.TypedTerm Syntax.Pattern
unGroupPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.GroupPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Guard
unGuard :: Typed.TypedTerm Syntax.Guard -> Typed.TypedTerm Syntax.NamedExpression
unGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Guard")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ImaginaryNumber
unImaginaryNumber :: Typed.TypedTerm Syntax.ImaginaryNumber -> Typed.TypedTerm Double
unImaginaryNumber x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ImaginaryNumber")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ImportName
unImportName :: Typed.TypedTerm Syntax.ImportName -> Typed.TypedTerm [Syntax.DottedAsName]
unImportName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ImportName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Interactive
unInteractive :: Typed.TypedTerm Syntax.Interactive -> Typed.TypedTerm Syntax.Statement
unInteractive x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Interactive")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ItemsPattern
unItemsPattern :: Typed.TypedTerm Syntax.ItemsPattern -> Typed.TypedTerm [Syntax.KeyValuePattern]
unItemsPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ItemsPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.KeywordPatterns
unKeywordPatterns :: Typed.TypedTerm Syntax.KeywordPatterns -> Typed.TypedTerm [Syntax.KeywordPattern]
unKeywordPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.KeywordPatterns")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Keywords
unKeywords :: Typed.TypedTerm Syntax.Keywords -> Typed.TypedTerm Syntax.ParamNoDefault
unKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Keywords")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.LambdaKwds
unLambdaKwds :: Typed.TypedTerm Syntax.LambdaKwds -> Typed.TypedTerm Syntax.LambdaParamNoDefault
unLambdaKwds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.LambdaKwds")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.LambdaParamNoDefault
unLambdaParamNoDefault :: Typed.TypedTerm Syntax.LambdaParamNoDefault -> Typed.TypedTerm Syntax.Name
unLambdaParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.LambdaParamNoDefault")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.List
unList :: Typed.TypedTerm Syntax.List -> Typed.TypedTerm [Syntax.StarNamedExpression]
unList x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.List")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.MaybeSequencePattern
unMaybeSequencePattern :: Typed.TypedTerm Syntax.MaybeSequencePattern -> Typed.TypedTerm [Syntax.MaybeStarPattern]
unMaybeSequencePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.MaybeSequencePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Module
unModule :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [[Syntax.Statement]]
unModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Module")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Name
unName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm String
unName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Name")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.NameOrAttribute
unNameOrAttribute :: Typed.TypedTerm Syntax.NameOrAttribute -> Typed.TypedTerm [Syntax.Name]
unNameOrAttribute x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.NameOrAttribute")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.OrPattern
unOrPattern :: Typed.TypedTerm Syntax.OrPattern -> Typed.TypedTerm [Syntax.ClosedPattern]
unOrPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.OrPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.PatternCaptureTarget
unPatternCaptureTarget :: Typed.TypedTerm Syntax.PatternCaptureTarget -> Typed.TypedTerm Syntax.Name
unPatternCaptureTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.PatternCaptureTarget")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.PositionalPatterns
unPositionalPatterns :: Typed.TypedTerm Syntax.PositionalPatterns -> Typed.TypedTerm [Syntax.Pattern]
unPositionalPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.PositionalPatterns")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.RaiseStatement
unRaiseStatement :: Typed.TypedTerm Syntax.RaiseStatement -> Typed.TypedTerm (Maybe Syntax.RaiseExpression)
unRaiseStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.RaiseStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ReturnStatement
unReturnStatement :: Typed.TypedTerm Syntax.ReturnStatement -> Typed.TypedTerm [Syntax.StarExpression]
unReturnStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ReturnStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Set
unSet :: Typed.TypedTerm Syntax.Set -> Typed.TypedTerm [Syntax.StarNamedExpression]
unSet x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Set")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.SlashNoDefault
unSlashNoDefault :: Typed.TypedTerm Syntax.SlashNoDefault -> Typed.TypedTerm [Syntax.ParamNoDefault]
unSlashNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.SlashNoDefault")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarAnnotation
unStarAnnotation :: Typed.TypedTerm Syntax.StarAnnotation -> Typed.TypedTerm Syntax.StarExpression
unStarAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarAnnotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarNamedExpressions
unStarNamedExpressions :: Typed.TypedTerm Syntax.StarNamedExpressions -> Typed.TypedTerm [Syntax.StarNamedExpression]
unStarNamedExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarNamedExpressions")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarTargetsListSeq
unStarTargetsListSeq :: Typed.TypedTerm Syntax.StarTargetsListSeq -> Typed.TypedTerm [Syntax.StarTarget]
unStarTargetsListSeq x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarTargetsListSeq")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarTargetsTupleSeq
unStarTargetsTupleSeq :: Typed.TypedTerm Syntax.StarTargetsTupleSeq -> Typed.TypedTerm [Syntax.StarTarget]
unStarTargetsTupleSeq x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarTargetsTupleSeq")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarredExpression
unStarredExpression :: Typed.TypedTerm Syntax.StarredExpression -> Typed.TypedTerm Syntax.Expression
unStarredExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarredExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Tuple
unTuple :: Typed.TypedTerm Syntax.Tuple -> Typed.TypedTerm [Syntax.StarNamedExpression]
unTuple x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Tuple")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.TypeComment
unTypeComment :: Typed.TypedTerm Syntax.TypeComment -> Typed.TypedTerm String
unTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.TypeComment")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ValuePattern
unValuePattern :: Typed.TypedTerm Syntax.ValuePattern -> Typed.TypedTerm Syntax.Attribute
unValuePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ValuePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.YieldStatement
unYieldStatement :: Typed.TypedTerm Syntax.YieldStatement -> Typed.TypedTerm Syntax.YieldExpression
unYieldStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.YieldStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.UntypedAssignment
untypedAssignment :: Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.AnnotatedRhs -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.UntypedAssignment
untypedAssignment targets rhs typeComment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm targets)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)}]}))
-- | DSL accessor for the rhs field of hydra.python.syntax.UntypedAssignment
untypedAssignmentRhs :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm Syntax.AnnotatedRhs
untypedAssignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.UntypedAssignment
untypedAssignmentTargets :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm [Syntax.StarTarget]
untypedAssignmentTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.UntypedAssignment
untypedAssignmentTypeComment :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm (Maybe Syntax.TypeComment)
untypedAssignmentTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the rhs field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithRhs :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm Syntax.AnnotatedRhs -> Typed.TypedTerm Syntax.UntypedAssignment
untypedAssignmentWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithTargets :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm [Syntax.StarTarget] -> Typed.TypedTerm Syntax.UntypedAssignment
untypedAssignmentWithTargets original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithTypeComment :: Typed.TypedTerm Syntax.UntypedAssignment -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.UntypedAssignment
untypedAssignmentWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionFieldName = (Core.Name "targets")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.ValuePattern wrapper
valuePattern :: Typed.TypedTerm Syntax.Attribute -> Typed.TypedTerm Syntax.ValuePattern
valuePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ValuePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.WhileStatement
whileStatement :: Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.WhileStatement
whileStatement condition body else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.WhileStatement
whileStatementBody :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Block
whileStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the condition field of hydra.python.syntax.WhileStatement
whileStatementCondition :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.NamedExpression
whileStatementCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.WhileStatement
whileStatementElse :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm (Maybe Syntax.Block)
whileStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.WhileStatement
whileStatementWithBody :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the condition field of hydra.python.syntax.WhileStatement
whileStatementWithCondition :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.NamedExpression -> Typed.TypedTerm Syntax.WhileStatement
whileStatementWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.WhileStatement
whileStatementWithElse :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm (Maybe Syntax.Block) -> Typed.TypedTerm Syntax.WhileStatement
whileStatementWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.WithItem
withItem :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm (Maybe Syntax.StarTarget) -> Typed.TypedTerm Syntax.WithItem
withItem expression as =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.WithItem
withItemAs :: Typed.TypedTerm Syntax.WithItem -> Typed.TypedTerm (Maybe Syntax.StarTarget)
withItemAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.WithItem
withItemExpression :: Typed.TypedTerm Syntax.WithItem -> Typed.TypedTerm Syntax.Expression
withItemExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.WithItem
withItemWithAs :: Typed.TypedTerm Syntax.WithItem -> Typed.TypedTerm (Maybe Syntax.StarTarget) -> Typed.TypedTerm Syntax.WithItem
withItemWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.WithItem
withItemWithExpression :: Typed.TypedTerm Syntax.WithItem -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.WithItem
withItemWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.WithStatement
withStatement :: Typed.TypedTerm Bool -> Typed.TypedTerm [Syntax.WithItem] -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.WithStatement
withStatement async items typeComment body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Typed.unTypedTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm typeComment)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.WithStatement
withStatementAsync :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm Bool
withStatementAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.WithStatement
withStatementBody :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm Syntax.Block
withStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the items field of hydra.python.syntax.WithStatement
withStatementItems :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm [Syntax.WithItem]
withStatementItems x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "items")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.WithStatement
withStatementTypeComment :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm (Maybe Syntax.TypeComment)
withStatementTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.WithStatement
withStatementWithAsync :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.WithStatement
withStatementWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "items")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.WithStatement
withStatementWithBody :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.WithStatement
withStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "items")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the items field of hydra.python.syntax.WithStatement
withStatementWithItems :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm [Syntax.WithItem] -> Typed.TypedTerm Syntax.WithStatement
withStatementWithItems original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "typeComment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.WithStatement
withStatementWithTypeComment :: Typed.TypedTerm Syntax.WithStatement -> Typed.TypedTerm (Maybe Syntax.TypeComment) -> Typed.TypedTerm Syntax.WithStatement
withStatementWithTypeComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "items")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the from variant of hydra.python.syntax.YieldExpression
yieldExpressionFrom :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.YieldExpression
yieldExpressionFrom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.YieldExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.YieldExpression
yieldExpressionSimple :: Typed.TypedTerm [Syntax.StarExpression] -> Typed.TypedTerm Syntax.YieldExpression
yieldExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.YieldExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.YieldStatement wrapper
yieldStatement :: Typed.TypedTerm Syntax.YieldExpression -> Typed.TypedTerm Syntax.YieldStatement
yieldStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.YieldStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
