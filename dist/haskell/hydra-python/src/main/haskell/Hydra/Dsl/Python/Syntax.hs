-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.python.syntax

module Hydra.Dsl.Python.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Python.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the star variant of hydra.python.syntax.AnnotatedRhs
annotatedRhsStar :: Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.AnnotatedRhs
annotatedRhsStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the yield variant of hydra.python.syntax.AnnotatedRhs
annotatedRhsYield :: Phantoms.TTerm Syntax.YieldExpression -> Phantoms.TTerm Syntax.AnnotatedRhs
annotatedRhsYield x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.AnnotatedStatement
annotatedStatement :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.AnnotatedStatement
annotatedStatement comment statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))
-- | DSL accessor for the comment field of hydra.python.syntax.AnnotatedStatement
annotatedStatementComment :: Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm String
annotatedStatementComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
        Core.projectionField = (Core.Name "comment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statement field of hydra.python.syntax.AnnotatedStatement
annotatedStatementStatement :: Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm Syntax.Statement
annotatedStatementStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
        Core.projectionField = (Core.Name "statement")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comment field of hydra.python.syntax.AnnotatedStatement
annotatedStatementWithComment :: Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.AnnotatedStatement
annotatedStatementWithComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
              Core.projectionField = (Core.Name "statement")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.python.syntax.AnnotatedStatement
annotatedStatementWithStatement :: Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.AnnotatedStatement
annotatedStatementWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
              Core.projectionField = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.Annotation wrapper
annotation :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Annotation
annotation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Annotation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Args
args :: Phantoms.TTerm [Syntax.PosArg] -> Phantoms.TTerm [Syntax.KwargOrStarred] -> Phantoms.TTerm [Syntax.KwargOrDoubleStarred] -> Phantoms.TTerm Syntax.Args
args positional kwargOrStarred kwargOrDoubleStarred =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Phantoms.unTTerm positional)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Phantoms.unTTerm kwargOrStarred)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Phantoms.unTTerm kwargOrDoubleStarred)}]}))
-- | DSL accessor for the kwargOrDoubleStarred field of hydra.python.syntax.Args
argsKwargOrDoubleStarred :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrDoubleStarred]
argsKwargOrDoubleStarred x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionField = (Core.Name "kwargOrDoubleStarred")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the kwargOrStarred field of hydra.python.syntax.Args
argsKwargOrStarred :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrStarred]
argsKwargOrStarred x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionField = (Core.Name "kwargOrStarred")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the positional field of hydra.python.syntax.Args
argsPositional :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.PosArg]
argsPositional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionField = (Core.Name "positional")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the kwargOrDoubleStarred field of hydra.python.syntax.Args
argsWithKwargOrDoubleStarred :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrDoubleStarred] -> Phantoms.TTerm Syntax.Args
argsWithKwargOrDoubleStarred original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionField = (Core.Name "positional")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionField = (Core.Name "kwargOrStarred")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the kwargOrStarred field of hydra.python.syntax.Args
argsWithKwargOrStarred :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.KwargOrStarred] -> Phantoms.TTerm Syntax.Args
argsWithKwargOrStarred original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionField = (Core.Name "positional")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionField = (Core.Name "kwargOrDoubleStarred")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the positional field of hydra.python.syntax.Args
argsWithPositional :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm [Syntax.PosArg] -> Phantoms.TTerm Syntax.Args
argsWithPositional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Args"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "positional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionField = (Core.Name "kwargOrStarred")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kwargOrDoubleStarred"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
              Core.projectionField = (Core.Name "kwargOrDoubleStarred")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.AsPattern
asPattern :: Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.AsPattern
asPattern pattern as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.AsPattern
asPatternAs :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget
asPatternAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pattern field of hydra.python.syntax.AsPattern
asPatternPattern :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.OrPattern
asPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.AsPattern
asPatternWithAs :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.AsPattern
asPatternWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.python.syntax.AsPattern
asPatternWithPattern :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm Syntax.AsPattern
asPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.AssertStatement
assertStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.AssertStatement
assertStatement expression1 expression2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression1"),
          Core.fieldTerm = (Phantoms.unTTerm expression1)},
        Core.Field {
          Core.fieldName = (Core.Name "expression2"),
          Core.fieldTerm = (Phantoms.unTTerm expression2)}]}))
-- | DSL accessor for the expression1 field of hydra.python.syntax.AssertStatement
assertStatementExpression1 :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.Expression
assertStatementExpression1 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
        Core.projectionField = (Core.Name "expression1")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression2 field of hydra.python.syntax.AssertStatement
assertStatementExpression2 :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
assertStatementExpression2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
        Core.projectionField = (Core.Name "expression2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression1 field of hydra.python.syntax.AssertStatement
assertStatementWithExpression1 :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssertStatement
assertStatementWithExpression1 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression1"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
              Core.projectionField = (Core.Name "expression2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the expression2 field of hydra.python.syntax.AssertStatement
assertStatementWithExpression2 :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.AssertStatement
assertStatementWithExpression2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression1"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
              Core.projectionField = (Core.Name "expression1")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the aug variant of hydra.python.syntax.Assignment
assignmentAug :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.Assignment
assignmentAug x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aug"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.AssignmentExpression
assignmentExpression :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpression name expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.AssignmentExpression
assignmentExpressionExpression :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression
assignmentExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.AssignmentExpression
assignmentExpressionName :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Name
assignmentExpressionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.AssignmentExpression
assignmentExpressionWithExpression :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.AssignmentExpression
assignmentExpressionWithName :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the typed variant of hydra.python.syntax.Assignment
assignmentTyped :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.Assignment
assignmentTyped x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the untyped variant of hydra.python.syntax.Assignment
assignmentUntyped :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm Syntax.Assignment
assignmentUntyped x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untyped"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the dict variant of hydra.python.syntax.Atom
atomDict :: Phantoms.TTerm Syntax.Dict -> Phantoms.TTerm Syntax.Atom
atomDict x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dict"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the dictcomp variant of hydra.python.syntax.Atom
atomDictcomp :: Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.Atom
atomDictcomp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dictcomp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ellipsis variant of hydra.python.syntax.Atom
atomEllipsis :: Phantoms.TTerm Syntax.Atom
atomEllipsis =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ellipsis"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the false variant of hydra.python.syntax.Atom
atomFalse :: Phantoms.TTerm Syntax.Atom
atomFalse =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "false"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the genexp variant of hydra.python.syntax.Atom
atomGenexp :: Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.Atom
atomGenexp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genexp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the group variant of hydra.python.syntax.Atom
atomGroup :: Phantoms.TTerm Syntax.Group -> Phantoms.TTerm Syntax.Atom
atomGroup x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.python.syntax.Atom
atomList :: Phantoms.TTerm Syntax.List -> Phantoms.TTerm Syntax.Atom
atomList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the listcomp variant of hydra.python.syntax.Atom
atomListcomp :: Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.Atom
atomListcomp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listcomp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.Atom
atomName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Atom
atomName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the none variant of hydra.python.syntax.Atom
atomNone :: Phantoms.TTerm Syntax.Atom
atomNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.Atom
atomNumber :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.Atom
atomNumber x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the set variant of hydra.python.syntax.Atom
atomSet :: Phantoms.TTerm Syntax.Set -> Phantoms.TTerm Syntax.Atom
atomSet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the setcomp variant of hydra.python.syntax.Atom
atomSetcomp :: Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.Atom
atomSetcomp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setcomp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.python.syntax.Atom
atomString :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.Atom
atomString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the true variant of hydra.python.syntax.Atom
atomTrue :: Phantoms.TTerm Syntax.Atom
atomTrue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "true"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.Atom
atomTuple :: Phantoms.TTerm Syntax.Tuple -> Phantoms.TTerm Syntax.Atom
atomTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Attribute wrapper
attribute :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.Attribute
attribute x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Attribute"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the ampersandEqual variant of hydra.python.syntax.AugAssign
augAssignAmpersandEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignAmpersandEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ampersandEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the atEqual variant of hydra.python.syntax.AugAssign
augAssignAtEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignAtEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the barEqual variant of hydra.python.syntax.AugAssign
augAssignBarEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignBarEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "barEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the caretEqual variant of hydra.python.syntax.AugAssign
augAssignCaretEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignCaretEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caretEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the doubleSlashEqual variant of hydra.python.syntax.AugAssign
augAssignDoubleSlashEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignDoubleSlashEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleSlashEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShiftEqual variant of hydra.python.syntax.AugAssign
augAssignLeftShiftEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignLeftShiftEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShiftEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minusEqual variant of hydra.python.syntax.AugAssign
augAssignMinusEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignMinusEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minusEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the percentEqual variant of hydra.python.syntax.AugAssign
augAssignPercentEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignPercentEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "percentEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plusEqual variant of hydra.python.syntax.AugAssign
augAssignPlusEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignPlusEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plusEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShiftEqual variant of hydra.python.syntax.AugAssign
augAssignRightShiftEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignRightShiftEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShiftEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the slashEqual variant of hydra.python.syntax.AugAssign
augAssignSlashEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignSlashEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the starStarEqual variant of hydra.python.syntax.AugAssign
augAssignStarStarEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignStarStarEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starStarEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the timesEqual variant of hydra.python.syntax.AugAssign
augAssignTimesEqual :: Phantoms.TTerm Syntax.AugAssign
augAssignTimesEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "timesEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.AugAssignment
augAssignment :: Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.AugAssign -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm Syntax.AugAssignment
augAssignment lhs augassign rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Phantoms.unTTerm augassign)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the augassign field of hydra.python.syntax.AugAssignment
augAssignmentAugassign :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AugAssign
augAssignmentAugassign x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionField = (Core.Name "augassign")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lhs field of hydra.python.syntax.AugAssignment
augAssignmentLhs :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.SingleTarget
augAssignmentLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.AugAssignment
augAssignmentRhs :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs
augAssignmentRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the augassign field of hydra.python.syntax.AugAssignment
augAssignmentWithAugassign :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AugAssign -> Phantoms.TTerm Syntax.AugAssignment
augAssignmentWithAugassign original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the lhs field of hydra.python.syntax.AugAssignment
augAssignmentWithLhs :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.AugAssignment
augAssignmentWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionField = (Core.Name "augassign")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.AugAssignment
augAssignmentWithRhs :: Phantoms.TTerm Syntax.AugAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm Syntax.AugAssignment
augAssignmentWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "augassign"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
              Core.projectionField = (Core.Name "augassign")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.AwaitPrimary
awaitPrimary :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.AwaitPrimary
awaitPrimary await primary =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Phantoms.unTTerm await)},
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)}]}))
-- | DSL accessor for the await field of hydra.python.syntax.AwaitPrimary
awaitPrimaryAwait :: Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Bool
awaitPrimaryAwait x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
        Core.projectionField = (Core.Name "await")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.AwaitPrimary
awaitPrimaryPrimary :: Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Syntax.Primary
awaitPrimaryPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the await field of hydra.python.syntax.AwaitPrimary
awaitPrimaryWithAwait :: Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.AwaitPrimary
awaitPrimaryWithAwait original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the primary field of hydra.python.syntax.AwaitPrimary
awaitPrimaryWithPrimary :: Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.AwaitPrimary
awaitPrimaryWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
              Core.projectionField = (Core.Name "await")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.BitwiseAnd
bitwiseAnd :: Phantoms.TTerm (Maybe Syntax.BitwiseAnd) -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.BitwiseAnd
bitwiseAnd lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndLhs :: Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm (Maybe Syntax.BitwiseAnd)
bitwiseAndLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndRhs :: Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.ShiftExpression
bitwiseAndRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndWithLhs :: Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm (Maybe Syntax.BitwiseAnd) -> Phantoms.TTerm Syntax.BitwiseAnd
bitwiseAndWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndWithRhs :: Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.BitwiseAnd
bitwiseAndWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.BitwiseOr
bitwiseOr :: Phantoms.TTerm (Maybe Syntax.BitwiseOr) -> Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseOr
bitwiseOr lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.BitwiseOr
bitwiseOrLhs :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm (Maybe Syntax.BitwiseOr)
bitwiseOrLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseOr
bitwiseOrRhs :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.BitwiseXor
bitwiseOrRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseOr
bitwiseOrWithLhs :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm (Maybe Syntax.BitwiseOr) -> Phantoms.TTerm Syntax.BitwiseOr
bitwiseOrWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.BitwiseOr
bitwiseOrWithRhs :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseOr
bitwiseOrWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.BitwiseXor
bitwiseXor :: Phantoms.TTerm (Maybe Syntax.BitwiseXor) -> Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.BitwiseXor
bitwiseXor lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.BitwiseXor
bitwiseXorLhs :: Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm (Maybe Syntax.BitwiseXor)
bitwiseXorLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseXor
bitwiseXorRhs :: Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseAnd
bitwiseXorRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseXor
bitwiseXorWithLhs :: Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm (Maybe Syntax.BitwiseXor) -> Phantoms.TTerm Syntax.BitwiseXor
bitwiseXorWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.BitwiseXor
bitwiseXorWithRhs :: Phantoms.TTerm Syntax.BitwiseXor -> Phantoms.TTerm Syntax.BitwiseAnd -> Phantoms.TTerm Syntax.BitwiseXor
bitwiseXorWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the indented variant of hydra.python.syntax.Block
blockIndented :: Phantoms.TTerm [[Syntax.Statement]] -> Phantoms.TTerm Syntax.Block
blockIndented x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Block"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indented"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Block
blockSimple :: Phantoms.TTerm [Syntax.SimpleStatement] -> Phantoms.TTerm Syntax.Block
blockSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Block"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.CapturePattern wrapper
capturePattern :: Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.CapturePattern
capturePattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.CapturePattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.CaseBlock
caseBlock :: Phantoms.TTerm Syntax.Patterns -> Phantoms.TTerm (Maybe Syntax.Guard) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CaseBlock
caseBlock patterns guard body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm patterns)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm guard)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.CaseBlock
caseBlockBody :: Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Block
caseBlockBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the guard field of hydra.python.syntax.CaseBlock
caseBlockGuard :: Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm (Maybe Syntax.Guard)
caseBlockGuard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionField = (Core.Name "guard")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the patterns field of hydra.python.syntax.CaseBlock
caseBlockPatterns :: Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Patterns
caseBlockPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionField = (Core.Name "patterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.CaseBlock
caseBlockWithBody :: Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.CaseBlock
caseBlockWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionField = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionField = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the guard field of hydra.python.syntax.CaseBlock
caseBlockWithGuard :: Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm (Maybe Syntax.Guard) -> Phantoms.TTerm Syntax.CaseBlock
caseBlockWithGuard original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionField = (Core.Name "patterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the patterns field of hydra.python.syntax.CaseBlock
caseBlockWithPatterns :: Phantoms.TTerm Syntax.CaseBlock -> Phantoms.TTerm Syntax.Patterns -> Phantoms.TTerm Syntax.CaseBlock
caseBlockWithPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionField = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ClassDefinition
classDefinition :: Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ClassDefinition
classDefinition decorators name typeParams arguments body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Phantoms.unTTerm decorators)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the arguments field of hydra.python.syntax.ClassDefinition
classDefinitionArguments :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Args)
classDefinitionArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ClassDefinition
classDefinitionBody :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Block
classDefinitionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the decorators field of hydra.python.syntax.ClassDefinition
classDefinitionDecorators :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators)
classDefinitionDecorators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionField = (Core.Name "decorators")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ClassDefinition
classDefinitionName :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Name
classDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.ClassDefinition
classDefinitionTypeParams :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm [Syntax.TypeParameter]
classDefinitionTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionField = (Core.Name "typeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.python.syntax.ClassDefinition
classDefinitionWithArguments :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.ClassDefinition
classDefinitionWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "decorators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.ClassDefinition
classDefinitionWithBody :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ClassDefinition
classDefinitionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "decorators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the decorators field of hydra.python.syntax.ClassDefinition
classDefinitionWithDecorators :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.ClassDefinition
classDefinitionWithDecorators original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.python.syntax.ClassDefinition
classDefinitionWithName :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ClassDefinition
classDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "decorators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.python.syntax.ClassDefinition
classDefinitionWithTypeParams :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.ClassDefinition
classDefinitionWithTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "decorators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ClassPattern
classPattern :: Phantoms.TTerm Syntax.NameOrAttribute -> Phantoms.TTerm (Maybe Syntax.PositionalPatterns) -> Phantoms.TTerm (Maybe Syntax.KeywordPatterns) -> Phantoms.TTerm Syntax.ClassPattern
classPattern nameOrAttribute positionalPatterns keywordPatterns =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Phantoms.unTTerm nameOrAttribute)},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Phantoms.unTTerm positionalPatterns)},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Phantoms.unTTerm keywordPatterns)}]}))
-- | DSL accessor for the keywordPatterns field of hydra.python.syntax.ClassPattern
classPatternKeywordPatterns :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.KeywordPatterns)
classPatternKeywordPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionField = (Core.Name "keywordPatterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the nameOrAttribute field of hydra.python.syntax.ClassPattern
classPatternNameOrAttribute :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm Syntax.NameOrAttribute
classPatternNameOrAttribute x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionField = (Core.Name "nameOrAttribute")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the positionalPatterns field of hydra.python.syntax.ClassPattern
classPatternPositionalPatterns :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.PositionalPatterns)
classPatternPositionalPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionField = (Core.Name "positionalPatterns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the keywordPatterns field of hydra.python.syntax.ClassPattern
classPatternWithKeywordPatterns :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.KeywordPatterns) -> Phantoms.TTerm Syntax.ClassPattern
classPatternWithKeywordPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionField = (Core.Name "nameOrAttribute")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionField = (Core.Name "positionalPatterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the nameOrAttribute field of hydra.python.syntax.ClassPattern
classPatternWithNameOrAttribute :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm Syntax.NameOrAttribute -> Phantoms.TTerm Syntax.ClassPattern
classPatternWithNameOrAttribute original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionField = (Core.Name "positionalPatterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionField = (Core.Name "keywordPatterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the positionalPatterns field of hydra.python.syntax.ClassPattern
classPatternWithPositionalPatterns :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm (Maybe Syntax.PositionalPatterns) -> Phantoms.TTerm Syntax.ClassPattern
classPatternWithPositionalPatterns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nameOrAttribute"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionField = (Core.Name "nameOrAttribute")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "positionalPatterns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywordPatterns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
              Core.projectionField = (Core.Name "keywordPatterns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the capture variant of hydra.python.syntax.ClosedPattern
closedPatternCapture :: Phantoms.TTerm Syntax.CapturePattern -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternCapture x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "capture"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the class variant of hydra.python.syntax.ClosedPattern
closedPatternClass :: Phantoms.TTerm Syntax.ClassPattern -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the group variant of hydra.python.syntax.ClosedPattern
closedPatternGroup :: Phantoms.TTerm Syntax.GroupPattern -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternGroup x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.python.syntax.ClosedPattern
closedPatternLiteral :: Phantoms.TTerm Syntax.LiteralExpression -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the mapping variant of hydra.python.syntax.ClosedPattern
closedPatternMapping :: Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternMapping x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapping"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sequence variant of hydra.python.syntax.ClosedPattern
closedPatternSequence :: Phantoms.TTerm Syntax.SequencePattern -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the value variant of hydra.python.syntax.ClosedPattern
closedPatternValue :: Phantoms.TTerm Syntax.ValuePattern -> Phantoms.TTerm Syntax.ClosedPattern
closedPatternValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.python.syntax.ClosedPattern
closedPatternWildcard :: Phantoms.TTerm Syntax.ClosedPattern
closedPatternWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.CommaStarEtc
commaStarEtc :: Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.CommaStarEtc
commaStarEtc paramMaybeDefault keywords =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramMaybeDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Phantoms.unTTerm keywords)}]}))
-- | DSL accessor for the keywords field of hydra.python.syntax.CommaStarEtc
commaStarEtcKeywords :: Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords)
commaStarEtcKeywords x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
        Core.projectionField = (Core.Name "keywords")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.CommaStarEtc
commaStarEtcParamMaybeDefault :: Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault]
commaStarEtcParamMaybeDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
        Core.projectionField = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.CommaStarEtc
commaStarEtcWithKeywords :: Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.CommaStarEtc
commaStarEtcWithKeywords original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
              Core.projectionField = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the paramMaybeDefault field of hydra.python.syntax.CommaStarEtc
commaStarEtcWithParamMaybeDefault :: Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm Syntax.CommaStarEtc
commaStarEtcWithParamMaybeDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
              Core.projectionField = (Core.Name "keywords")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPair :: Phantoms.TTerm Syntax.CompareOp -> Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPair operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the operator field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairOperator :: Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.CompareOp
compareOpBitwiseOrPairOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairRhs :: Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.BitwiseOr
compareOpBitwiseOrPairRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the operator field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithOperator :: Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.CompareOp -> Phantoms.TTerm Syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithRhs :: Phantoms.TTerm Syntax.CompareOpBitwiseOrPair -> Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the eq variant of hydra.python.syntax.CompareOp
compareOpEq :: Phantoms.TTerm Syntax.CompareOp
compareOpEq =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the gt variant of hydra.python.syntax.CompareOp
compareOpGt :: Phantoms.TTerm Syntax.CompareOp
compareOpGt =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the gte variant of hydra.python.syntax.CompareOp
compareOpGte :: Phantoms.TTerm Syntax.CompareOp
compareOpGte =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.python.syntax.CompareOp
compareOpIn :: Phantoms.TTerm Syntax.CompareOp
compareOpIn =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the is variant of hydra.python.syntax.CompareOp
compareOpIs :: Phantoms.TTerm Syntax.CompareOp
compareOpIs =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "is"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the isnot variant of hydra.python.syntax.CompareOp
compareOpIsnot :: Phantoms.TTerm Syntax.CompareOp
compareOpIsnot =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "isnot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lt variant of hydra.python.syntax.CompareOp
compareOpLt :: Phantoms.TTerm Syntax.CompareOp
compareOpLt =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lte variant of hydra.python.syntax.CompareOp
compareOpLte :: Phantoms.TTerm Syntax.CompareOp
compareOpLte =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the noteq variant of hydra.python.syntax.CompareOp
compareOpNoteq :: Phantoms.TTerm Syntax.CompareOp
compareOpNoteq =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noteq"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notin variant of hydra.python.syntax.CompareOp
compareOpNotin :: Phantoms.TTerm Syntax.CompareOp
compareOpNotin =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notin"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.Comparison
comparison :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm [Syntax.CompareOpBitwiseOrPair] -> Phantoms.TTerm Syntax.Comparison
comparison lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Comparison"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Comparison
comparisonLhs :: Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm Syntax.BitwiseOr
comparisonLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Comparison
comparisonRhs :: Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm [Syntax.CompareOpBitwiseOrPair]
comparisonRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Comparison
comparisonWithLhs :: Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.Comparison
comparisonWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Comparison"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Comparison
comparisonWithRhs :: Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm [Syntax.CompareOpBitwiseOrPair] -> Phantoms.TTerm Syntax.Comparison
comparisonWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Comparison"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ComplexNumber
complexNumber :: Phantoms.TTerm Syntax.SignedRealNumber -> Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.ImaginaryNumber -> Phantoms.TTerm Syntax.ComplexNumber
complexNumber real plusOrMinus imaginary =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Phantoms.unTTerm real)},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Phantoms.unTTerm plusOrMinus)},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Phantoms.unTTerm imaginary)}]}))
-- | DSL accessor for the imaginary field of hydra.python.syntax.ComplexNumber
complexNumberImaginary :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.ImaginaryNumber
complexNumberImaginary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionField = (Core.Name "imaginary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the plusOrMinus field of hydra.python.syntax.ComplexNumber
complexNumberPlusOrMinus :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.PlusOrMinus
complexNumberPlusOrMinus x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionField = (Core.Name "plusOrMinus")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the real field of hydra.python.syntax.ComplexNumber
complexNumberReal :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.SignedRealNumber
complexNumberReal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionField = (Core.Name "real")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the imaginary field of hydra.python.syntax.ComplexNumber
complexNumberWithImaginary :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.ImaginaryNumber -> Phantoms.TTerm Syntax.ComplexNumber
complexNumberWithImaginary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionField = (Core.Name "real")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionField = (Core.Name "plusOrMinus")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the plusOrMinus field of hydra.python.syntax.ComplexNumber
complexNumberWithPlusOrMinus :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.ComplexNumber
complexNumberWithPlusOrMinus original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionField = (Core.Name "real")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionField = (Core.Name "imaginary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the real field of hydra.python.syntax.ComplexNumber
complexNumberWithReal :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.SignedRealNumber -> Phantoms.TTerm Syntax.ComplexNumber
complexNumberWithReal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "real"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "plusOrMinus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionField = (Core.Name "plusOrMinus")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imaginary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
              Core.projectionField = (Core.Name "imaginary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the classDef variant of hydra.python.syntax.CompoundStatement
compoundStatementClassDef :: Phantoms.TTerm Syntax.ClassDefinition -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementClassDef x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classDef"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the for variant of hydra.python.syntax.CompoundStatement
compoundStatementFor :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementFor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.python.syntax.CompoundStatement
compoundStatementFunction :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the if variant of hydra.python.syntax.CompoundStatement
compoundStatementIf :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the match variant of hydra.python.syntax.CompoundStatement
compoundStatementMatch :: Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementMatch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the try variant of hydra.python.syntax.CompoundStatement
compoundStatementTry :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementTry x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the while variant of hydra.python.syntax.CompoundStatement
compoundStatementWhile :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementWhile x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the with variant of hydra.python.syntax.CompoundStatement
compoundStatementWith :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Syntax.CompoundStatement
compoundStatementWith x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Conditional
conditional :: Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conditional
conditional body if_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Phantoms.unTTerm if_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.Conditional
conditionalBody :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction
conditionalBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.Conditional
conditionalElse :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Expression
conditionalElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the if field of hydra.python.syntax.Conditional
conditionalIf :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction
conditionalIf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionField = (Core.Name "if")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.Conditional
conditionalWithBody :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Conditional
conditionalWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionField = (Core.Name "if")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.Conditional
conditionalWithElse :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conditional
conditionalWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionField = (Core.Name "if")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the if field of hydra.python.syntax.Conditional
conditionalWithIf :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Conditional
conditionalWithIf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Conditional"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.Conjunction wrapper
conjunction :: Phantoms.TTerm [Syntax.Inversion] -> Phantoms.TTerm Syntax.Conjunction
conjunction x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Conjunction"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Decorators wrapper
decorators :: Phantoms.TTerm [Syntax.NamedExpression] -> Phantoms.TTerm Syntax.Decorators
decorators x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Decorators"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Default wrapper
default_ :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Default
default_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Default"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.DelStatement wrapper
delStatement :: Phantoms.TTerm Syntax.DelTargets -> Phantoms.TTerm Syntax.DelStatement
delStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DelStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the name variant of hydra.python.syntax.DelTAtom
delTAtomName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DelTAtom
delTAtomName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the target variant of hydra.python.syntax.DelTAtom
delTAtomTarget :: Phantoms.TTerm Syntax.DelTarget -> Phantoms.TTerm Syntax.DelTAtom
delTAtomTarget x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the targets variant of hydra.python.syntax.DelTAtom
delTAtomTargets :: Phantoms.TTerm Syntax.DelTargets -> Phantoms.TTerm Syntax.DelTAtom
delTAtomTargets x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targets"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the delTAtom variant of hydra.python.syntax.DelTarget
delTargetDelTAtom :: Phantoms.TTerm Syntax.DelTAtom -> Phantoms.TTerm Syntax.DelTarget
delTargetDelTAtom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delTAtom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.DelTarget
delTargetPrimaryAndName :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.DelTarget
delTargetPrimaryAndName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.DelTarget
delTargetPrimaryAndSlices :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.DelTarget
delTargetPrimaryAndSlices x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.DelTargets wrapper
delTargets :: Phantoms.TTerm [Syntax.DelTarget] -> Phantoms.TTerm Syntax.DelTargets
delTargets x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DelTargets"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Dict wrapper
dict :: Phantoms.TTerm [Syntax.DoubleStarredKvpair] -> Phantoms.TTerm Syntax.Dict
dict x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Dict"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Dictcomp
dictcomp :: Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Dictcomp
dictcomp kvpair forIfClauses =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kvpair"),
          Core.fieldTerm = (Phantoms.unTTerm kvpair)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Phantoms.unTTerm forIfClauses)}]}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Dictcomp
dictcompForIfClauses :: Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.ForIfClauses
dictcompForIfClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
        Core.projectionField = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the kvpair field of hydra.python.syntax.Dictcomp
dictcompKvpair :: Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.Kvpair
dictcompKvpair x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
        Core.projectionField = (Core.Name "kvpair")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Dictcomp
dictcompWithForIfClauses :: Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Dictcomp
dictcompWithForIfClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kvpair"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
              Core.projectionField = (Core.Name "kvpair")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the kvpair field of hydra.python.syntax.Dictcomp
dictcompWithKvpair :: Phantoms.TTerm Syntax.Dictcomp -> Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Dictcomp
dictcompWithKvpair original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kvpair"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
              Core.projectionField = (Core.Name "forIfClauses")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.Disjunction wrapper
disjunction :: Phantoms.TTerm [Syntax.Conjunction] -> Phantoms.TTerm Syntax.Disjunction
disjunction x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Disjunction"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.DottedAsName
dottedAsName :: Phantoms.TTerm Syntax.DottedName -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.DottedAsName
dottedAsName name as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.DottedAsName
dottedAsNameAs :: Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm (Maybe Syntax.Name)
dottedAsNameAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.DottedAsName
dottedAsNameName :: Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm Syntax.DottedName
dottedAsNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.DottedAsName
dottedAsNameWithAs :: Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.DottedAsName
dottedAsNameWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.DottedAsName
dottedAsNameWithName :: Phantoms.TTerm Syntax.DottedAsName -> Phantoms.TTerm Syntax.DottedName -> Phantoms.TTerm Syntax.DottedAsName
dottedAsNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.DottedName wrapper
dottedName :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.DottedName
dottedName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DottedName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.DoubleStarPattern wrapper
doubleStarPattern :: Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.DoubleStarPattern
doubleStarPattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DoubleStarPattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameter :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DoubleStarTypeParameter
doubleStarTypeParameter name default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterDefault :: Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression)
doubleStarTypeParameterDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterName :: Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm Syntax.Name
doubleStarTypeParameterName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithDefault :: Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithName :: Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the pair variant of hydra.python.syntax.DoubleStarredKvpair
doubleStarredKvpairPair :: Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.DoubleStarredKvpair
doubleStarredKvpairPair x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarredKvpair"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.DoubleStarredKvpair
doubleStarredKvpairStarred :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.DoubleStarredKvpair
doubleStarredKvpairStarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarredKvpair"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Eval wrapper
eval :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Eval
eval x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Eval"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ExceptBlock
exceptBlock :: Phantoms.TTerm (Maybe Syntax.ExceptExpression) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptBlock
exceptBlock expression body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.ExceptBlock
exceptBlockBody :: Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm Syntax.Block
exceptBlockBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptBlock
exceptBlockExpression :: Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm (Maybe Syntax.ExceptExpression)
exceptBlockExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.ExceptBlock
exceptBlockWithBody :: Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptBlock
exceptBlockWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.ExceptBlock
exceptBlockWithExpression :: Phantoms.TTerm Syntax.ExceptBlock -> Phantoms.TTerm (Maybe Syntax.ExceptExpression) -> Phantoms.TTerm Syntax.ExceptBlock
exceptBlockWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ExceptExpression
exceptExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ExceptExpression
exceptExpression expression as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.ExceptExpression
exceptExpressionAs :: Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm (Maybe Syntax.Name)
exceptExpressionAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptExpression
exceptExpressionExpression :: Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm Syntax.Expression
exceptExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ExceptExpression
exceptExpressionWithAs :: Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ExceptExpression
exceptExpressionWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.ExceptExpression
exceptExpressionWithExpression :: Phantoms.TTerm Syntax.ExceptExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExceptExpression
exceptExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.ExceptStarBlock
exceptStarBlock :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptStarBlock
exceptStarBlock expression as body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockAs :: Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm (Maybe Syntax.Name)
exceptStarBlockAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockBody :: Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Block
exceptStarBlockBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockExpression :: Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Expression
exceptStarBlockExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithAs :: Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ExceptStarBlock
exceptStarBlockWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithBody :: Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ExceptStarBlock
exceptStarBlockWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithExpression :: Phantoms.TTerm Syntax.ExceptStarBlock -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExceptStarBlock
exceptStarBlockWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the conditional variant of hydra.python.syntax.Expression
expressionConditional :: Phantoms.TTerm Syntax.Conditional -> Phantoms.TTerm Syntax.Expression
expressionConditional x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lambda variant of hydra.python.syntax.Expression
expressionLambda :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Expression
expressionSimple :: Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.Expression
expressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the complement variant of hydra.python.syntax.Factor
factorComplement :: Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Factor
factorComplement x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the negative variant of hydra.python.syntax.Factor
factorNegative :: Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Factor
factorNegative x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the positive variant of hydra.python.syntax.Factor
factorPositive :: Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Factor
factorPositive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "positive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Factor
factorSimple :: Phantoms.TTerm Syntax.Power -> Phantoms.TTerm Syntax.Factor
factorSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.File wrapper
file :: Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.File
file x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.File"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ForIfClause
forIfClause :: Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm [Syntax.Disjunction] -> Phantoms.TTerm Syntax.ForIfClause
forIfClause async targets in_ ifs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm targets)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Phantoms.unTTerm ifs)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.ForIfClause
forIfClauseAsync :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Bool
forIfClauseAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionField = (Core.Name "async")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the ifs field of hydra.python.syntax.ForIfClause
forIfClauseIfs :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.Disjunction]
forIfClauseIfs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionField = (Core.Name "ifs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the in field of hydra.python.syntax.ForIfClause
forIfClauseIn :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Syntax.Disjunction
forIfClauseIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionField = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ForIfClause
forIfClauseTargets :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.StarTarget]
forIfClauseTargets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionField = (Core.Name "targets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.ForIfClause
forIfClauseWithAsync :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ForIfClause
forIfClauseWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "ifs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the ifs field of hydra.python.syntax.ForIfClause
forIfClauseWithIfs :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.Disjunction] -> Phantoms.TTerm Syntax.ForIfClause
forIfClauseWithIfs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the in field of hydra.python.syntax.ForIfClause
forIfClauseWithIn :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm Syntax.ForIfClause
forIfClauseWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "ifs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.ForIfClause
forIfClauseWithTargets :: Phantoms.TTerm Syntax.ForIfClause -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.ForIfClause
forIfClauseWithTargets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ifs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
              Core.projectionField = (Core.Name "ifs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.ForIfClauses wrapper
forIfClauses :: Phantoms.TTerm [Syntax.ForIfClause] -> Phantoms.TTerm Syntax.ForIfClauses
forIfClauses x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ForIfClauses"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ForStatement
forStatement :: Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.ForStatement
forStatement async targets expressions typeComment body else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm targets)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.ForStatement
forStatementAsync :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Bool
forStatementAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionField = (Core.Name "async")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ForStatement
forStatementBody :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Block
forStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.ForStatement
forStatementElse :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Block)
forStatementElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expressions field of hydra.python.syntax.ForStatement
forStatementExpressions :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarExpression]
forStatementExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionField = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ForStatement
forStatementTargets :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarTarget]
forStatementTargets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionField = (Core.Name "targets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ForStatement
forStatementTypeComment :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment)
forStatementTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.ForStatement
forStatementWithAsync :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ForStatement
forStatementWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.ForStatement
forStatementWithBody :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForStatement
forStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.ForStatement
forStatementWithElse :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.ForStatement
forStatementWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expressions field of hydra.python.syntax.ForStatement
forStatementWithExpressions :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.ForStatement
forStatementWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.ForStatement
forStatementWithTargets :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.ForStatement
forStatementWithTargets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ForStatement
forStatementWithTypeComment :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ForStatement
forStatementWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.FuncTypeComment wrapper
funcTypeComment :: Phantoms.TTerm Syntax.TypeComment -> Phantoms.TTerm Syntax.FuncTypeComment
funcTypeComment x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.FuncTypeComment"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.FunctionDefRaw
functionDefRaw :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm (Maybe Syntax.Parameters) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.FuncTypeComment) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRaw async name typeParams params returnType funcTypeComment block =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Phantoms.unTTerm funcTypeComment)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.FunctionDefRaw
functionDefRawAsync :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Bool
functionDefRawAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "async")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the block field of hydra.python.syntax.FunctionDefRaw
functionDefRawBlock :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Block
functionDefRawBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "block")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the funcTypeComment field of hydra.python.syntax.FunctionDefRaw
functionDefRawFuncTypeComment :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.FuncTypeComment)
functionDefRawFuncTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "funcTypeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.FunctionDefRaw
functionDefRawName :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Name
functionDefRawName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.python.syntax.FunctionDefRaw
functionDefRawParams :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Parameters)
functionDefRawParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the returnType field of hydra.python.syntax.FunctionDefRaw
functionDefRawReturnType :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Expression)
functionDefRawReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "returnType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.FunctionDefRaw
functionDefRawTypeParams :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm [Syntax.TypeParameter]
functionDefRawTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionField = (Core.Name "typeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithAsync :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the block field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithBlock :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the funcTypeComment field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithFuncTypeComment :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.FuncTypeComment) -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithFuncTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithName :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the params field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithParams :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Parameters) -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the returnType field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithReturnType :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithTypeParams :: Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefRawWithTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "returnType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "funcTypeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "funcTypeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
              Core.projectionField = (Core.Name "block")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.FunctionDefinition
functionDefinition :: Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinition decorators raw =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Phantoms.unTTerm decorators)},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = (Phantoms.unTTerm raw)}]}))
-- | DSL accessor for the decorators field of hydra.python.syntax.FunctionDefinition
functionDefinitionDecorators :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators)
functionDefinitionDecorators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "decorators")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the raw field of hydra.python.syntax.FunctionDefinition
functionDefinitionRaw :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.FunctionDefRaw
functionDefinitionRaw x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
        Core.projectionField = (Core.Name "raw")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decorators field of hydra.python.syntax.FunctionDefinition
functionDefinitionWithDecorators :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm (Maybe Syntax.Decorators) -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithDecorators original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "raw")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the raw field of hydra.python.syntax.FunctionDefinition
functionDefinitionWithRaw :: Phantoms.TTerm Syntax.FunctionDefinition -> Phantoms.TTerm Syntax.FunctionDefRaw -> Phantoms.TTerm Syntax.FunctionDefinition
functionDefinitionWithRaw original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "decorators"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
              Core.projectionField = (Core.Name "decorators")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "raw"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.Genexp
genexp :: Phantoms.TTerm Syntax.GenexpHead -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Genexp
genexp head tail =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Genexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm tail)}]}))
-- | DSL accessor for the head field of hydra.python.syntax.Genexp
genexpHead :: Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.GenexpHead
genexpHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
        Core.projectionField = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the assignment variant of hydra.python.syntax.GenexpHead
genexpHeadAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.GenexpHead
genexpHeadAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.GenexpHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.GenexpHead
genexpHeadExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.GenexpHead
genexpHeadExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.GenexpHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the tail field of hydra.python.syntax.Genexp
genexpTail :: Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.ForIfClauses
genexpTail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
        Core.projectionField = (Core.Name "tail")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.Genexp
genexpWithHead :: Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.GenexpHead -> Phantoms.TTerm Syntax.Genexp
genexpWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Genexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
              Core.projectionField = (Core.Name "tail")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.python.syntax.Genexp
genexpWithTail :: Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Genexp
genexpWithTail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Genexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
              Core.projectionField = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the expression variant of hydra.python.syntax.Group
groupExpression :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Group
groupExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Group"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.GroupPattern wrapper
groupPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.GroupPattern
groupPattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.GroupPattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the yield variant of hydra.python.syntax.Group
groupYield :: Phantoms.TTerm Syntax.YieldExpression -> Phantoms.TTerm Syntax.Group
groupYield x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Group"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Guard wrapper
guard :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Guard
guard x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Guard"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.IfStatement
ifStatement :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.IfTail) -> Phantoms.TTerm Syntax.IfStatement
ifStatement condition body continuation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Phantoms.unTTerm continuation)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.IfStatement
ifStatementBody :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Block
ifStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the condition field of hydra.python.syntax.IfStatement
ifStatementCondition :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.NamedExpression
ifStatementCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the continuation field of hydra.python.syntax.IfStatement
ifStatementContinuation :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm (Maybe Syntax.IfTail)
ifStatementContinuation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionField = (Core.Name "continuation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.IfStatement
ifStatementWithBody :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfStatement
ifStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionField = (Core.Name "continuation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the condition field of hydra.python.syntax.IfStatement
ifStatementWithCondition :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.IfStatement
ifStatementWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionField = (Core.Name "continuation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the continuation field of hydra.python.syntax.IfStatement
ifStatementWithContinuation :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm (Maybe Syntax.IfTail) -> Phantoms.TTerm Syntax.IfStatement
ifStatementWithContinuation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "continuation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the elif variant of hydra.python.syntax.IfTail
ifTailElif :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.IfTail
ifTailElif x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.IfTail"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elif"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the else variant of hydra.python.syntax.IfTail
ifTailElse :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfTail
ifTailElse x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.IfTail"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "else"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.ImaginaryNumber wrapper
imaginaryNumber :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.ImaginaryNumber
imaginaryNumber x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ImaginaryNumber"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ImportFrom
importFrom :: Phantoms.TTerm [Syntax.RelativeImportPrefix] -> Phantoms.TTerm (Maybe Syntax.DottedName) -> Phantoms.TTerm Syntax.ImportFromTargets -> Phantoms.TTerm Syntax.ImportFrom
importFrom prefixes dottedName targets =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Phantoms.unTTerm prefixes)},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Phantoms.unTTerm dottedName)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm targets)}]}))
-- | DSL constructor for hydra.python.syntax.ImportFromAsName
importFromAsName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ImportFromAsName
importFromAsName name as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.ImportFromAsName
importFromAsNameAs :: Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm (Maybe Syntax.Name)
importFromAsNameAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ImportFromAsName
importFromAsNameName :: Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm Syntax.Name
importFromAsNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ImportFromAsName
importFromAsNameWithAs :: Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm (Maybe Syntax.Name) -> Phantoms.TTerm Syntax.ImportFromAsName
importFromAsNameWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.ImportFromAsName
importFromAsNameWithName :: Phantoms.TTerm Syntax.ImportFromAsName -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ImportFromAsName
importFromAsNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the dottedName field of hydra.python.syntax.ImportFrom
importFromDottedName :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm (Maybe Syntax.DottedName)
importFromDottedName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionField = (Core.Name "dottedName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the prefixes field of hydra.python.syntax.ImportFrom
importFromPrefixes :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm [Syntax.RelativeImportPrefix]
importFromPrefixes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionField = (Core.Name "prefixes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ImportFrom
importFromTargets :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm Syntax.ImportFromTargets
importFromTargets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionField = (Core.Name "targets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the parens variant of hydra.python.syntax.ImportFromTargets
importFromTargetsParens :: Phantoms.TTerm [Syntax.ImportFromAsName] -> Phantoms.TTerm Syntax.ImportFromTargets
importFromTargetsParens x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.ImportFromTargets
importFromTargetsSimple :: Phantoms.TTerm [Syntax.ImportFromAsName] -> Phantoms.TTerm Syntax.ImportFromTargets
importFromTargetsSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.ImportFromTargets
importFromTargetsStar :: Phantoms.TTerm Syntax.ImportFromTargets
importFromTargetsStar =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the dottedName field of hydra.python.syntax.ImportFrom
importFromWithDottedName :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm (Maybe Syntax.DottedName) -> Phantoms.TTerm Syntax.ImportFrom
importFromWithDottedName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionField = (Core.Name "prefixes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the prefixes field of hydra.python.syntax.ImportFrom
importFromWithPrefixes :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm [Syntax.RelativeImportPrefix] -> Phantoms.TTerm Syntax.ImportFrom
importFromWithPrefixes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionField = (Core.Name "dottedName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.ImportFrom
importFromWithTargets :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm Syntax.ImportFromTargets -> Phantoms.TTerm Syntax.ImportFrom
importFromWithTargets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "prefixes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionField = (Core.Name "prefixes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dottedName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
              Core.projectionField = (Core.Name "dottedName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.ImportName wrapper
importName :: Phantoms.TTerm [Syntax.DottedAsName] -> Phantoms.TTerm Syntax.ImportName
importName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ImportName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the from variant of hydra.python.syntax.ImportStatement
importStatementFrom :: Phantoms.TTerm Syntax.ImportFrom -> Phantoms.TTerm Syntax.ImportStatement
importStatementFrom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.ImportStatement
importStatementName :: Phantoms.TTerm Syntax.ImportName -> Phantoms.TTerm Syntax.ImportStatement
importStatementName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Interactive wrapper
interactive :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Interactive
interactive x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Interactive"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the not variant of hydra.python.syntax.Inversion
inversionNot :: Phantoms.TTerm Syntax.Inversion -> Phantoms.TTerm Syntax.Inversion
inversionNot x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Inversion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Inversion
inversionSimple :: Phantoms.TTerm Syntax.Comparison -> Phantoms.TTerm Syntax.Inversion
inversionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Inversion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.ItemsPattern wrapper
itemsPattern :: Phantoms.TTerm [Syntax.KeyValuePattern] -> Phantoms.TTerm Syntax.ItemsPattern
itemsPattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ItemsPattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.KeyValuePattern
keyValuePattern :: Phantoms.TTerm Syntax.LiteralExpressionOrAttribute -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeyValuePattern
keyValuePattern key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the key field of hydra.python.syntax.KeyValuePattern
keyValuePatternKey :: Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute
keyValuePatternKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.KeyValuePattern
keyValuePatternValue :: Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.Pattern
keyValuePatternValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.python.syntax.KeyValuePattern
keyValuePatternWithKey :: Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute -> Phantoms.TTerm Syntax.KeyValuePattern
keyValuePatternWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.python.syntax.KeyValuePattern
keyValuePatternWithValue :: Phantoms.TTerm Syntax.KeyValuePattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeyValuePattern
keyValuePatternWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.KeywordPattern
keywordPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeywordPattern
keywordPattern name pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.KeywordPattern
keywordPatternName :: Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Name
keywordPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pattern field of hydra.python.syntax.KeywordPattern
keywordPatternPattern :: Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Pattern
keywordPatternPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.KeywordPattern
keywordPatternWithName :: Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.KeywordPattern
keywordPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pattern field of hydra.python.syntax.KeywordPattern
keywordPatternWithPattern :: Phantoms.TTerm Syntax.KeywordPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.KeywordPattern
keywordPatternWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.KeywordPatterns wrapper
keywordPatterns :: Phantoms.TTerm [Syntax.KeywordPattern] -> Phantoms.TTerm Syntax.KeywordPatterns
keywordPatterns x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.KeywordPatterns"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Keywords wrapper
keywords :: Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.Keywords
keywords x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Keywords"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Kvpair
kvpair :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kvpair
kvpair key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the key field of hydra.python.syntax.Kvpair
kvpairKey :: Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression
kvpairKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.Kvpair
kvpairValue :: Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression
kvpairValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.python.syntax.Kvpair
kvpairWithKey :: Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kvpair
kvpairWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.python.syntax.Kvpair
kvpairWithValue :: Phantoms.TTerm Syntax.Kvpair -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kvpair
kvpairWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.Kwarg
kwarg :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kwarg
kwarg name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.Kwarg
kwargName :: Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Name
kwargName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the doubleStarred variant of hydra.python.syntax.KwargOrDoubleStarred
kwargOrDoubleStarredDoubleStarred :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.KwargOrDoubleStarred
kwargOrDoubleStarredDoubleStarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrDoubleStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStarred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the kwarg variant of hydra.python.syntax.KwargOrDoubleStarred
kwargOrDoubleStarredKwarg :: Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.KwargOrDoubleStarred
kwargOrDoubleStarredKwarg x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrDoubleStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwarg"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the kwarg variant of hydra.python.syntax.KwargOrStarred
kwargOrStarredKwarg :: Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.KwargOrStarred
kwargOrStarredKwarg x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwarg"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.KwargOrStarred
kwargOrStarredStarred :: Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.KwargOrStarred
kwargOrStarredStarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the value field of hydra.python.syntax.Kwarg
kwargValue :: Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Expression
kwargValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.Kwarg
kwargWithName :: Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Kwarg
kwargWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.python.syntax.Kwarg
kwargWithValue :: Phantoms.TTerm Syntax.Kwarg -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Kwarg
kwargWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.Lambda
lambda :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Lambda
lambda params body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.Lambda
lambdaBody :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression
lambdaBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.LambdaKwds wrapper
lambdaKwds :: Phantoms.TTerm Syntax.LambdaParamNoDefault -> Phantoms.TTerm Syntax.LambdaKwds
lambdaKwds x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.LambdaKwds"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefault :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefault param default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultDefault :: Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default)
lambdaParamMaybeDefaultDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultParam :: Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm Syntax.Name
lambdaParamMaybeDefaultParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
        Core.projectionField = (Core.Name "param")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithDefault :: Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the param field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithParam :: Phantoms.TTerm Syntax.LambdaParamMaybeDefault -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.LambdaParamNoDefault wrapper
lambdaParamNoDefault :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LambdaParamNoDefault
lambdaParamNoDefault x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.LambdaParamNoDefault"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefault :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamWithDefault
lambdaParamWithDefault param default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultDefault :: Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm (Maybe Syntax.Default)
lambdaParamWithDefaultDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultParam :: Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm Syntax.Name
lambdaParamWithDefaultParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
        Core.projectionField = (Core.Name "param")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithDefault :: Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the param field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithParam :: Phantoms.TTerm Syntax.LambdaParamWithDefault -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.LambdaParameters
lambdaParameters :: Phantoms.TTerm (Maybe Syntax.LambdaSlashNoDefault) -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.LambdaStarEtc) -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParameters slashNoDefault paramNoDefault paramWithDefault starEtc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm slashNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersParamNoDefault :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamNoDefault]
lambdaParametersParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersParamWithDefault :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamWithDefault]
lambdaParametersParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the slashNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersSlashNoDefault :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaSlashNoDefault)
lambdaParametersSlashNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionField = (Core.Name "slashNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.LambdaParameters
lambdaParametersStarEtc :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaStarEtc)
lambdaParametersStarEtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionField = (Core.Name "starEtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithParamNoDefault :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "slashNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithParamWithDefault :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "slashNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the slashNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithSlashNoDefault :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaSlashNoDefault) -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersWithSlashNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.LambdaParameters
lambdaParametersWithStarEtc :: Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm (Maybe Syntax.LambdaStarEtc) -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParametersWithStarEtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slashNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "slashNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the params field of hydra.python.syntax.Lambda
lambdaParams :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.LambdaParameters
lambdaParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
        Core.projectionField = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefault :: Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaSlashNoDefault
lambdaSlashNoDefault parameters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)}]}))
-- | DSL accessor for the parameters field of hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultParameters :: Phantoms.TTerm Syntax.LambdaSlashNoDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault]
lambdaSlashNoDefaultParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the parameters field of hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultWithParameters :: Phantoms.TTerm Syntax.LambdaSlashNoDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefault :: Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm Syntax.LambdaSlashWithDefault
lambdaSlashWithDefault paramNoDefault paramWithDefault =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultParamNoDefault :: Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault]
lambdaSlashWithDefaultParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultParamWithDefault :: Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamWithDefault]
lambdaSlashWithDefaultParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamNoDefault :: Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamNoDefault] -> Phantoms.TTerm Syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamWithDefault :: Phantoms.TTerm Syntax.LambdaSlashWithDefault -> Phantoms.TTerm [Syntax.LambdaParamWithDefault] -> Phantoms.TTerm Syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the kwds variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcKwds :: Phantoms.TTerm Syntax.LambdaKwds -> Phantoms.TTerm Syntax.LambdaStarEtc
lambdaStarEtcKwds x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwds"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the paramMaybeDefault variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcParamMaybeDefault :: Phantoms.TTerm [Syntax.LambdaParamMaybeDefault] -> Phantoms.TTerm Syntax.LambdaStarEtc
lambdaStarEtcParamMaybeDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramMaybeDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the paramNoDefault variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcParamNoDefault :: Phantoms.TTerm Syntax.LambdaParamNoDefault -> Phantoms.TTerm Syntax.LambdaStarEtc
lambdaStarEtcParamNoDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramNoDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcStar :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.LambdaStarEtc
lambdaStarEtcStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL updater for the body field of hydra.python.syntax.Lambda
lambdaWithBody :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Lambda
lambdaWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
              Core.projectionField = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the params field of hydra.python.syntax.Lambda
lambdaWithParams :: Phantoms.TTerm Syntax.Lambda -> Phantoms.TTerm Syntax.LambdaParameters -> Phantoms.TTerm Syntax.Lambda
lambdaWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Lambda"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.List wrapper
list :: Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.List
list x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.List"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Listcomp
listcomp :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Listcomp
listcomp expression forIfClauses =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Phantoms.unTTerm forIfClauses)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.Listcomp
listcompExpression :: Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.NamedExpression
listcompExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Listcomp
listcompForIfClauses :: Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.ForIfClauses
listcompForIfClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
        Core.projectionField = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.Listcomp
listcompWithExpression :: Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Listcomp
listcompWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
              Core.projectionField = (Core.Name "forIfClauses")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Listcomp
listcompWithForIfClauses :: Phantoms.TTerm Syntax.Listcomp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Listcomp
listcompWithForIfClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the complex variant of hydra.python.syntax.LiteralExpression
literalExpressionComplex :: Phantoms.TTerm Syntax.ComplexNumber -> Phantoms.TTerm Syntax.LiteralExpression
literalExpressionComplex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the false variant of hydra.python.syntax.LiteralExpression
literalExpressionFalse :: Phantoms.TTerm Syntax.LiteralExpression
literalExpressionFalse =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "false"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.python.syntax.LiteralExpression
literalExpressionNone :: Phantoms.TTerm Syntax.LiteralExpression
literalExpressionNone =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.LiteralExpression
literalExpressionNumber :: Phantoms.TTerm Syntax.SignedNumber -> Phantoms.TTerm Syntax.LiteralExpression
literalExpressionNumber x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the attribute variant of hydra.python.syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeAttribute :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeAttribute x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpressionOrAttribute"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "attribute"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.python.syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeLiteral :: Phantoms.TTerm Syntax.LiteralExpression -> Phantoms.TTerm Syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpressionOrAttribute"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.python.syntax.LiteralExpression
literalExpressionString :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.LiteralExpression
literalExpressionString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the true variant of hydra.python.syntax.LiteralExpression
literalExpressionTrue :: Phantoms.TTerm Syntax.LiteralExpression
literalExpressionTrue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "true"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.MappingPattern
mappingPattern :: Phantoms.TTerm (Maybe Syntax.ItemsPattern) -> Phantoms.TTerm (Maybe Syntax.DoubleStarPattern) -> Phantoms.TTerm Syntax.MappingPattern
mappingPattern items doubleStar =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "doubleStar"),
          Core.fieldTerm = (Phantoms.unTTerm doubleStar)}]}))
-- | DSL accessor for the doubleStar field of hydra.python.syntax.MappingPattern
mappingPatternDoubleStar :: Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.DoubleStarPattern)
mappingPatternDoubleStar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
        Core.projectionField = (Core.Name "doubleStar")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the items field of hydra.python.syntax.MappingPattern
mappingPatternItems :: Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.ItemsPattern)
mappingPatternItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the doubleStar field of hydra.python.syntax.MappingPattern
mappingPatternWithDoubleStar :: Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.DoubleStarPattern) -> Phantoms.TTerm Syntax.MappingPattern
mappingPatternWithDoubleStar original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "doubleStar"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the items field of hydra.python.syntax.MappingPattern
mappingPatternWithItems :: Phantoms.TTerm Syntax.MappingPattern -> Phantoms.TTerm (Maybe Syntax.ItemsPattern) -> Phantoms.TTerm Syntax.MappingPattern
mappingPatternWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "doubleStar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
              Core.projectionField = (Core.Name "doubleStar")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.MatchStatement
matchStatement :: Phantoms.TTerm Syntax.SubjectExpression -> Phantoms.TTerm [Syntax.CaseBlock] -> Phantoms.TTerm Syntax.MatchStatement
matchStatement subject cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm subject)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.python.syntax.MatchStatement
matchStatementCases :: Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm [Syntax.CaseBlock]
matchStatementCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
        Core.projectionField = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subject field of hydra.python.syntax.MatchStatement
matchStatementSubject :: Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm Syntax.SubjectExpression
matchStatementSubject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
        Core.projectionField = (Core.Name "subject")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.python.syntax.MatchStatement
matchStatementWithCases :: Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm [Syntax.CaseBlock] -> Phantoms.TTerm Syntax.MatchStatement
matchStatementWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
              Core.projectionField = (Core.Name "subject")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the subject field of hydra.python.syntax.MatchStatement
matchStatementWithSubject :: Phantoms.TTerm Syntax.MatchStatement -> Phantoms.TTerm Syntax.SubjectExpression -> Phantoms.TTerm Syntax.MatchStatement
matchStatementWithSubject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subject"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
              Core.projectionField = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.MaybeSequencePattern wrapper
maybeSequencePattern :: Phantoms.TTerm [Syntax.MaybeStarPattern] -> Phantoms.TTerm Syntax.MaybeSequencePattern
maybeSequencePattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.MaybeSequencePattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the pattern variant of hydra.python.syntax.MaybeStarPattern
maybeStarPatternPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.MaybeStarPattern
maybeStarPatternPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.MaybeStarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.MaybeStarPattern
maybeStarPatternStar :: Phantoms.TTerm Syntax.StarPattern -> Phantoms.TTerm Syntax.MaybeStarPattern
maybeStarPatternStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.MaybeStarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Module wrapper
module_ :: Phantoms.TTerm [[Syntax.Statement]] -> Phantoms.TTerm Syntax.Module
module_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Module"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Name wrapper
name :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.NameOrAttribute wrapper
nameOrAttribute :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.NameOrAttribute
nameOrAttribute x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.NameOrAttribute"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the assignment variant of hydra.python.syntax.NamedExpression
namedExpressionAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.NamedExpression
namedExpressionAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.NamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.NamedExpression
namedExpressionSimple :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.NamedExpression
namedExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.NamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtc :: Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtc paramNoDefaultStarAnnotation paramMaybeDefault keywords =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefaultStarAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramMaybeDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Phantoms.unTTerm keywords)}]}))
-- | DSL accessor for the keywords field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcKeywords :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords)
noDefaultStarAnnotationStarEtcKeywords x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionField = (Core.Name "keywords")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcParamMaybeDefault :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault]
noDefaultStarAnnotationStarEtcParamMaybeDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionField = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramNoDefaultStarAnnotation field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionField = (Core.Name "paramNoDefaultStarAnnotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithKeywords :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithKeywords original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionField = (Core.Name "paramNoDefaultStarAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionField = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamMaybeDefault :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamMaybeDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionField = (Core.Name "paramNoDefaultStarAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionField = (Core.Name "keywords")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramNoDefaultStarAnnotation field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefaultStarAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionField = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
              Core.projectionField = (Core.Name "keywords")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtc :: Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarEtc
noDefaultStarEtc paramNoDefault paramMaybeDefault keywords =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramMaybeDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Phantoms.unTTerm keywords)}]}))
-- | DSL accessor for the keywords field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcKeywords :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords)
noDefaultStarEtcKeywords x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionField = (Core.Name "keywords")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcParamMaybeDefault :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault]
noDefaultStarEtcParamMaybeDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionField = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcParamNoDefault :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm Syntax.ParamNoDefault
noDefaultStarEtcParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithKeywords :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm (Maybe Syntax.Keywords) -> Phantoms.TTerm Syntax.NoDefaultStarEtc
noDefaultStarEtcWithKeywords original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionField = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamMaybeDefault :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm [Syntax.ParamMaybeDefault] -> Phantoms.TTerm Syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamMaybeDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionField = (Core.Name "keywords")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamNoDefault :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.NoDefaultStarEtc
noDefaultStarEtcWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramMaybeDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionField = (Core.Name "paramMaybeDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keywords"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
              Core.projectionField = (Core.Name "keywords")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the float variant of hydra.python.syntax.Number
numberFloat :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Number
numberFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the imaginary variant of hydra.python.syntax.Number
numberImaginary :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Number
numberImaginary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "imaginary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.python.syntax.Number
numberInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Number
numberInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.OpenSequencePattern
openSequencePattern :: Phantoms.TTerm Syntax.MaybeStarPattern -> Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern) -> Phantoms.TTerm Syntax.OpenSequencePattern
openSequencePattern head tail =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm tail)}]}))
-- | DSL accessor for the head field of hydra.python.syntax.OpenSequencePattern
openSequencePatternHead :: Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm Syntax.MaybeStarPattern
openSequencePatternHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
        Core.projectionField = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tail field of hydra.python.syntax.OpenSequencePattern
openSequencePatternTail :: Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern)
openSequencePatternTail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
        Core.projectionField = (Core.Name "tail")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.OpenSequencePattern
openSequencePatternWithHead :: Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm Syntax.MaybeStarPattern -> Phantoms.TTerm Syntax.OpenSequencePattern
openSequencePatternWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
              Core.projectionField = (Core.Name "tail")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.python.syntax.OpenSequencePattern
openSequencePatternWithTail :: Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern) -> Phantoms.TTerm Syntax.OpenSequencePattern
openSequencePatternWithTail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
              Core.projectionField = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.OrPattern wrapper
orPattern :: Phantoms.TTerm [Syntax.ClosedPattern] -> Phantoms.TTerm Syntax.OrPattern
orPattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.OrPattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Param
param :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Annotation) -> Phantoms.TTerm Syntax.Param
param name annotation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.python.syntax.Param
paramAnnotation :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.Annotation)
paramAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
        Core.projectionField = (Core.Name "annotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamMaybeDefault
paramMaybeDefault :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamMaybeDefault
paramMaybeDefault param default_ typeComment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultDefault :: Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default)
paramMaybeDefaultDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultParam :: Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm Syntax.Param
paramMaybeDefaultParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionField = (Core.Name "param")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultTypeComment :: Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment)
paramMaybeDefaultTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithDefault :: Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.Default) -> Phantoms.TTerm Syntax.ParamMaybeDefault
paramMaybeDefaultWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the param field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithParam :: Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ParamMaybeDefault
paramMaybeDefaultWithParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithTypeComment :: Phantoms.TTerm Syntax.ParamMaybeDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamMaybeDefault
paramMaybeDefaultWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.Param
paramName :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.Name
paramName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefault
paramNoDefault :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefault
paramNoDefault param typeComment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)}]}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamNoDefault
paramNoDefaultParam :: Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.Param
paramNoDefaultParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
        Core.projectionField = (Core.Name "param")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParameters :: Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParameters paramNoDefault paramWithDefault starEtc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersParamNoDefault :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault]
paramNoDefaultParametersParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersParamWithDefault :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault]
paramNoDefaultParametersParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersStarEtc :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc)
paramNoDefaultParametersStarEtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionField = (Core.Name "starEtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamNoDefault :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamWithDefault :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithStarEtc :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithStarEtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotation :: Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotation paramStarAnnotation typeComment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramStarAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm paramStarAnnotation)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)}]}))
-- | DSL accessor for the paramStarAnnotation field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationParamStarAnnotation :: Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation
paramNoDefaultStarAnnotationParamStarAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
        Core.projectionField = (Core.Name "paramStarAnnotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationTypeComment :: Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm (Maybe Syntax.TypeComment)
paramNoDefaultStarAnnotationTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramStarAnnotation field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithParamStarAnnotation :: Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithParamStarAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramStarAnnotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithTypeComment :: Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramStarAnnotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
              Core.projectionField = (Core.Name "paramStarAnnotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamNoDefault
paramNoDefaultTypeComment :: Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment)
paramNoDefaultTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the param field of hydra.python.syntax.ParamNoDefault
paramNoDefaultWithParam :: Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ParamNoDefault
paramNoDefaultWithParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamNoDefault
paramNoDefaultWithTypeComment :: Phantoms.TTerm Syntax.ParamNoDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamNoDefault
paramNoDefaultWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ParamStarAnnotation
paramStarAnnotation :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.StarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation
paramStarAnnotation name annotation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm annotation)}]}))
-- | DSL accessor for the annotation field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationAnnotation :: Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.StarAnnotation
paramStarAnnotationAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
        Core.projectionField = (Core.Name "annotation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationName :: Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.Name
paramStarAnnotationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the annotation field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationWithAnnotation :: Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.StarAnnotation -> Phantoms.TTerm Syntax.ParamStarAnnotation
paramStarAnnotationWithAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationWithName :: Phantoms.TTerm Syntax.ParamStarAnnotation -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ParamStarAnnotation
paramStarAnnotationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
              Core.projectionField = (Core.Name "annotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the annotation field of hydra.python.syntax.Param
paramWithAnnotation :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm (Maybe Syntax.Annotation) -> Phantoms.TTerm Syntax.Param
paramWithAnnotation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ParamWithDefault
paramWithDefault :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.Default -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamWithDefault
paramWithDefault param default_ typeComment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.ParamWithDefault
paramWithDefaultDefault :: Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Default
paramWithDefaultDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamWithDefault
paramWithDefaultParam :: Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Param
paramWithDefaultParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionField = (Core.Name "param")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParameters :: Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamWithDefaultParameters
paramWithDefaultParameters paramWithDefault starEtc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm starEtc)}]}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersParamWithDefault :: Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault]
paramWithDefaultParametersParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersStarEtc :: Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc)
paramWithDefaultParametersStarEtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
        Core.projectionField = (Core.Name "starEtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithParamWithDefault :: Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithStarEtc :: Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithStarEtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamWithDefault
paramWithDefaultTypeComment :: Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment)
paramWithDefaultTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithDefault :: Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Default -> Phantoms.TTerm Syntax.ParamWithDefault
paramWithDefaultWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the param field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithParam :: Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.ParamWithDefault
paramWithDefaultWithParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithTypeComment :: Phantoms.TTerm Syntax.ParamWithDefault -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.ParamWithDefault
paramWithDefaultWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionField = (Core.Name "param")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.Param
paramWithName :: Phantoms.TTerm Syntax.Param -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Param
paramWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Param"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
              Core.projectionField = (Core.Name "annotation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the paramNoDefault variant of hydra.python.syntax.Parameters
parametersParamNoDefault :: Phantoms.TTerm Syntax.ParamNoDefaultParameters -> Phantoms.TTerm Syntax.Parameters
parametersParamNoDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramNoDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the paramWithDefault variant of hydra.python.syntax.Parameters
parametersParamWithDefault :: Phantoms.TTerm Syntax.ParamWithDefaultParameters -> Phantoms.TTerm Syntax.Parameters
parametersParamWithDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramWithDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slashNoDefault variant of hydra.python.syntax.Parameters
parametersSlashNoDefault :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm Syntax.Parameters
parametersSlashNoDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashNoDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slashWithDefault variant of hydra.python.syntax.Parameters
parametersSlashWithDefault :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm Syntax.Parameters
parametersSlashWithDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashWithDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starEtc variant of hydra.python.syntax.Parameters
parametersStarEtc :: Phantoms.TTerm Syntax.StarEtc -> Phantoms.TTerm Syntax.Parameters
parametersStarEtc x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starEtc"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the as variant of hydra.python.syntax.Pattern
patternAs :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern
patternAs x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.PatternCaptureTarget wrapper
patternCaptureTarget :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PatternCaptureTarget
patternCaptureTarget x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.PatternCaptureTarget"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the or variant of hydra.python.syntax.Pattern
patternOr :: Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm Syntax.Pattern
patternOr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pattern variant of hydra.python.syntax.Patterns
patternsPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Patterns
patternsPattern x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Patterns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sequence variant of hydra.python.syntax.Patterns
patternsSequence :: Phantoms.TTerm Syntax.OpenSequencePattern -> Phantoms.TTerm Syntax.Patterns
patternsSequence x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Patterns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the minus variant of hydra.python.syntax.PlusOrMinus
plusOrMinusMinus :: Phantoms.TTerm Syntax.PlusOrMinus
plusOrMinusMinus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PlusOrMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.python.syntax.PlusOrMinus
plusOrMinusPlus :: Phantoms.TTerm Syntax.PlusOrMinus
plusOrMinusPlus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PlusOrMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assignment variant of hydra.python.syntax.PosArg
posArgAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.PosArg
posArgAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.PosArg
posArgExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PosArg
posArgExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.PosArg
posArgStarred :: Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.PosArg
posArgStarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.PositionalPatterns wrapper
positionalPatterns :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.PositionalPatterns
positionalPatterns x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.PositionalPatterns"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Power
power :: Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm (Maybe Syntax.Factor) -> Phantoms.TTerm Syntax.Power
power lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Power"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Power
powerLhs :: Phantoms.TTerm Syntax.Power -> Phantoms.TTerm Syntax.AwaitPrimary
powerLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Power
powerRhs :: Phantoms.TTerm Syntax.Power -> Phantoms.TTerm (Maybe Syntax.Factor)
powerRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Power
powerWithLhs :: Phantoms.TTerm Syntax.Power -> Phantoms.TTerm Syntax.AwaitPrimary -> Phantoms.TTerm Syntax.Power
powerWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Power"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Power
powerWithRhs :: Phantoms.TTerm Syntax.Power -> Phantoms.TTerm (Maybe Syntax.Factor) -> Phantoms.TTerm Syntax.Power
powerWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Power"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the compound variant of hydra.python.syntax.Primary
primaryCompound :: Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.Primary
primaryCompound x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the call variant of hydra.python.syntax.PrimaryRhs
primaryRhsCall :: Phantoms.TTerm Syntax.Args -> Phantoms.TTerm Syntax.PrimaryRhs
primaryRhsCall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the genexp variant of hydra.python.syntax.PrimaryRhs
primaryRhsGenexp :: Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.PrimaryRhs
primaryRhsGenexp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genexp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the project variant of hydra.python.syntax.PrimaryRhs
primaryRhsProject :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PrimaryRhs
primaryRhsProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slices variant of hydra.python.syntax.PrimaryRhs
primaryRhsSlices :: Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.PrimaryRhs
primaryRhsSlices x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slices"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Primary
primarySimple :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.Primary
primarySimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.PrimaryWithRhs
primaryWithRhs :: Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.PrimaryRhs -> Phantoms.TTerm Syntax.PrimaryWithRhs
primaryWithRhs primary rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the primary field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsPrimary :: Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.Primary
primaryWithRhsPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsRhs :: Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.PrimaryRhs
primaryWithRhsRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the primary field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsWithPrimary :: Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.Primary -> Phantoms.TTerm Syntax.PrimaryWithRhs
primaryWithRhsWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsWithRhs :: Phantoms.TTerm Syntax.PrimaryWithRhs -> Phantoms.TTerm Syntax.PrimaryRhs -> Phantoms.TTerm Syntax.PrimaryWithRhs
primaryWithRhsWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the double variant of hydra.python.syntax.QuoteStyle
quoteStyleDouble :: Phantoms.TTerm Syntax.QuoteStyle
quoteStyleDouble =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the single variant of hydra.python.syntax.QuoteStyle
quoteStyleSingle :: Phantoms.TTerm Syntax.QuoteStyle
quoteStyleSingle =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tripleDouble variant of hydra.python.syntax.QuoteStyle
quoteStyleTripleDouble :: Phantoms.TTerm Syntax.QuoteStyle
quoteStyleTripleDouble =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tripleDouble"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tripleSingle variant of hydra.python.syntax.QuoteStyle
quoteStyleTripleSingle :: Phantoms.TTerm Syntax.QuoteStyle
quoteStyleTripleSingle =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tripleSingle"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.RaiseExpression
raiseExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.RaiseExpression
raiseExpression expression from =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm from)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.RaiseExpression
raiseExpressionExpression :: Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm Syntax.Expression
raiseExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the from field of hydra.python.syntax.RaiseExpression
raiseExpressionFrom :: Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
raiseExpressionFrom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
        Core.projectionField = (Core.Name "from")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.RaiseExpression
raiseExpressionWithExpression :: Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RaiseExpression
raiseExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
              Core.projectionField = (Core.Name "from")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the from field of hydra.python.syntax.RaiseExpression
raiseExpressionWithFrom :: Phantoms.TTerm Syntax.RaiseExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.RaiseExpression
raiseExpressionWithFrom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "from"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.RaiseStatement wrapper
raiseStatement :: Phantoms.TTerm (Maybe Syntax.RaiseExpression) -> Phantoms.TTerm Syntax.RaiseStatement
raiseStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.RaiseStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the float variant of hydra.python.syntax.RealNumber
realNumberFloat :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.RealNumber
realNumberFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.python.syntax.RealNumber
realNumberInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.RealNumber
realNumberInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the dot variant of hydra.python.syntax.RelativeImportPrefix
relativeImportPrefixDot :: Phantoms.TTerm Syntax.RelativeImportPrefix
relativeImportPrefixDot =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RelativeImportPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the ellipsis variant of hydra.python.syntax.RelativeImportPrefix
relativeImportPrefixEllipsis :: Phantoms.TTerm Syntax.RelativeImportPrefix
relativeImportPrefixEllipsis =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RelativeImportPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ellipsis"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.python.syntax.ReturnStatement wrapper
returnStatement :: Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.ReturnStatement
returnStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the list variant of hydra.python.syntax.SequencePattern
sequencePatternList :: Phantoms.TTerm (Maybe Syntax.MaybeSequencePattern) -> Phantoms.TTerm Syntax.SequencePattern
sequencePatternList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SequencePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.SequencePattern
sequencePatternTuple :: Phantoms.TTerm (Maybe Syntax.OpenSequencePattern) -> Phantoms.TTerm Syntax.SequencePattern
sequencePatternTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SequencePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Set wrapper
set :: Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.Set
set x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Set"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.Setcomp
setcomp :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Setcomp
setcomp expression forIfClauses =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Phantoms.unTTerm forIfClauses)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.Setcomp
setcompExpression :: Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.NamedExpression
setcompExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Setcomp
setcompForIfClauses :: Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.ForIfClauses
setcompForIfClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
        Core.projectionField = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.Setcomp
setcompWithExpression :: Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Setcomp
setcompWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
              Core.projectionField = (Core.Name "forIfClauses")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Setcomp
setcompWithForIfClauses :: Phantoms.TTerm Syntax.Setcomp -> Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm Syntax.Setcomp
setcompWithForIfClauses original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "forIfClauses"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ShiftExpression
shiftExpression :: Phantoms.TTerm (Maybe Syntax.ShiftLhs) -> Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpression lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.ShiftExpression
shiftExpressionLhs :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm (Maybe Syntax.ShiftLhs)
shiftExpressionLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.ShiftExpression
shiftExpressionRhs :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.Sum
shiftExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.ShiftExpression
shiftExpressionWithLhs :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm (Maybe Syntax.ShiftLhs) -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.ShiftExpression
shiftExpressionWithRhs :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.ShiftExpression
shiftExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.ShiftLhs
shiftLhs :: Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.ShiftOp -> Phantoms.TTerm Syntax.ShiftLhs
shiftLhs operand operator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)}]}))
-- | DSL accessor for the operand field of hydra.python.syntax.ShiftLhs
shiftLhsOperand :: Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftExpression
shiftLhsOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
        Core.projectionField = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.ShiftLhs
shiftLhsOperator :: Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftOp
shiftLhsOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.ShiftLhs
shiftLhsWithOperand :: Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftExpression -> Phantoms.TTerm Syntax.ShiftLhs
shiftLhsWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.python.syntax.ShiftLhs
shiftLhsWithOperator :: Phantoms.TTerm Syntax.ShiftLhs -> Phantoms.TTerm Syntax.ShiftOp -> Phantoms.TTerm Syntax.ShiftLhs
shiftLhsWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
              Core.projectionField = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the left variant of hydra.python.syntax.ShiftOp
shiftOpLeft :: Phantoms.TTerm Syntax.ShiftOp
shiftOpLeft =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ShiftOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "left"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the right variant of hydra.python.syntax.ShiftOp
shiftOpRight :: Phantoms.TTerm Syntax.ShiftOp
shiftOpRight =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ShiftOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "right"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.SignedNumber
signedNumberNumber :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.SignedNumber
signedNumberNumber x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sign variant of hydra.python.syntax.SignedNumber
signedNumberSign :: Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.SignedNumber
signedNumberSign x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the number variant of hydra.python.syntax.SignedRealNumber
signedRealNumberNumber :: Phantoms.TTerm Syntax.RealNumber -> Phantoms.TTerm Syntax.SignedRealNumber
signedRealNumberNumber x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedRealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the sign variant of hydra.python.syntax.SignedRealNumber
signedRealNumberSign :: Phantoms.TTerm Syntax.PlusOrMinus -> Phantoms.TTerm Syntax.SignedRealNumber
signedRealNumberSign x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedRealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the assert variant of hydra.python.syntax.SimpleStatement
simpleStatementAssert :: Phantoms.TTerm Syntax.AssertStatement -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementAssert x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the assignment variant of hydra.python.syntax.SimpleStatement
simpleStatementAssignment :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the break variant of hydra.python.syntax.SimpleStatement
simpleStatementBreak :: Phantoms.TTerm Syntax.SimpleStatement
simpleStatementBreak =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the continue variant of hydra.python.syntax.SimpleStatement
simpleStatementContinue :: Phantoms.TTerm Syntax.SimpleStatement
simpleStatementContinue =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the del variant of hydra.python.syntax.SimpleStatement
simpleStatementDel :: Phantoms.TTerm Syntax.DelStatement -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementDel x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "del"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the global variant of hydra.python.syntax.SimpleStatement
simpleStatementGlobal :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementGlobal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the import variant of hydra.python.syntax.SimpleStatement
simpleStatementImport :: Phantoms.TTerm Syntax.ImportStatement -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementImport x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the nonlocal variant of hydra.python.syntax.SimpleStatement
simpleStatementNonlocal :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementNonlocal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonlocal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pass variant of hydra.python.syntax.SimpleStatement
simpleStatementPass :: Phantoms.TTerm Syntax.SimpleStatement
simpleStatementPass =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pass"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the raise variant of hydra.python.syntax.SimpleStatement
simpleStatementRaise :: Phantoms.TTerm Syntax.RaiseStatement -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementRaise x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raise"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the return variant of hydra.python.syntax.SimpleStatement
simpleStatementReturn :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementReturn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starExpressions variant of hydra.python.syntax.SimpleStatement
simpleStatementStarExpressions :: Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementStarExpressions x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starExpressions"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeAlias variant of hydra.python.syntax.SimpleStatement
simpleStatementTypeAlias :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementTypeAlias x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAlias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the yield variant of hydra.python.syntax.SimpleStatement
simpleStatementYield :: Phantoms.TTerm Syntax.YieldStatement -> Phantoms.TTerm Syntax.SimpleStatement
simpleStatementYield x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.SimpleTypeParameter
simpleTypeParameter :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleTypeParameter
simpleTypeParameter name bound default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm bound)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the bound field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterBound :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression)
simpleTypeParameterBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionField = (Core.Name "bound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the default field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterDefault :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression)
simpleTypeParameterDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterName :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm Syntax.Name
simpleTypeParameterName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bound field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithBound :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleTypeParameter
simpleTypeParameterWithBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the default field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithDefault :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleTypeParameter
simpleTypeParameterWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithName :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SimpleTypeParameter
simpleTypeParameterWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionField = (Core.Name "bound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndName :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleSubscriptAttributeTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndSlices :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndSlices x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleSubscriptAttributeTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.SingleTarget
singleTargetName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.SingleTarget
singleTargetName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the parens variant of hydra.python.syntax.SingleTarget
singleTargetParens :: Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.SingleTarget
singleTargetParens x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the subscriptAttributeTarget variant of hydra.python.syntax.SingleTarget
singleTargetSubscriptAttributeTarget :: Phantoms.TTerm Syntax.SingleSubscriptAttributeTarget -> Phantoms.TTerm Syntax.SingleTarget
singleTargetSubscriptAttributeTarget x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subscriptAttributeTarget"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.SlashNoDefault wrapper
slashNoDefault :: Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashNoDefault
slashNoDefault x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.SlashNoDefault"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParameters :: Phantoms.TTerm Syntax.SlashNoDefault -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParameters slash paramNoDefault paramWithDefault starEtc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Phantoms.unTTerm slash)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersParamNoDefault :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault]
slashNoDefaultParametersParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersParamWithDefault :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault]
slashNoDefaultParametersParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the slash field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersSlash :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm Syntax.SlashNoDefault
slashNoDefaultParametersSlash x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionField = (Core.Name "slash")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersStarEtc :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc)
slashNoDefaultParametersStarEtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionField = (Core.Name "starEtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamNoDefault :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "slash")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamWithDefault :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "slash")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the slash field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithSlash :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm Syntax.SlashNoDefault -> Phantoms.TTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithSlash original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithStarEtc :: Phantoms.TTerm Syntax.SlashNoDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithStarEtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "slash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "slash")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.SlashWithDefault
slashWithDefault :: Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashWithDefault
slashWithDefault paramNoDefault paramWithDefault =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultParamNoDefault :: Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamNoDefault]
slashWithDefaultParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultParamWithDefault :: Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamWithDefault]
slashWithDefaultParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParameters :: Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParameters paramNoDefault paramWithDefault starEtc =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramNoDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm paramWithDefault)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm starEtc)}]}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersParamNoDefault :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault]
slashWithDefaultParametersParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionField = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersParamWithDefault :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault]
slashWithDefaultParametersParamWithDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionField = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersStarEtc :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc)
slashWithDefaultParametersStarEtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionField = (Core.Name "starEtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamNoDefault :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamWithDefault :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionField = (Core.Name "starEtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the starEtc field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithStarEtc :: Phantoms.TTerm Syntax.SlashWithDefaultParameters -> Phantoms.TTerm (Maybe Syntax.StarEtc) -> Phantoms.TTerm Syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithStarEtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "starEtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultWithParamNoDefault :: Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamNoDefault] -> Phantoms.TTerm Syntax.SlashWithDefault
slashWithDefaultWithParamNoDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
              Core.projectionField = (Core.Name "paramWithDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultWithParamWithDefault :: Phantoms.TTerm Syntax.SlashWithDefault -> Phantoms.TTerm [Syntax.ParamWithDefault] -> Phantoms.TTerm Syntax.SlashWithDefault
slashWithDefaultWithParamWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "paramNoDefault"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
              Core.projectionField = (Core.Name "paramNoDefault")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramWithDefault"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.SliceExpression
sliceExpression :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression
sliceExpression start stop step =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Phantoms.unTTerm stop)},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Phantoms.unTTerm step)}]}))
-- | DSL accessor for the start field of hydra.python.syntax.SliceExpression
sliceExpressionStart :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
sliceExpressionStart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionField = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the step field of hydra.python.syntax.SliceExpression
sliceExpressionStep :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
sliceExpressionStep x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionField = (Core.Name "step")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the stop field of hydra.python.syntax.SliceExpression
sliceExpressionStop :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression)
sliceExpressionStop x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionField = (Core.Name "stop")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the start field of hydra.python.syntax.SliceExpression
sliceExpressionWithStart :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression
sliceExpressionWithStart original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionField = (Core.Name "stop")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionField = (Core.Name "step")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the step field of hydra.python.syntax.SliceExpression
sliceExpressionWithStep :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression
sliceExpressionWithStep original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionField = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionField = (Core.Name "stop")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the stop field of hydra.python.syntax.SliceExpression
sliceExpressionWithStop :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SliceExpression
sliceExpressionWithStop original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionField = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stop"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "step"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
              Core.projectionField = (Core.Name "step")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the named variant of hydra.python.syntax.Slice
sliceNamed :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Slice
sliceNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slice variant of hydra.python.syntax.SliceOrStarredExpression
sliceOrStarredExpressionSlice :: Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.SliceOrStarredExpression
sliceOrStarredExpressionSlice x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SliceOrStarredExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.SliceOrStarredExpression
sliceOrStarredExpressionStarred :: Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.SliceOrStarredExpression
sliceOrStarredExpressionStarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SliceOrStarredExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slice_ variant of hydra.python.syntax.Slice
sliceSlice_ :: Phantoms.TTerm Syntax.SliceExpression -> Phantoms.TTerm Syntax.Slice
sliceSlice_ x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice_"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Slices
slices :: Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm [Syntax.SliceOrStarredExpression] -> Phantoms.TTerm Syntax.Slices
slices head tail =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Slices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm tail)}]}))
-- | DSL accessor for the head field of hydra.python.syntax.Slices
slicesHead :: Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.Slice
slicesHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
        Core.projectionField = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tail field of hydra.python.syntax.Slices
slicesTail :: Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm [Syntax.SliceOrStarredExpression]
slicesTail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
        Core.projectionField = (Core.Name "tail")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.Slices
slicesWithHead :: Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.Slices
slicesWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Slices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
              Core.projectionField = (Core.Name "tail")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tail field of hydra.python.syntax.Slices
slicesWithTail :: Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm [Syntax.SliceOrStarredExpression] -> Phantoms.TTerm Syntax.Slices
slicesWithTail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Slices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
              Core.projectionField = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.StarAnnotation wrapper
starAnnotation :: Phantoms.TTerm Syntax.StarExpression -> Phantoms.TTerm Syntax.StarAnnotation
starAnnotation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarAnnotation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the name variant of hydra.python.syntax.StarAtom
starAtomName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.StarAtom
starAtomName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starTargetsListSeq variant of hydra.python.syntax.StarAtom
starAtomStarTargetsListSeq :: Phantoms.TTerm (Maybe Syntax.StarTargetsListSeq) -> Phantoms.TTerm Syntax.StarAtom
starAtomStarTargetsListSeq x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starTargetsListSeq"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starTargetsTupleSeq variant of hydra.python.syntax.StarAtom
starAtomStarTargetsTupleSeq :: Phantoms.TTerm (Maybe Syntax.StarTargetsTupleSeq) -> Phantoms.TTerm Syntax.StarAtom
starAtomStarTargetsTupleSeq x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starTargetsTupleSeq"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the targetWithStarAtom variant of hydra.python.syntax.StarAtom
starAtomTargetWithStarAtom :: Phantoms.TTerm Syntax.TargetWithStarAtom -> Phantoms.TTerm Syntax.StarAtom
starAtomTargetWithStarAtom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetWithStarAtom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the keywords variant of hydra.python.syntax.StarEtc
starEtcKeywords :: Phantoms.TTerm Syntax.Keywords -> Phantoms.TTerm Syntax.StarEtc
starEtcKeywords x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keywords"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starComma variant of hydra.python.syntax.StarEtc
starEtcStarComma :: Phantoms.TTerm Syntax.CommaStarEtc -> Phantoms.TTerm Syntax.StarEtc
starEtcStarComma x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starComma"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starNoDefault variant of hydra.python.syntax.StarEtc
starEtcStarNoDefault :: Phantoms.TTerm Syntax.NoDefaultStarEtc -> Phantoms.TTerm Syntax.StarEtc
starEtcStarNoDefault x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starNoDefault"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starNoDefaultStarAnnotation variant of hydra.python.syntax.StarEtc
starEtcStarNoDefaultStarAnnotation :: Phantoms.TTerm Syntax.NoDefaultStarAnnotationStarEtc -> Phantoms.TTerm Syntax.StarEtc
starEtcStarNoDefaultStarAnnotation x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starNoDefaultStarAnnotation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.StarExpression
starExpressionSimple :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.StarExpression
starExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.StarExpression
starExpressionStar :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.StarExpression
starExpressionStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.StarNamedExpression
starNamedExpressionSimple :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.StarNamedExpression
starNamedExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarNamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.StarNamedExpression
starNamedExpressionStar :: Phantoms.TTerm Syntax.BitwiseOr -> Phantoms.TTerm Syntax.StarNamedExpression
starNamedExpressionStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarNamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.StarNamedExpressions wrapper
starNamedExpressions :: Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.StarNamedExpressions
starNamedExpressions x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarNamedExpressions"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the capture variant of hydra.python.syntax.StarPattern
starPatternCapture :: Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.StarPattern
starPatternCapture x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "capture"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.python.syntax.StarPattern
starPatternWildcard :: Phantoms.TTerm Syntax.StarPattern
starPatternWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the starred variant of hydra.python.syntax.StarTarget
starTargetStarred :: Phantoms.TTerm Syntax.StarTarget -> Phantoms.TTerm Syntax.StarTarget
starTargetStarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the unstarred variant of hydra.python.syntax.StarTarget
starTargetUnstarred :: Phantoms.TTerm Syntax.TargetWithStarAtom -> Phantoms.TTerm Syntax.StarTarget
starTargetUnstarred x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unstarred"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.StarTargetsListSeq wrapper
starTargetsListSeq :: Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.StarTargetsListSeq
starTargetsListSeq x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarTargetsListSeq"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.python.syntax.StarTargetsTupleSeq wrapper
starTargetsTupleSeq :: Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.StarTargetsTupleSeq
starTargetsTupleSeq x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarTargetsTupleSeq"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.StarTypeParameter
starTypeParameter :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.StarExpression) -> Phantoms.TTerm Syntax.StarTypeParameter
starTypeParameter name default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))
-- | DSL accessor for the default field of hydra.python.syntax.StarTypeParameter
starTypeParameterDefault :: Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm (Maybe Syntax.StarExpression)
starTypeParameterDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
        Core.projectionField = (Core.Name "default")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.StarTypeParameter
starTypeParameterName :: Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm Syntax.Name
starTypeParameterName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.StarTypeParameter
starTypeParameterWithDefault :: Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm (Maybe Syntax.StarExpression) -> Phantoms.TTerm Syntax.StarTypeParameter
starTypeParameterWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.StarTypeParameter
starTypeParameterWithName :: Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.StarTypeParameter
starTypeParameterWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
              Core.projectionField = (Core.Name "default")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.StarredExpression wrapper
starredExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.StarredExpression
starredExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarredExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the annotated variant of hydra.python.syntax.Statement
statementAnnotated :: Phantoms.TTerm Syntax.AnnotatedStatement -> Phantoms.TTerm Syntax.Statement
statementAnnotated x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the compound variant of hydra.python.syntax.Statement
statementCompound :: Phantoms.TTerm Syntax.CompoundStatement -> Phantoms.TTerm Syntax.Statement
statementCompound x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Statement
statementSimple :: Phantoms.TTerm [Syntax.SimpleStatement] -> Phantoms.TTerm Syntax.Statement
statementSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.String
string :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe Syntax.StringPrefix) -> Phantoms.TTerm Syntax.QuoteStyle -> Phantoms.TTerm Syntax.String_
string value prefix quoteStyle =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Phantoms.unTTerm quoteStyle)}]}))
-- | DSL accessor for the prefix field of hydra.python.syntax.String
stringPrefix :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm (Maybe Syntax.StringPrefix)
stringPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionField = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the bytes variant of hydra.python.syntax.StringPrefix
stringPrefixBytes :: Phantoms.TTerm Syntax.StringPrefix
stringPrefixBytes =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the raw variant of hydra.python.syntax.StringPrefix
stringPrefixRaw :: Phantoms.TTerm Syntax.StringPrefix
stringPrefixRaw =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raw"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rawBytes variant of hydra.python.syntax.StringPrefix
stringPrefixRawBytes :: Phantoms.TTerm Syntax.StringPrefix
stringPrefixRawBytes =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rawBytes"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unicode variant of hydra.python.syntax.StringPrefix
stringPrefixUnicode :: Phantoms.TTerm Syntax.StringPrefix
stringPrefixUnicode =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unicode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the quoteStyle field of hydra.python.syntax.String
stringQuoteStyle :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.QuoteStyle
stringQuoteStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionField = (Core.Name "quoteStyle")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.String
stringValue :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm String
stringValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the prefix field of hydra.python.syntax.String
stringWithPrefix :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm (Maybe Syntax.StringPrefix) -> Phantoms.TTerm Syntax.String_
stringWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionField = (Core.Name "quoteStyle")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the quoteStyle field of hydra.python.syntax.String
stringWithQuoteStyle :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm Syntax.QuoteStyle -> Phantoms.TTerm Syntax.String_
stringWithQuoteStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.python.syntax.String
stringWithValue :: Phantoms.TTerm Syntax.String_ -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.String_
stringWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.String"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "quoteStyle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
              Core.projectionField = (Core.Name "quoteStyle")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the simple variant of hydra.python.syntax.SubjectExpression
subjectExpressionSimple :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.SubjectExpression
subjectExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SubjectExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.SubjectExpression
subjectExpressionTuple :: Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.SubjectExpression
subjectExpressionTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SubjectExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Sum
sum :: Phantoms.TTerm (Maybe Syntax.SumLhs) -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Sum
sum lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Sum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Sum
sumLhs :: Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm (Maybe Syntax.SumLhs)
sumLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.SumLhs
sumLhs2 :: Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.SumOp -> Phantoms.TTerm Syntax.SumLhs
sumLhs2 operand operator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)}]}))
-- | DSL accessor for the operand field of hydra.python.syntax.SumLhs
sumLhsOperand :: Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.Sum
sumLhsOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
        Core.projectionField = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.SumLhs
sumLhsOperator :: Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.SumOp
sumLhsOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.SumLhs
sumLhsWithOperand :: Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.SumLhs
sumLhsWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.python.syntax.SumLhs
sumLhsWithOperator :: Phantoms.TTerm Syntax.SumLhs -> Phantoms.TTerm Syntax.SumOp -> Phantoms.TTerm Syntax.SumLhs
sumLhsWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
              Core.projectionField = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the add variant of hydra.python.syntax.SumOp
sumOpAdd :: Phantoms.TTerm Syntax.SumOp
sumOpAdd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SumOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sub variant of hydra.python.syntax.SumOp
sumOpSub :: Phantoms.TTerm Syntax.SumOp
sumOpSub =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SumOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sub"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Sum
sumRhs :: Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.Term
sumRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Sum
sumWithLhs :: Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm (Maybe Syntax.SumLhs) -> Phantoms.TTerm Syntax.Sum
sumWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Sum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Sum
sumWithRhs :: Phantoms.TTerm Syntax.Sum -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Sum
sumWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Sum"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArguments :: Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.TPrimaryAndArguments
tPrimaryAndArguments primary arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsArguments :: Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm (Maybe Syntax.Args)
tPrimaryAndArgumentsArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsPrimary :: Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm Syntax.TPrimary
tPrimaryAndArgumentsPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithArguments :: Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm (Maybe Syntax.Args) -> Phantoms.TTerm Syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithPrimary :: Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexp :: Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.TPrimaryAndGenexp
tPrimaryAndGenexp primary genexp =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "genexp"),
          Core.fieldTerm = (Phantoms.unTTerm genexp)}]}))
-- | DSL accessor for the genexp field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpGenexp :: Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.Genexp
tPrimaryAndGenexpGenexp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
        Core.projectionField = (Core.Name "genexp")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpPrimary :: Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.TPrimary
tPrimaryAndGenexpPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the genexp field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithGenexp :: Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.Genexp -> Phantoms.TTerm Syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithGenexp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "genexp"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithPrimary :: Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "genexp"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
              Core.projectionField = (Core.Name "genexp")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndName
tPrimaryAndName :: Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TPrimaryAndName
tPrimaryAndName primary name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameName :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.Name
tPrimaryAndNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNamePrimary :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TPrimary
tPrimaryAndNamePrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameWithName :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TPrimaryAndName
tPrimaryAndNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameWithPrimary :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndName
tPrimaryAndNameWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlices :: Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.TPrimaryAndSlices
tPrimaryAndSlices primary slices =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm primary)},
        Core.Field {
          Core.fieldName = (Core.Name "slices"),
          Core.fieldTerm = (Phantoms.unTTerm slices)}]}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesPrimary :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TPrimary
tPrimaryAndSlicesPrimary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
        Core.projectionField = (Core.Name "primary")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the slices field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesSlices :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.Slices
tPrimaryAndSlicesSlices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
        Core.projectionField = (Core.Name "slices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithPrimary :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TPrimary -> Phantoms.TTerm Syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithPrimary original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "slices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
              Core.projectionField = (Core.Name "slices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the slices field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithSlices :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.Slices -> Phantoms.TTerm Syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithSlices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "primary"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
              Core.projectionField = (Core.Name "primary")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "slices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the atom variant of hydra.python.syntax.TPrimary
tPrimaryAtom :: Phantoms.TTerm Syntax.Atom -> Phantoms.TTerm Syntax.TPrimary
tPrimaryAtom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndArguments variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndArguments :: Phantoms.TTerm Syntax.TPrimaryAndArguments -> Phantoms.TTerm Syntax.TPrimary
tPrimaryPrimaryAndArguments x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndArguments"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndGenexp variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndGenexp :: Phantoms.TTerm Syntax.TPrimaryAndGenexp -> Phantoms.TTerm Syntax.TPrimary
tPrimaryPrimaryAndGenexp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndGenexp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndName :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TPrimary
tPrimaryPrimaryAndName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndSlices :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TPrimary
tPrimaryPrimaryAndSlices x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the atom variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomAtom :: Phantoms.TTerm Syntax.StarAtom -> Phantoms.TTerm Syntax.TargetWithStarAtom
targetWithStarAtomAtom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the project variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomProject :: Phantoms.TTerm Syntax.TPrimaryAndName -> Phantoms.TTerm Syntax.TargetWithStarAtom
targetWithStarAtomProject x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slices variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomSlices :: Phantoms.TTerm Syntax.TPrimaryAndSlices -> Phantoms.TTerm Syntax.TargetWithStarAtom
targetWithStarAtomSlices x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slices"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Term
term :: Phantoms.TTerm (Maybe Syntax.TermLhs) -> Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Term
term lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Term"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.Term
termLhs :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.TermLhs)
termLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.TermLhs
termLhs2 :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermOp -> Phantoms.TTerm Syntax.TermLhs
termLhs2 operand operator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)}]}))
-- | DSL accessor for the operand field of hydra.python.syntax.TermLhs
termLhsOperand :: Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.Term
termLhsOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
        Core.projectionField = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.TermLhs
termLhsOperator :: Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.TermOp
termLhsOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.TermLhs
termLhsWithOperand :: Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.TermLhs
termLhsWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.python.syntax.TermLhs
termLhsWithOperator :: Phantoms.TTerm Syntax.TermLhs -> Phantoms.TTerm Syntax.TermOp -> Phantoms.TTerm Syntax.TermLhs
termLhsWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
              Core.projectionField = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the div variant of hydra.python.syntax.TermOp
termOpDiv :: Phantoms.TTerm Syntax.TermOp
termOpDiv =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the floordiv variant of hydra.python.syntax.TermOp
termOpFloordiv :: Phantoms.TTerm Syntax.TermOp
termOpFloordiv =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floordiv"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the matmul variant of hydra.python.syntax.TermOp
termOpMatmul :: Phantoms.TTerm Syntax.TermOp
termOpMatmul =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "matmul"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mod variant of hydra.python.syntax.TermOp
termOpMod :: Phantoms.TTerm Syntax.TermOp
termOpMod =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mul variant of hydra.python.syntax.TermOp
termOpMul :: Phantoms.TTerm Syntax.TermOp
termOpMul =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mul"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Term
termRhs :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Factor
termRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Term
termWithLhs :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm (Maybe Syntax.TermLhs) -> Phantoms.TTerm Syntax.Term
termWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Term"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.Term
termWithRhs :: Phantoms.TTerm Syntax.Term -> Phantoms.TTerm Syntax.Factor -> Phantoms.TTerm Syntax.Term
termWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.Term"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatement :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.ExceptStarBlock] -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStarStatement
tryExceptStarStatement body excepts else_ finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Phantoms.unTTerm excepts)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm finally)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementBody :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm Syntax.Block
tryExceptStarStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementElse :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block)
tryExceptStarStatementElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the excepts field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementExcepts :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm [Syntax.ExceptStarBlock]
tryExceptStarStatementExcepts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionField = (Core.Name "excepts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementFinally :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block)
tryExceptStarStatementFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionField = (Core.Name "finally")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithBody :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "excepts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithElse :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "excepts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the excepts field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithExcepts :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm [Syntax.ExceptStarBlock] -> Phantoms.TTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithExcepts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithFinally :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStarStatement
tryExceptStarStatementWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "excepts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TryExceptStatement
tryExceptStatement :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.ExceptBlock] -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStatement
tryExceptStatement body excepts else_ finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Phantoms.unTTerm excepts)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm finally)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.TryExceptStatement
tryExceptStatementBody :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm Syntax.Block
tryExceptStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.TryExceptStatement
tryExceptStatementElse :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block)
tryExceptStatementElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the excepts field of hydra.python.syntax.TryExceptStatement
tryExceptStatementExcepts :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm [Syntax.ExceptBlock]
tryExceptStatementExcepts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionField = (Core.Name "excepts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryExceptStatement
tryExceptStatementFinally :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block)
tryExceptStatementFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionField = (Core.Name "finally")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithBody :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryExceptStatement
tryExceptStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "excepts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithElse :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStatement
tryExceptStatementWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "excepts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the excepts field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithExcepts :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm [Syntax.ExceptBlock] -> Phantoms.TTerm Syntax.TryExceptStatement
tryExceptStatementWithExcepts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithFinally :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.TryExceptStatement
tryExceptStatementWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "excepts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "excepts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.TryFinallyStatement
tryFinallyStatement :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryFinallyStatement
tryFinallyStatement body finally =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm finally)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementBody :: Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block
tryFinallyStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementFinally :: Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block
tryFinallyStatementFinally x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
        Core.projectionField = (Core.Name "finally")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementWithBody :: Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryFinallyStatement
tryFinallyStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
              Core.projectionField = (Core.Name "finally")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the finally field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementWithFinally :: Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.TryFinallyStatement
tryFinallyStatementWithFinally original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finally"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the except variant of hydra.python.syntax.TryStatement
tryStatementExcept :: Phantoms.TTerm Syntax.TryExceptStatement -> Phantoms.TTerm Syntax.TryStatement
tryStatementExcept x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "except"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the exceptStar variant of hydra.python.syntax.TryStatement
tryStatementExceptStar :: Phantoms.TTerm Syntax.TryExceptStarStatement -> Phantoms.TTerm Syntax.TryStatement
tryStatementExceptStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exceptStar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the finally variant of hydra.python.syntax.TryStatement
tryStatementFinally :: Phantoms.TTerm Syntax.TryFinallyStatement -> Phantoms.TTerm Syntax.TryStatement
tryStatementFinally x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finally"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Tuple wrapper
tuple :: Phantoms.TTerm [Syntax.StarNamedExpression] -> Phantoms.TTerm Syntax.Tuple
tuple x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Tuple"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.TypeAlias
typeAlias :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAlias
typeAlias name typeParams expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.python.syntax.TypeAlias
typeAliasExpression :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Expression
typeAliasExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.TypeAlias
typeAliasName :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Name
typeAliasName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.TypeAlias
typeAliasTypeParams :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm [Syntax.TypeParameter]
typeAliasTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionField = (Core.Name "typeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.TypeAlias
typeAliasWithExpression :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.python.syntax.TypeAlias
typeAliasWithName :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.python.syntax.TypeAlias
typeAliasWithTypeParams :: Phantoms.TTerm Syntax.TypeAlias -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.TypeAlias
typeAliasWithTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.python.syntax.TypeComment wrapper
typeComment :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypeComment
typeComment x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.TypeComment"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the doubleStarredExpression variant of hydra.python.syntax.TypeExpression
typeExpressionDoubleStarredExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionDoubleStarredExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStarredExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.TypeExpression
typeExpressionExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the starredExpression variant of hydra.python.syntax.TypeExpression
typeExpressionStarredExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionStarredExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starredExpression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the doubleStar variant of hydra.python.syntax.TypeParameter
typeParameterDoubleStar :: Phantoms.TTerm Syntax.DoubleStarTypeParameter -> Phantoms.TTerm Syntax.TypeParameter
typeParameterDoubleStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.TypeParameter
typeParameterSimple :: Phantoms.TTerm Syntax.SimpleTypeParameter -> Phantoms.TTerm Syntax.TypeParameter
typeParameterSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.TypeParameter
typeParameterStar :: Phantoms.TTerm Syntax.StarTypeParameter -> Phantoms.TTerm Syntax.TypeParameter
typeParameterStar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.python.syntax.TypedAssignment
typedAssignment :: Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.AnnotatedRhs) -> Phantoms.TTerm Syntax.TypedAssignment
typedAssignment lhs type_ rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.python.syntax.TypedAssignment
typedAssignmentLhs :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.SingleTarget
typedAssignmentLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.TypedAssignment
typedAssignmentRhs :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm (Maybe Syntax.AnnotatedRhs)
typedAssignmentRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.python.syntax.TypedAssignment
typedAssignmentType :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.Expression
typedAssignmentType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.TypedAssignment
typedAssignmentWithLhs :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.SingleTarget -> Phantoms.TTerm Syntax.TypedAssignment
typedAssignmentWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.python.syntax.TypedAssignment
typedAssignmentWithRhs :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm (Maybe Syntax.AnnotatedRhs) -> Phantoms.TTerm Syntax.TypedAssignment
typedAssignmentWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.python.syntax.TypedAssignment
typedAssignmentWithType :: Phantoms.TTerm Syntax.TypedAssignment -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypedAssignment
typedAssignmentWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the body of hydra.python.syntax.Annotation
unAnnotation :: Phantoms.TTerm Syntax.Annotation -> Phantoms.TTerm Syntax.Expression
unAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Annotation")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Attribute
unAttribute :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm [Syntax.Name]
unAttribute x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Attribute")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.CapturePattern
unCapturePattern :: Phantoms.TTerm Syntax.CapturePattern -> Phantoms.TTerm Syntax.PatternCaptureTarget
unCapturePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.CapturePattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Conjunction
unConjunction :: Phantoms.TTerm Syntax.Conjunction -> Phantoms.TTerm [Syntax.Inversion]
unConjunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Conjunction")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Decorators
unDecorators :: Phantoms.TTerm Syntax.Decorators -> Phantoms.TTerm [Syntax.NamedExpression]
unDecorators x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Decorators")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Default
unDefault :: Phantoms.TTerm Syntax.Default -> Phantoms.TTerm Syntax.Expression
unDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Default")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DelStatement
unDelStatement :: Phantoms.TTerm Syntax.DelStatement -> Phantoms.TTerm Syntax.DelTargets
unDelStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DelStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DelTargets
unDelTargets :: Phantoms.TTerm Syntax.DelTargets -> Phantoms.TTerm [Syntax.DelTarget]
unDelTargets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DelTargets")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Dict
unDict :: Phantoms.TTerm Syntax.Dict -> Phantoms.TTerm [Syntax.DoubleStarredKvpair]
unDict x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Dict")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Disjunction
unDisjunction :: Phantoms.TTerm Syntax.Disjunction -> Phantoms.TTerm [Syntax.Conjunction]
unDisjunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Disjunction")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DottedName
unDottedName :: Phantoms.TTerm Syntax.DottedName -> Phantoms.TTerm [Syntax.Name]
unDottedName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DottedName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DoubleStarPattern
unDoubleStarPattern :: Phantoms.TTerm Syntax.DoubleStarPattern -> Phantoms.TTerm Syntax.PatternCaptureTarget
unDoubleStarPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DoubleStarPattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Eval
unEval :: Phantoms.TTerm Syntax.Eval -> Phantoms.TTerm [Syntax.Expression]
unEval x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Eval")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.File
unFile :: Phantoms.TTerm Syntax.File -> Phantoms.TTerm [Syntax.Statement]
unFile x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.File")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ForIfClauses
unForIfClauses :: Phantoms.TTerm Syntax.ForIfClauses -> Phantoms.TTerm [Syntax.ForIfClause]
unForIfClauses x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ForIfClauses")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.FuncTypeComment
unFuncTypeComment :: Phantoms.TTerm Syntax.FuncTypeComment -> Phantoms.TTerm Syntax.TypeComment
unFuncTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.FuncTypeComment")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.GroupPattern
unGroupPattern :: Phantoms.TTerm Syntax.GroupPattern -> Phantoms.TTerm Syntax.Pattern
unGroupPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.GroupPattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Guard
unGuard :: Phantoms.TTerm Syntax.Guard -> Phantoms.TTerm Syntax.NamedExpression
unGuard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Guard")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ImaginaryNumber
unImaginaryNumber :: Phantoms.TTerm Syntax.ImaginaryNumber -> Phantoms.TTerm Double
unImaginaryNumber x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ImaginaryNumber")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ImportName
unImportName :: Phantoms.TTerm Syntax.ImportName -> Phantoms.TTerm [Syntax.DottedAsName]
unImportName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ImportName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Interactive
unInteractive :: Phantoms.TTerm Syntax.Interactive -> Phantoms.TTerm Syntax.Statement
unInteractive x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Interactive")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ItemsPattern
unItemsPattern :: Phantoms.TTerm Syntax.ItemsPattern -> Phantoms.TTerm [Syntax.KeyValuePattern]
unItemsPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ItemsPattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.KeywordPatterns
unKeywordPatterns :: Phantoms.TTerm Syntax.KeywordPatterns -> Phantoms.TTerm [Syntax.KeywordPattern]
unKeywordPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.KeywordPatterns")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Keywords
unKeywords :: Phantoms.TTerm Syntax.Keywords -> Phantoms.TTerm Syntax.ParamNoDefault
unKeywords x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Keywords")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.LambdaKwds
unLambdaKwds :: Phantoms.TTerm Syntax.LambdaKwds -> Phantoms.TTerm Syntax.LambdaParamNoDefault
unLambdaKwds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.LambdaKwds")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.LambdaParamNoDefault
unLambdaParamNoDefault :: Phantoms.TTerm Syntax.LambdaParamNoDefault -> Phantoms.TTerm Syntax.Name
unLambdaParamNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.LambdaParamNoDefault")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.List
unList :: Phantoms.TTerm Syntax.List -> Phantoms.TTerm [Syntax.StarNamedExpression]
unList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.List")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.MaybeSequencePattern
unMaybeSequencePattern :: Phantoms.TTerm Syntax.MaybeSequencePattern -> Phantoms.TTerm [Syntax.MaybeStarPattern]
unMaybeSequencePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.MaybeSequencePattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Module
unModule :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [[Syntax.Statement]]
unModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Module")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Name
unName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm String
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Name")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.NameOrAttribute
unNameOrAttribute :: Phantoms.TTerm Syntax.NameOrAttribute -> Phantoms.TTerm [Syntax.Name]
unNameOrAttribute x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.NameOrAttribute")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.OrPattern
unOrPattern :: Phantoms.TTerm Syntax.OrPattern -> Phantoms.TTerm [Syntax.ClosedPattern]
unOrPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.OrPattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.PatternCaptureTarget
unPatternCaptureTarget :: Phantoms.TTerm Syntax.PatternCaptureTarget -> Phantoms.TTerm Syntax.Name
unPatternCaptureTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.PatternCaptureTarget")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.PositionalPatterns
unPositionalPatterns :: Phantoms.TTerm Syntax.PositionalPatterns -> Phantoms.TTerm [Syntax.Pattern]
unPositionalPatterns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.PositionalPatterns")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.RaiseStatement
unRaiseStatement :: Phantoms.TTerm Syntax.RaiseStatement -> Phantoms.TTerm (Maybe Syntax.RaiseExpression)
unRaiseStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.RaiseStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ReturnStatement
unReturnStatement :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm [Syntax.StarExpression]
unReturnStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ReturnStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Set
unSet :: Phantoms.TTerm Syntax.Set -> Phantoms.TTerm [Syntax.StarNamedExpression]
unSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Set")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.SlashNoDefault
unSlashNoDefault :: Phantoms.TTerm Syntax.SlashNoDefault -> Phantoms.TTerm [Syntax.ParamNoDefault]
unSlashNoDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.SlashNoDefault")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarAnnotation
unStarAnnotation :: Phantoms.TTerm Syntax.StarAnnotation -> Phantoms.TTerm Syntax.StarExpression
unStarAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarAnnotation")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarNamedExpressions
unStarNamedExpressions :: Phantoms.TTerm Syntax.StarNamedExpressions -> Phantoms.TTerm [Syntax.StarNamedExpression]
unStarNamedExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarNamedExpressions")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarTargetsListSeq
unStarTargetsListSeq :: Phantoms.TTerm Syntax.StarTargetsListSeq -> Phantoms.TTerm [Syntax.StarTarget]
unStarTargetsListSeq x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarTargetsListSeq")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarTargetsTupleSeq
unStarTargetsTupleSeq :: Phantoms.TTerm Syntax.StarTargetsTupleSeq -> Phantoms.TTerm [Syntax.StarTarget]
unStarTargetsTupleSeq x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarTargetsTupleSeq")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarredExpression
unStarredExpression :: Phantoms.TTerm Syntax.StarredExpression -> Phantoms.TTerm Syntax.Expression
unStarredExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarredExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Tuple
unTuple :: Phantoms.TTerm Syntax.Tuple -> Phantoms.TTerm [Syntax.StarNamedExpression]
unTuple x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Tuple")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.TypeComment
unTypeComment :: Phantoms.TTerm Syntax.TypeComment -> Phantoms.TTerm String
unTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.TypeComment")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ValuePattern
unValuePattern :: Phantoms.TTerm Syntax.ValuePattern -> Phantoms.TTerm Syntax.Attribute
unValuePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ValuePattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.YieldStatement
unYieldStatement :: Phantoms.TTerm Syntax.YieldStatement -> Phantoms.TTerm Syntax.YieldExpression
unYieldStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.YieldStatement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.UntypedAssignment
untypedAssignment :: Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.UntypedAssignment
untypedAssignment targets rhs typeComment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm targets)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)}]}))
-- | DSL accessor for the rhs field of hydra.python.syntax.UntypedAssignment
untypedAssignmentRhs :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs
untypedAssignmentRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.UntypedAssignment
untypedAssignmentTargets :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm [Syntax.StarTarget]
untypedAssignmentTargets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionField = (Core.Name "targets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.UntypedAssignment
untypedAssignmentTypeComment :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm (Maybe Syntax.TypeComment)
untypedAssignmentTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the rhs field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithRhs :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm Syntax.AnnotatedRhs -> Phantoms.TTerm Syntax.UntypedAssignment
untypedAssignmentWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the targets field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithTargets :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm [Syntax.StarTarget] -> Phantoms.TTerm Syntax.UntypedAssignment
untypedAssignmentWithTargets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithTypeComment :: Phantoms.TTerm Syntax.UntypedAssignment -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.UntypedAssignment
untypedAssignmentWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "targets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionField = (Core.Name "targets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.python.syntax.ValuePattern wrapper
valuePattern :: Phantoms.TTerm Syntax.Attribute -> Phantoms.TTerm Syntax.ValuePattern
valuePattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ValuePattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.python.syntax.WhileStatement
whileStatement :: Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.WhileStatement
whileStatement condition body else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the body field of hydra.python.syntax.WhileStatement
whileStatementBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Block
whileStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the condition field of hydra.python.syntax.WhileStatement
whileStatementCondition :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.NamedExpression
whileStatementCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.WhileStatement
whileStatementElse :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Block)
whileStatementElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.WhileStatement
whileStatementWithBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the condition field of hydra.python.syntax.WhileStatement
whileStatementWithCondition :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.NamedExpression -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.python.syntax.WhileStatement
whileStatementWithElse :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm (Maybe Syntax.Block) -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.WithItem
withItem :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm (Maybe Syntax.StarTarget) -> Phantoms.TTerm Syntax.WithItem
withItem expression as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))
-- | DSL accessor for the as field of hydra.python.syntax.WithItem
withItemAs :: Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm (Maybe Syntax.StarTarget)
withItemAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
        Core.projectionField = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.WithItem
withItemExpression :: Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm Syntax.Expression
withItemExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.WithItem
withItemWithAs :: Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm (Maybe Syntax.StarTarget) -> Phantoms.TTerm Syntax.WithItem
withItemWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.python.syntax.WithItem
withItemWithExpression :: Phantoms.TTerm Syntax.WithItem -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.WithItem
withItemWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
              Core.projectionField = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.python.syntax.WithStatement
withStatement :: Phantoms.TTerm Bool -> Phantoms.TTerm [Syntax.WithItem] -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WithStatement
withStatement async items typeComment body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm typeComment)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the async field of hydra.python.syntax.WithStatement
withStatementAsync :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Bool
withStatementAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionField = (Core.Name "async")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.WithStatement
withStatementBody :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Syntax.Block
withStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the items field of hydra.python.syntax.WithStatement
withStatementItems :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm [Syntax.WithItem]
withStatementItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.WithStatement
withStatementTypeComment :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment)
withStatementTypeComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionField = (Core.Name "typeComment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.WithStatement
withStatementWithAsync :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.WithStatement
withStatementWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.python.syntax.WithStatement
withStatementWithBody :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.WithStatement
withStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the items field of hydra.python.syntax.WithStatement
withStatementWithItems :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm [Syntax.WithItem] -> Phantoms.TTerm Syntax.WithStatement
withStatementWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "typeComment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeComment field of hydra.python.syntax.WithStatement
withStatementWithTypeComment :: Phantoms.TTerm Syntax.WithStatement -> Phantoms.TTerm (Maybe Syntax.TypeComment) -> Phantoms.TTerm Syntax.WithStatement
withStatementWithTypeComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "async")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeComment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the from variant of hydra.python.syntax.YieldExpression
yieldExpressionFrom :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.YieldExpression
yieldExpressionFrom x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.YieldExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.YieldExpression
yieldExpressionSimple :: Phantoms.TTerm [Syntax.StarExpression] -> Phantoms.TTerm Syntax.YieldExpression
yieldExpressionSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.YieldExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.YieldStatement wrapper
yieldStatement :: Phantoms.TTerm Syntax.YieldExpression -> Phantoms.TTerm Syntax.YieldStatement
yieldStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.YieldStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
