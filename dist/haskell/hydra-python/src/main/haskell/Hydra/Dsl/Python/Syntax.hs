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
annotatedRhsStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotatedRhsStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.python.syntax.AnnotatedRhs
annotatedRhsYield :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotatedRhsYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.AnnotatedStatement
annotatedStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
annotatedStatementComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotatedStatementComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.python.syntax.AnnotatedStatement
annotatedStatementStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotatedStatementStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AnnotatedStatement"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comment field of hydra.python.syntax.AnnotatedStatement
annotatedStatementWithComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
annotatedStatementWithStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
annotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
annotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Annotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Args
args :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
argsKwargOrDoubleStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
argsKwargOrDoubleStarred x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionFieldName = (Core.Name "kwargOrDoubleStarred")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kwargOrStarred field of hydra.python.syntax.Args
argsKwargOrStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
argsKwargOrStarred x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionFieldName = (Core.Name "kwargOrStarred")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the positional field of hydra.python.syntax.Args
argsPositional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
argsPositional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Args"),
        Core.projectionFieldName = (Core.Name "positional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the kwargOrDoubleStarred field of hydra.python.syntax.Args
argsWithKwargOrDoubleStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
argsWithKwargOrStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
argsWithPositional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
asPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
asPatternAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
asPatternAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.python.syntax.AsPattern
asPatternPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
asPatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.AsPattern
asPatternWithAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
asPatternWithPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assertStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assertStatementExpression1 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assertStatementExpression1 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
        Core.projectionFieldName = (Core.Name "expression1")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression2 field of hydra.python.syntax.AssertStatement
assertStatementExpression2 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assertStatementExpression2 x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssertStatement"),
        Core.projectionFieldName = (Core.Name "expression2")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression1 field of hydra.python.syntax.AssertStatement
assertStatementWithExpression1 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assertStatementWithExpression2 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assignmentAug :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentAug x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aug"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.AssignmentExpression
assignmentExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assignmentExpressionExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.AssignmentExpression
assignmentExpressionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentExpressionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.AssignmentExpression
assignmentExpressionWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assignmentExpressionWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
assignmentTyped :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the untyped variant of hydra.python.syntax.Assignment
assignmentUntyped :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
assignmentUntyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Assignment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "untyped"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dict variant of hydra.python.syntax.Atom
atomDict :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomDict x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dict"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dictcomp variant of hydra.python.syntax.Atom
atomDictcomp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomDictcomp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dictcomp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ellipsis variant of hydra.python.syntax.Atom
atomEllipsis :: Typed.TypedTerm t0
atomEllipsis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ellipsis"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the false variant of hydra.python.syntax.Atom
atomFalse :: Typed.TypedTerm t0
atomFalse =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "false"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the genexp variant of hydra.python.syntax.Atom
atomGenexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomGenexp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genexp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the group variant of hydra.python.syntax.Atom
atomGroup :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomGroup x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.python.syntax.Atom
atomList :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the listcomp variant of hydra.python.syntax.Atom
atomListcomp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomListcomp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listcomp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.Atom
atomName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the none variant of hydra.python.syntax.Atom
atomNone :: Typed.TypedTerm t0
atomNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.Atom
atomNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the set variant of hydra.python.syntax.Atom
atomSet :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomSet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the setcomp variant of hydra.python.syntax.Atom
atomSetcomp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomSetcomp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "setcomp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.python.syntax.Atom
atomString :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the true variant of hydra.python.syntax.Atom
atomTrue :: Typed.TypedTerm t0
atomTrue =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "true"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.Atom
atomTuple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
atomTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Attribute wrapper
attribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
attribute x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Attribute"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the ampersandEqual variant of hydra.python.syntax.AugAssign
augAssignAmpersandEqual :: Typed.TypedTerm t0
augAssignAmpersandEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ampersandEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the atEqual variant of hydra.python.syntax.AugAssign
augAssignAtEqual :: Typed.TypedTerm t0
augAssignAtEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the barEqual variant of hydra.python.syntax.AugAssign
augAssignBarEqual :: Typed.TypedTerm t0
augAssignBarEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "barEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the caretEqual variant of hydra.python.syntax.AugAssign
augAssignCaretEqual :: Typed.TypedTerm t0
augAssignCaretEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caretEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the doubleSlashEqual variant of hydra.python.syntax.AugAssign
augAssignDoubleSlashEqual :: Typed.TypedTerm t0
augAssignDoubleSlashEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleSlashEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShiftEqual variant of hydra.python.syntax.AugAssign
augAssignLeftShiftEqual :: Typed.TypedTerm t0
augAssignLeftShiftEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShiftEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minusEqual variant of hydra.python.syntax.AugAssign
augAssignMinusEqual :: Typed.TypedTerm t0
augAssignMinusEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minusEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the percentEqual variant of hydra.python.syntax.AugAssign
augAssignPercentEqual :: Typed.TypedTerm t0
augAssignPercentEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "percentEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plusEqual variant of hydra.python.syntax.AugAssign
augAssignPlusEqual :: Typed.TypedTerm t0
augAssignPlusEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plusEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShiftEqual variant of hydra.python.syntax.AugAssign
augAssignRightShiftEqual :: Typed.TypedTerm t0
augAssignRightShiftEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShiftEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the slashEqual variant of hydra.python.syntax.AugAssign
augAssignSlashEqual :: Typed.TypedTerm t0
augAssignSlashEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the starStarEqual variant of hydra.python.syntax.AugAssign
augAssignStarStarEqual :: Typed.TypedTerm t0
augAssignStarStarEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starStarEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the timesEqual variant of hydra.python.syntax.AugAssign
augAssignTimesEqual :: Typed.TypedTerm t0
augAssignTimesEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.AugAssign"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "timesEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.AugAssignment
augAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
augAssignmentAugassign :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
augAssignmentAugassign x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionFieldName = (Core.Name "augassign")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lhs field of hydra.python.syntax.AugAssignment
augAssignmentLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
augAssignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.AugAssignment
augAssignmentRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
augAssignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AugAssignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the augassign field of hydra.python.syntax.AugAssignment
augAssignmentWithAugassign :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
augAssignmentWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
augAssignmentWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
awaitPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
awaitPrimaryAwait :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
awaitPrimaryAwait x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
        Core.projectionFieldName = (Core.Name "await")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.AwaitPrimary
awaitPrimaryPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
awaitPrimaryPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.AwaitPrimary"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the await field of hydra.python.syntax.AwaitPrimary
awaitPrimaryWithAwait :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
awaitPrimaryWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseAnd :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseAndLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
bitwiseAndLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
bitwiseAndRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseAnd"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseAnd
bitwiseAndWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseAndWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseOr :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseOrLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
bitwiseOrLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseOr
bitwiseOrRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
bitwiseOrRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseOr"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseOr
bitwiseOrWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseOrWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseXor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseXorLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
bitwiseXorLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.BitwiseXor
bitwiseXorRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
bitwiseXorRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.BitwiseXor"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.BitwiseXor
bitwiseXorWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
bitwiseXorWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
blockIndented :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
blockIndented x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Block"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indented"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Block
blockSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
blockSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Block"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.CapturePattern wrapper
capturePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
capturePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.CapturePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.CaseBlock
caseBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
caseBlockBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
caseBlockBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the guard field of hydra.python.syntax.CaseBlock
caseBlockGuard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
caseBlockGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionFieldName = (Core.Name "guard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the patterns field of hydra.python.syntax.CaseBlock
caseBlockPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
caseBlockPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CaseBlock"),
        Core.projectionFieldName = (Core.Name "patterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.CaseBlock
caseBlockWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
caseBlockWithGuard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
caseBlockWithPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classDefinition :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5
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
classDefinitionArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDefinitionArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ClassDefinition
classDefinitionBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDefinitionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the decorators field of hydra.python.syntax.ClassDefinition
classDefinitionDecorators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDefinitionDecorators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "decorators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ClassDefinition
classDefinitionName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.ClassDefinition
classDefinitionTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classDefinitionTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassDefinition"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.python.syntax.ClassDefinition
classDefinitionWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classDefinitionWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classDefinitionWithDecorators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classDefinitionWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classDefinitionWithTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
classPatternKeywordPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classPatternKeywordPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionFieldName = (Core.Name "keywordPatterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the nameOrAttribute field of hydra.python.syntax.ClassPattern
classPatternNameOrAttribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classPatternNameOrAttribute x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionFieldName = (Core.Name "nameOrAttribute")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the positionalPatterns field of hydra.python.syntax.ClassPattern
classPatternPositionalPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
classPatternPositionalPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ClassPattern"),
        Core.projectionFieldName = (Core.Name "positionalPatterns")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywordPatterns field of hydra.python.syntax.ClassPattern
classPatternWithKeywordPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classPatternWithNameOrAttribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
classPatternWithPositionalPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
closedPatternCapture :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternCapture x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "capture"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the class variant of hydra.python.syntax.ClosedPattern
closedPatternClass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the group variant of hydra.python.syntax.ClosedPattern
closedPatternGroup :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternGroup x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.python.syntax.ClosedPattern
closedPatternLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mapping variant of hydra.python.syntax.ClosedPattern
closedPatternMapping :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternMapping x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mapping"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequence variant of hydra.python.syntax.ClosedPattern
closedPatternSequence :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternSequence x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the value variant of hydra.python.syntax.ClosedPattern
closedPatternValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
closedPatternValue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.python.syntax.ClosedPattern
closedPatternWildcard :: Typed.TypedTerm t0
closedPatternWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ClosedPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.CommaStarEtc
commaStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
commaStarEtcKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
commaStarEtcKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
        Core.projectionFieldName = (Core.Name "keywords")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.CommaStarEtc
commaStarEtcParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
commaStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CommaStarEtc"),
        Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.CommaStarEtc
commaStarEtcWithKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
commaStarEtcWithParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
compareOpBitwiseOrPair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
compareOpBitwiseOrPairOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compareOpBitwiseOrPairOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compareOpBitwiseOrPairRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.CompareOpBitwiseOrPair"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operator field of hydra.python.syntax.CompareOpBitwiseOrPair
compareOpBitwiseOrPairWithOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
compareOpBitwiseOrPairWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
compareOpEq :: Typed.TypedTerm t0
compareOpEq =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the gt variant of hydra.python.syntax.CompareOp
compareOpGt :: Typed.TypedTerm t0
compareOpGt =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the gte variant of hydra.python.syntax.CompareOp
compareOpGte :: Typed.TypedTerm t0
compareOpGte =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.python.syntax.CompareOp
compareOpIn :: Typed.TypedTerm t0
compareOpIn =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the is variant of hydra.python.syntax.CompareOp
compareOpIs :: Typed.TypedTerm t0
compareOpIs =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "is"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the isnot variant of hydra.python.syntax.CompareOp
compareOpIsnot :: Typed.TypedTerm t0
compareOpIsnot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "isnot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lt variant of hydra.python.syntax.CompareOp
compareOpLt :: Typed.TypedTerm t0
compareOpLt =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lte variant of hydra.python.syntax.CompareOp
compareOpLte :: Typed.TypedTerm t0
compareOpLte =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the noteq variant of hydra.python.syntax.CompareOp
compareOpNoteq :: Typed.TypedTerm t0
compareOpNoteq =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "noteq"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notin variant of hydra.python.syntax.CompareOp
compareOpNotin :: Typed.TypedTerm t0
compareOpNotin =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompareOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notin"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.Comparison
comparison :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
comparisonLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
comparisonLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Comparison
comparisonRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
comparisonRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Comparison"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Comparison
comparisonWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
comparisonWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
complexNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
complexNumberImaginary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
complexNumberImaginary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionFieldName = (Core.Name "imaginary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the plusOrMinus field of hydra.python.syntax.ComplexNumber
complexNumberPlusOrMinus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
complexNumberPlusOrMinus x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionFieldName = (Core.Name "plusOrMinus")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the real field of hydra.python.syntax.ComplexNumber
complexNumberReal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
complexNumberReal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ComplexNumber"),
        Core.projectionFieldName = (Core.Name "real")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imaginary field of hydra.python.syntax.ComplexNumber
complexNumberWithImaginary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
complexNumberWithPlusOrMinus :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
complexNumberWithReal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
compoundStatementClassDef :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementClassDef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classDef"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.python.syntax.CompoundStatement
compoundStatementFor :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.python.syntax.CompoundStatement
compoundStatementFunction :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.python.syntax.CompoundStatement
compoundStatementIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the match variant of hydra.python.syntax.CompoundStatement
compoundStatementMatch :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementMatch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the try variant of hydra.python.syntax.CompoundStatement
compoundStatementTry :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementTry x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.python.syntax.CompoundStatement
compoundStatementWhile :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the with variant of hydra.python.syntax.CompoundStatement
compoundStatementWith :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
compoundStatementWith x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.CompoundStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "with"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Conditional
conditional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
conditionalBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.Conditional
conditionalElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the if field of hydra.python.syntax.Conditional
conditionalIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conditionalIf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Conditional"),
        Core.projectionFieldName = (Core.Name "if")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.Conditional
conditionalWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
conditionalWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
conditionalWithIf :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
conjunction :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
conjunction x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Conjunction"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Decorators wrapper
decorators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
decorators x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Decorators"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Default wrapper
default_ :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
default_ x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Default"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.DelStatement wrapper
delStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DelStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the name variant of hydra.python.syntax.DelTAtom
delTAtomName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTAtomName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the target variant of hydra.python.syntax.DelTAtom
delTAtomTarget :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTAtomTarget x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the targets variant of hydra.python.syntax.DelTAtom
delTAtomTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTAtomTargets x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targets"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the delTAtom variant of hydra.python.syntax.DelTarget
delTargetDelTAtom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTargetDelTAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delTAtom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.DelTarget
delTargetPrimaryAndName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTargetPrimaryAndName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.DelTarget
delTargetPrimaryAndSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTargetPrimaryAndSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DelTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.DelTargets wrapper
delTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
delTargets x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DelTargets"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Dict wrapper
dict :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dict x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Dict"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Dictcomp
dictcomp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
dictcompForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dictcompForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
        Core.projectionFieldName = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kvpair field of hydra.python.syntax.Dictcomp
dictcompKvpair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dictcompKvpair x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Dictcomp"),
        Core.projectionFieldName = (Core.Name "kvpair")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the forIfClauses field of hydra.python.syntax.Dictcomp
dictcompWithForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
dictcompWithKvpair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
disjunction :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
disjunction x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Disjunction"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.DottedAsName
dottedAsName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
dottedAsNameAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dottedAsNameAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.DottedAsName
dottedAsNameName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dottedAsNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DottedAsName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.DottedAsName
dottedAsNameWithAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
dottedAsNameWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
dottedName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
dottedName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DottedName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.DoubleStarPattern wrapper
doubleStarPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doubleStarPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.DoubleStarPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
doubleStarTypeParameterDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doubleStarTypeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doubleStarTypeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarTypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.DoubleStarTypeParameter
doubleStarTypeParameterWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
doubleStarTypeParameterWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
doubleStarredKvpairPair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doubleStarredKvpairPair x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarredKvpair"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pair"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.DoubleStarredKvpair
doubleStarredKvpairStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
doubleStarredKvpairStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.DoubleStarredKvpair"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Eval wrapper
eval :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
eval x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Eval"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ExceptBlock
exceptBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptBlockBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptBlockBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptBlock
exceptBlockExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptBlockExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptBlock"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.ExceptBlock
exceptBlockWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptBlockWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptExpressionAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptExpressionAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptExpression
exceptExpressionExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ExceptExpression
exceptExpressionWithAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptExpressionWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptStarBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
exceptStarBlockAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptStarBlockAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptStarBlockBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
exceptStarBlockExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ExceptStarBlock"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ExceptStarBlock
exceptStarBlockWithAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptStarBlockWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
exceptStarBlockWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
expressionConditional :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionConditional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.python.syntax.Expression
expressionLambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Expression
expressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
expressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the complement variant of hydra.python.syntax.Factor
factorComplement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
factorComplement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the negative variant of hydra.python.syntax.Factor
factorNegative :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
factorNegative x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negative"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the positive variant of hydra.python.syntax.Factor
factorPositive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
factorPositive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "positive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Factor
factorSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
factorSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Factor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.File wrapper
file :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
file x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.File"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ForIfClause
forIfClause :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
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
forIfClauseAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forIfClauseAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the ifs field of hydra.python.syntax.ForIfClause
forIfClauseIfs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forIfClauseIfs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "ifs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.python.syntax.ForIfClause
forIfClauseIn :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forIfClauseIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ForIfClause
forIfClauseTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forIfClauseTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForIfClause"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.ForIfClause
forIfClauseWithAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forIfClauseWithIfs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forIfClauseWithIn :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forIfClauseWithTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forIfClauses x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ForIfClauses"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ForStatement
forStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6
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
forStatementAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.ForStatement
forStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.ForStatement
forStatementElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expressions field of hydra.python.syntax.ForStatement
forStatementExpressions :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ForStatement
forStatementTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ForStatement
forStatementTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
forStatementTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.ForStatement
forStatementWithAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forStatementWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forStatementWithExpressions :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forStatementWithTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
forStatementWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
funcTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
funcTypeComment x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.FuncTypeComment"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.FunctionDefRaw
functionDefRaw :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6 -> Typed.TypedTerm t7
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
functionDefRawAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the block field of hydra.python.syntax.FunctionDefRaw
functionDefRawBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the funcTypeComment field of hydra.python.syntax.FunctionDefRaw
functionDefRawFuncTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawFuncTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "funcTypeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.FunctionDefRaw
functionDefRawName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.python.syntax.FunctionDefRaw
functionDefRawParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the returnType field of hydra.python.syntax.FunctionDefRaw
functionDefRawReturnType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawReturnType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "returnType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.FunctionDefRaw
functionDefRawTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefRawTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefRaw"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.FunctionDefRaw
functionDefRawWithAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefRawWithBlock :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefRawWithFuncTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefRawWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefRawWithParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefRawWithReturnType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefRawWithTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefinition :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefinitionDecorators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefinitionDecorators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "decorators")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the raw field of hydra.python.syntax.FunctionDefinition
functionDefinitionRaw :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
functionDefinitionRaw x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.FunctionDefinition"),
        Core.projectionFieldName = (Core.Name "raw")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decorators field of hydra.python.syntax.FunctionDefinition
functionDefinitionWithDecorators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
functionDefinitionWithRaw :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
genexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
genexpHead :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
genexpHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.python.syntax.GenexpHead
genexpHeadAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
genexpHeadAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.GenexpHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.GenexpHead
genexpHeadExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
genexpHeadExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.GenexpHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the tail field of hydra.python.syntax.Genexp
genexpTail :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
genexpTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Genexp"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.Genexp
genexpWithHead :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
genexpWithTail :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
groupExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
groupExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Group"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.GroupPattern wrapper
groupPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
groupPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.GroupPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the yield variant of hydra.python.syntax.Group
groupYield :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
groupYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Group"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Guard wrapper
guard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
guard x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Guard"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.IfStatement
ifStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
ifStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the condition field of hydra.python.syntax.IfStatement
ifStatementCondition :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifStatementCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the continuation field of hydra.python.syntax.IfStatement
ifStatementContinuation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifStatementContinuation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "continuation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.IfStatement
ifStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
ifStatementWithCondition :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
ifStatementWithContinuation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
ifTailElif :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifTailElif x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.IfTail"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "elif"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the else variant of hydra.python.syntax.IfTail
ifTailElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
ifTailElse x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.IfTail"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "else"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.ImaginaryNumber wrapper
imaginaryNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
imaginaryNumber x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ImaginaryNumber"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ImportFrom
importFrom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
importFromAsName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
importFromAsNameAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromAsNameAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ImportFromAsName
importFromAsNameName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromAsNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFromAsName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.ImportFromAsName
importFromAsNameWithAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
importFromAsNameWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
importFromDottedName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromDottedName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionFieldName = (Core.Name "dottedName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the prefixes field of hydra.python.syntax.ImportFrom
importFromPrefixes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromPrefixes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionFieldName = (Core.Name "prefixes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.ImportFrom
importFromTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ImportFrom"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the parens variant of hydra.python.syntax.ImportFromTargets
importFromTargetsParens :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromTargetsParens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.ImportFromTargets
importFromTargetsSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importFromTargetsSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.ImportFromTargets
importFromTargetsStar :: Typed.TypedTerm t0
importFromTargetsStar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportFromTargets"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the dottedName field of hydra.python.syntax.ImportFrom
importFromWithDottedName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
importFromWithPrefixes :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
importFromWithTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
importName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ImportName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the from variant of hydra.python.syntax.ImportStatement
importStatementFrom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importStatementFrom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.ImportStatement
importStatementName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
importStatementName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ImportStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Interactive wrapper
interactive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
interactive x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Interactive"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the not variant of hydra.python.syntax.Inversion
inversionNot :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
inversionNot x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Inversion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Inversion
inversionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
inversionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Inversion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.ItemsPattern wrapper
itemsPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
itemsPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ItemsPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.KeyValuePattern
keyValuePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
keyValuePatternKey :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
keyValuePatternKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.KeyValuePattern
keyValuePatternValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
keyValuePatternValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeyValuePattern"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.python.syntax.KeyValuePattern
keyValuePatternWithKey :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
keyValuePatternWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
keywordPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
keywordPatternName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
keywordPatternName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.python.syntax.KeywordPattern
keywordPatternPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
keywordPatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.KeywordPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.KeywordPattern
keywordPatternWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
keywordPatternWithPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
keywordPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
keywordPatterns x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.KeywordPatterns"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Keywords wrapper
keywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
keywords x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Keywords"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Kvpair
kvpair :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
kvpairKey :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kvpairKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.Kvpair
kvpairValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kvpairValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kvpair"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.python.syntax.Kvpair
kvpairWithKey :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
kvpairWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
kwarg :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
kwargName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kwargName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the doubleStarred variant of hydra.python.syntax.KwargOrDoubleStarred
kwargOrDoubleStarredDoubleStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kwargOrDoubleStarredDoubleStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrDoubleStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStarred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the kwarg variant of hydra.python.syntax.KwargOrDoubleStarred
kwargOrDoubleStarredKwarg :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kwargOrDoubleStarredKwarg x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrDoubleStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwarg"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the kwarg variant of hydra.python.syntax.KwargOrStarred
kwargOrStarredKwarg :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kwargOrStarredKwarg x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwarg"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.KwargOrStarred
kwargOrStarredStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kwargOrStarredStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.KwargOrStarred"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the value field of hydra.python.syntax.Kwarg
kwargValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
kwargValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Kwarg"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.Kwarg
kwargWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
kwargWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.LambdaKwds wrapper
lambdaKwds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaKwds x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.LambdaKwds"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParamMaybeDefaultDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParamMaybeDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParamMaybeDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.LambdaParamMaybeDefault
lambdaParamMaybeDefaultWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParamMaybeDefaultWithParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParamNoDefault x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.LambdaParamNoDefault"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParamWithDefaultDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParamWithDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParamWithDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParamWithDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.LambdaParamWithDefault
lambdaParamWithDefaultWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParamWithDefaultWithParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
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
lambdaParametersParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slashNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersSlashNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParametersSlashNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "slashNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.LambdaParameters
lambdaParametersStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.LambdaParameters
lambdaParametersWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParametersWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParametersWithSlashNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParametersWithStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Lambda"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaSlashNoDefault parameters =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)}]}))
-- | DSL accessor for the parameters field of hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaSlashNoDefaultParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the parameters field of hydra.python.syntax.LambdaSlashNoDefault
lambdaSlashNoDefaultWithParameters :: t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
lambdaSlashNoDefaultWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.syntax.LambdaSlashNoDefault"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaSlashWithDefaultParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaSlashWithDefaultParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaSlashWithDefaultParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.LambdaSlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.LambdaSlashWithDefault
lambdaSlashWithDefaultWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaSlashWithDefaultWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaStarEtcKwds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaStarEtcKwds x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "kwds"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the paramMaybeDefault variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramMaybeDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the paramNoDefault variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaStarEtcParamNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.LambdaStarEtc
lambdaStarEtcStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
lambdaStarEtcStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LambdaStarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the body field of hydra.python.syntax.Lambda
lambdaWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
lambdaWithParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
list :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
list x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.List"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Listcomp
listcomp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
listcompExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
listcompExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Listcomp
listcompForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
listcompForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Listcomp"),
        Core.projectionFieldName = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.Listcomp
listcompWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
listcompWithForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
literalExpressionComplex :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalExpressionComplex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the false variant of hydra.python.syntax.LiteralExpression
literalExpressionFalse :: Typed.TypedTerm t0
literalExpressionFalse =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "false"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the none variant of hydra.python.syntax.LiteralExpression
literalExpressionNone :: Typed.TypedTerm t0
literalExpressionNone =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.LiteralExpression
literalExpressionNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalExpressionNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the attribute variant of hydra.python.syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeAttribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalExpressionOrAttributeAttribute x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpressionOrAttribute"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "attribute"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.python.syntax.LiteralExpressionOrAttribute
literalExpressionOrAttributeLiteral :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalExpressionOrAttributeLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpressionOrAttribute"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.python.syntax.LiteralExpression
literalExpressionString :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
literalExpressionString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the true variant of hydra.python.syntax.LiteralExpression
literalExpressionTrue :: Typed.TypedTerm t0
literalExpressionTrue =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.LiteralExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "true"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.MappingPattern
mappingPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
mappingPatternDoubleStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
mappingPatternDoubleStar x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
        Core.projectionFieldName = (Core.Name "doubleStar")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the items field of hydra.python.syntax.MappingPattern
mappingPatternItems :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
mappingPatternItems x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MappingPattern"),
        Core.projectionFieldName = (Core.Name "items")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the doubleStar field of hydra.python.syntax.MappingPattern
mappingPatternWithDoubleStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
mappingPatternWithItems :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
matchStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
matchStatementCases :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
matchStatementCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subject field of hydra.python.syntax.MatchStatement
matchStatementSubject :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
matchStatementSubject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.MatchStatement"),
        Core.projectionFieldName = (Core.Name "subject")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.python.syntax.MatchStatement
matchStatementWithCases :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
matchStatementWithSubject :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
maybeSequencePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
maybeSequencePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.MaybeSequencePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the pattern variant of hydra.python.syntax.MaybeStarPattern
maybeStarPatternPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
maybeStarPatternPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.MaybeStarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.MaybeStarPattern
maybeStarPatternStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
maybeStarPatternStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.MaybeStarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Module wrapper
module_ :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
module_ x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Module"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.Name wrapper
name :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
name x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Name"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.NameOrAttribute wrapper
nameOrAttribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
nameOrAttribute x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.NameOrAttribute"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the assignment variant of hydra.python.syntax.NamedExpression
namedExpressionAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
namedExpressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.NamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.NamedExpression
namedExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
namedExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.NamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
noDefaultStarAnnotationStarEtcKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
noDefaultStarAnnotationStarEtcKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionFieldName = (Core.Name "keywords")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
noDefaultStarAnnotationStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramNoDefaultStarAnnotation field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
noDefaultStarAnnotationStarEtcParamNoDefaultStarAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarAnnotationStarEtc"),
        Core.projectionFieldName = (Core.Name "paramNoDefaultStarAnnotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.NoDefaultStarAnnotationStarEtc
noDefaultStarAnnotationStarEtcWithKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
noDefaultStarAnnotationStarEtcWithParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
noDefaultStarAnnotationStarEtcWithParamNoDefaultStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
noDefaultStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
noDefaultStarEtcKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
noDefaultStarEtcKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionFieldName = (Core.Name "keywords")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramMaybeDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
noDefaultStarEtcParamMaybeDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionFieldName = (Core.Name "paramMaybeDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramNoDefault field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
noDefaultStarEtcParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.NoDefaultStarEtc"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the keywords field of hydra.python.syntax.NoDefaultStarEtc
noDefaultStarEtcWithKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
noDefaultStarEtcWithParamMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
noDefaultStarEtcWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
numberFloat :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numberFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the imaginary variant of hydra.python.syntax.Number
numberImaginary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numberImaginary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "imaginary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.python.syntax.Number
numberInteger :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
numberInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Number"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.OpenSequencePattern
openSequencePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
openSequencePatternHead :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
openSequencePatternHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tail field of hydra.python.syntax.OpenSequencePattern
openSequencePatternTail :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
openSequencePatternTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.OpenSequencePattern"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.OpenSequencePattern
openSequencePatternWithHead :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
openSequencePatternWithTail :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
orPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
orPattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.OrPattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Param
param :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamMaybeDefault
paramMaybeDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
paramMaybeDefaultDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramMaybeDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramMaybeDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramMaybeDefaultTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamMaybeDefault"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.ParamMaybeDefault
paramMaybeDefaultWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramMaybeDefaultWithParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramMaybeDefaultWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Param"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefault
paramNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
paramNoDefaultParametersParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.ParamNoDefaultParameters
paramNoDefaultParametersWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultParametersWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultParametersWithStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultStarAnnotationParamStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultStarAnnotationParamStarAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
        Core.projectionFieldName = (Core.Name "paramStarAnnotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultStarAnnotationTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefaultStarAnnotation"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramStarAnnotation field of hydra.python.syntax.ParamNoDefaultStarAnnotation
paramNoDefaultStarAnnotationWithParamStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultStarAnnotationWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramNoDefaultTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamNoDefault"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the param field of hydra.python.syntax.ParamNoDefault
paramNoDefaultWithParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramNoDefaultWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramStarAnnotationAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramStarAnnotationAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
        Core.projectionFieldName = (Core.Name "annotation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramStarAnnotationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamStarAnnotation"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the annotation field of hydra.python.syntax.ParamStarAnnotation
paramStarAnnotationWithAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramStarAnnotationWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
paramWithDefaultDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramWithDefaultDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.python.syntax.ParamWithDefault
paramWithDefaultParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramWithDefaultParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithDefaultParametersParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramWithDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramWithDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramWithDefault field of hydra.python.syntax.ParamWithDefaultParameters
paramWithDefaultParametersWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithDefaultParametersWithStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithDefaultTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
paramWithDefaultTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ParamWithDefault"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.ParamWithDefault
paramWithDefaultWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithDefaultWithParam :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithDefaultWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
paramWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
parametersParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
parametersParamNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the paramWithDefault variant of hydra.python.syntax.Parameters
parametersParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
parametersParamWithDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paramWithDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slashNoDefault variant of hydra.python.syntax.Parameters
parametersSlashNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
parametersSlashNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slashWithDefault variant of hydra.python.syntax.Parameters
parametersSlashWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
parametersSlashWithDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slashWithDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starEtc variant of hydra.python.syntax.Parameters
parametersStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
parametersStarEtc x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Parameters"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starEtc"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the as variant of hydra.python.syntax.Pattern
patternAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternAs x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.PatternCaptureTarget wrapper
patternCaptureTarget :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternCaptureTarget x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.PatternCaptureTarget"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the or variant of hydra.python.syntax.Pattern
patternOr :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternOr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.python.syntax.Patterns
patternsPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternsPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Patterns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequence variant of hydra.python.syntax.Patterns
patternsSequence :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
patternsSequence x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Patterns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the minus variant of hydra.python.syntax.PlusOrMinus
plusOrMinusMinus :: Typed.TypedTerm t0
plusOrMinusMinus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PlusOrMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.python.syntax.PlusOrMinus
plusOrMinusPlus :: Typed.TypedTerm t0
plusOrMinusPlus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PlusOrMinus"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assignment variant of hydra.python.syntax.PosArg
posArgAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
posArgAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.PosArg
posArgExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
posArgExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.PosArg
posArgStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
posArgStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PosArg"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.PositionalPatterns wrapper
positionalPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
positionalPatterns x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.PositionalPatterns"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Power
power :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
powerLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
powerLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Power
powerRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
powerRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Power"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Power
powerWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
powerWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
primaryCompound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryCompound x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the call variant of hydra.python.syntax.PrimaryRhs
primaryRhsCall :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryRhsCall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the genexp variant of hydra.python.syntax.PrimaryRhs
primaryRhsGenexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryRhsGenexp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "genexp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.python.syntax.PrimaryRhs
primaryRhsProject :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryRhsProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slices variant of hydra.python.syntax.PrimaryRhs
primaryRhsSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryRhsSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.PrimaryRhs"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Primary
primarySimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primarySimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Primary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.PrimaryWithRhs
primaryWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
primaryWithRhsPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryWithRhsPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
primaryWithRhsRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.PrimaryWithRhs"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the primary field of hydra.python.syntax.PrimaryWithRhs
primaryWithRhsWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
primaryWithRhsWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
quoteStyleDouble :: Typed.TypedTerm t0
quoteStyleDouble =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the single variant of hydra.python.syntax.QuoteStyle
quoteStyleSingle :: Typed.TypedTerm t0
quoteStyleSingle =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tripleDouble variant of hydra.python.syntax.QuoteStyle
quoteStyleTripleDouble :: Typed.TypedTerm t0
quoteStyleTripleDouble =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tripleDouble"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the tripleSingle variant of hydra.python.syntax.QuoteStyle
quoteStyleTripleSingle :: Typed.TypedTerm t0
quoteStyleTripleSingle =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.QuoteStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tripleSingle"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.python.syntax.RaiseExpression
raiseExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
raiseExpressionExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
raiseExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the from field of hydra.python.syntax.RaiseExpression
raiseExpressionFrom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
raiseExpressionFrom x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.RaiseExpression"),
        Core.projectionFieldName = (Core.Name "from")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.RaiseExpression
raiseExpressionWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
raiseExpressionWithFrom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
raiseStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
raiseStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.RaiseStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the float variant of hydra.python.syntax.RealNumber
realNumberFloat :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
realNumberFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.python.syntax.RealNumber
realNumberInteger :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
realNumberInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the dot variant of hydra.python.syntax.RelativeImportPrefix
relativeImportPrefixDot :: Typed.TypedTerm t0
relativeImportPrefixDot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RelativeImportPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the ellipsis variant of hydra.python.syntax.RelativeImportPrefix
relativeImportPrefixEllipsis :: Typed.TypedTerm t0
relativeImportPrefixEllipsis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.RelativeImportPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ellipsis"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.python.syntax.ReturnStatement wrapper
returnStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
returnStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ReturnStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the list variant of hydra.python.syntax.SequencePattern
sequencePatternList :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sequencePatternList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SequencePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.SequencePattern
sequencePatternTuple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sequencePatternTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SequencePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Set wrapper
set :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
set x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Set"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.Setcomp
setcomp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
setcompExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
setcompExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the forIfClauses field of hydra.python.syntax.Setcomp
setcompForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
setcompForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Setcomp"),
        Core.projectionFieldName = (Core.Name "forIfClauses")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.Setcomp
setcompWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
setcompWithForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftExpressionLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpressionLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.ShiftExpression
shiftExpressionRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftExpressionRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftExpression"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.ShiftExpression
shiftExpressionWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftExpressionWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftLhsOperand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftLhsOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.ShiftLhs
shiftLhsOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
shiftLhsOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.ShiftLhs"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.ShiftLhs
shiftLhsWithOperand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftLhsWithOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
shiftOpLeft :: Typed.TypedTerm t0
shiftOpLeft =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ShiftOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "left"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the right variant of hydra.python.syntax.ShiftOp
shiftOpRight :: Typed.TypedTerm t0
shiftOpRight =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.ShiftOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "right"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.python.syntax.SignedNumber
signedNumberNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
signedNumberNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sign variant of hydra.python.syntax.SignedNumber
signedNumberSign :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
signedNumberSign x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sign"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the number variant of hydra.python.syntax.SignedRealNumber
signedRealNumberNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
signedRealNumberNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedRealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sign variant of hydra.python.syntax.SignedRealNumber
signedRealNumberSign :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
signedRealNumberSign x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SignedRealNumber"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sign"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assert variant of hydra.python.syntax.SimpleStatement
simpleStatementAssert :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementAssert x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assert"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assignment variant of hydra.python.syntax.SimpleStatement
simpleStatementAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the break variant of hydra.python.syntax.SimpleStatement
simpleStatementBreak :: Typed.TypedTerm t0
simpleStatementBreak =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the continue variant of hydra.python.syntax.SimpleStatement
simpleStatementContinue :: Typed.TypedTerm t0
simpleStatementContinue =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the del variant of hydra.python.syntax.SimpleStatement
simpleStatementDel :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementDel x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "del"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the global variant of hydra.python.syntax.SimpleStatement
simpleStatementGlobal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementGlobal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "global"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the import variant of hydra.python.syntax.SimpleStatement
simpleStatementImport :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the nonlocal variant of hydra.python.syntax.SimpleStatement
simpleStatementNonlocal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementNonlocal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonlocal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pass variant of hydra.python.syntax.SimpleStatement
simpleStatementPass :: Typed.TypedTerm t0
simpleStatementPass =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pass"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the raise variant of hydra.python.syntax.SimpleStatement
simpleStatementRaise :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementRaise x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raise"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.python.syntax.SimpleStatement
simpleStatementReturn :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starExpressions variant of hydra.python.syntax.SimpleStatement
simpleStatementStarExpressions :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementStarExpressions x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starExpressions"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeAlias variant of hydra.python.syntax.SimpleStatement
simpleStatementTypeAlias :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementTypeAlias x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAlias"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.python.syntax.SimpleStatement
simpleStatementYield :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleStatementYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SimpleStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.SimpleTypeParameter
simpleTypeParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
simpleTypeParameterBound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleTypeParameterBound x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionFieldName = (Core.Name "bound")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleTypeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
simpleTypeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SimpleTypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bound field of hydra.python.syntax.SimpleTypeParameter
simpleTypeParameterWithBound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
simpleTypeParameterWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
simpleTypeParameterWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
singleSubscriptAttributeTargetPrimaryAndName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleSubscriptAttributeTargetPrimaryAndName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleSubscriptAttributeTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.SingleSubscriptAttributeTarget
singleSubscriptAttributeTargetPrimaryAndSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleSubscriptAttributeTargetPrimaryAndSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleSubscriptAttributeTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.python.syntax.SingleTarget
singleTargetName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleTargetName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parens variant of hydra.python.syntax.SingleTarget
singleTargetParens :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleTargetParens x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the subscriptAttributeTarget variant of hydra.python.syntax.SingleTarget
singleTargetSubscriptAttributeTarget :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
singleTargetSubscriptAttributeTarget x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SingleTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subscriptAttributeTarget"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.SlashNoDefault wrapper
slashNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashNoDefault x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.SlashNoDefault"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
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
slashNoDefaultParametersParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashNoDefaultParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashNoDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slash field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersSlash :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashNoDefaultParametersSlash x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "slash")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashNoDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashNoDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashNoDefaultParameters
slashNoDefaultParametersWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashNoDefaultParametersWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashNoDefaultParametersWithSlash :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashNoDefaultParametersWithStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashWithDefaultParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashWithDefaultParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashWithDefault
slashWithDefaultParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashWithDefaultParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefault"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParameters :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
slashWithDefaultParametersParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashWithDefaultParametersParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramNoDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramWithDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashWithDefaultParametersParamWithDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "paramWithDefault")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the starEtc field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slashWithDefaultParametersStarEtc x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SlashWithDefaultParameters"),
        Core.projectionFieldName = (Core.Name "starEtc")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the paramNoDefault field of hydra.python.syntax.SlashWithDefaultParameters
slashWithDefaultParametersWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashWithDefaultParametersWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashWithDefaultParametersWithStarEtc :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashWithDefaultWithParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slashWithDefaultWithParamWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sliceExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
sliceExpressionStart :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceExpressionStart x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionFieldName = (Core.Name "start")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the step field of hydra.python.syntax.SliceExpression
sliceExpressionStep :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceExpressionStep x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionFieldName = (Core.Name "step")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the stop field of hydra.python.syntax.SliceExpression
sliceExpressionStop :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceExpressionStop x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SliceExpression"),
        Core.projectionFieldName = (Core.Name "stop")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the start field of hydra.python.syntax.SliceExpression
sliceExpressionWithStart :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sliceExpressionWithStep :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sliceExpressionWithStop :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sliceNamed :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice variant of hydra.python.syntax.SliceOrStarredExpression
sliceOrStarredExpressionSlice :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceOrStarredExpressionSlice x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SliceOrStarredExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starred variant of hydra.python.syntax.SliceOrStarredExpression
sliceOrStarredExpressionStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceOrStarredExpressionStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SliceOrStarredExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice_ variant of hydra.python.syntax.Slice
sliceSlice_ :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sliceSlice_ x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice_"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Slices
slices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slicesHead :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slicesHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tail field of hydra.python.syntax.Slices
slicesTail :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
slicesTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Slices"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the head field of hydra.python.syntax.Slices
slicesWithHead :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
slicesWithTail :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
starAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starAnnotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarAnnotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the name variant of hydra.python.syntax.StarAtom
starAtomName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starAtomName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starTargetsListSeq variant of hydra.python.syntax.StarAtom
starAtomStarTargetsListSeq :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starAtomStarTargetsListSeq x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starTargetsListSeq"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starTargetsTupleSeq variant of hydra.python.syntax.StarAtom
starAtomStarTargetsTupleSeq :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starAtomStarTargetsTupleSeq x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starTargetsTupleSeq"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the targetWithStarAtom variant of hydra.python.syntax.StarAtom
starAtomTargetWithStarAtom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starAtomTargetWithStarAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetWithStarAtom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the keywords variant of hydra.python.syntax.StarEtc
starEtcKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starEtcKeywords x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "keywords"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starComma variant of hydra.python.syntax.StarEtc
starEtcStarComma :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starEtcStarComma x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starComma"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starNoDefault variant of hydra.python.syntax.StarEtc
starEtcStarNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starEtcStarNoDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starNoDefault"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starNoDefaultStarAnnotation variant of hydra.python.syntax.StarEtc
starEtcStarNoDefaultStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starEtcStarNoDefaultStarAnnotation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarEtc"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starNoDefaultStarAnnotation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.StarExpression
starExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.StarExpression
starExpressionStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starExpressionStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.StarNamedExpression
starNamedExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starNamedExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarNamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.StarNamedExpression
starNamedExpressionStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starNamedExpressionStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarNamedExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.StarNamedExpressions wrapper
starNamedExpressions :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starNamedExpressions x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarNamedExpressions"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the capture variant of hydra.python.syntax.StarPattern
starPatternCapture :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starPatternCapture x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "capture"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.python.syntax.StarPattern
starPatternWildcard :: Typed.TypedTerm t0
starPatternWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarPattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the starred variant of hydra.python.syntax.StarTarget
starTargetStarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starTargetStarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unstarred variant of hydra.python.syntax.StarTarget
starTargetUnstarred :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starTargetUnstarred x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StarTarget"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unstarred"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.StarTargetsListSeq wrapper
starTargetsListSeq :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starTargetsListSeq x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarTargetsListSeq"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.python.syntax.StarTargetsTupleSeq wrapper
starTargetsTupleSeq :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starTargetsTupleSeq x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarTargetsTupleSeq"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.StarTypeParameter
starTypeParameter :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
starTypeParameterDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starTypeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.StarTypeParameter
starTypeParameterName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starTypeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.StarTypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the default field of hydra.python.syntax.StarTypeParameter
starTypeParameterWithDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
starTypeParameterWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
starredExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
starredExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.StarredExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the annotated variant of hydra.python.syntax.Statement
statementAnnotated :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementAnnotated x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "annotated"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the compound variant of hydra.python.syntax.Statement
statementCompound :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementCompound x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.Statement
statementSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
statementSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.String
string :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
stringPrefix :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
stringPrefix x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the bytes variant of hydra.python.syntax.StringPrefix
stringPrefixBytes :: Typed.TypedTerm t0
stringPrefixBytes =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bytes"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the raw variant of hydra.python.syntax.StringPrefix
stringPrefixRaw :: Typed.TypedTerm t0
stringPrefixRaw =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raw"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rawBytes variant of hydra.python.syntax.StringPrefix
stringPrefixRawBytes :: Typed.TypedTerm t0
stringPrefixRawBytes =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rawBytes"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unicode variant of hydra.python.syntax.StringPrefix
stringPrefixUnicode :: Typed.TypedTerm t0
stringPrefixUnicode =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.StringPrefix"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unicode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the quoteStyle field of hydra.python.syntax.String
stringQuoteStyle :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
stringQuoteStyle x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionFieldName = (Core.Name "quoteStyle")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.python.syntax.String
stringValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
stringValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.String"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the prefix field of hydra.python.syntax.String
stringWithPrefix :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
stringWithQuoteStyle :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
stringWithValue :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
subjectExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
subjectExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SubjectExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.python.syntax.SubjectExpression
subjectExpressionTuple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
subjectExpressionTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SubjectExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Sum
sum :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sumLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sumLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.SumLhs
sumLhs2 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sumLhsOperand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sumLhsOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.SumLhs
sumLhsOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sumLhsOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.SumLhs"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.SumLhs
sumLhsWithOperand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sumLhsWithOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sumOpAdd :: Typed.TypedTerm t0
sumOpAdd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SumOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the sub variant of hydra.python.syntax.SumOp
sumOpSub :: Typed.TypedTerm t0
sumOpSub =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.SumOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sub"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Sum
sumRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
sumRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Sum"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Sum
sumWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
sumWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndArgumentsArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndArgumentsArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndArgumentsPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndArguments"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.python.syntax.TPrimaryAndArguments
tPrimaryAndArgumentsWithArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndArgumentsWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndGenexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndGenexpGenexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndGenexpGenexp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
        Core.projectionFieldName = (Core.Name "genexp")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndGenexpPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndGenexp"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the genexp field of hydra.python.syntax.TPrimaryAndGenexp
tPrimaryAndGenexpWithGenexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndGenexpWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndNameName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primary field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNamePrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndNamePrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndName"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.python.syntax.TPrimaryAndName
tPrimaryAndNameWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndNameWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndSlicesPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndSlicesPrimary x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
        Core.projectionFieldName = (Core.Name "primary")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slices field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAndSlicesSlices x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TPrimaryAndSlices"),
        Core.projectionFieldName = (Core.Name "slices")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the primary field of hydra.python.syntax.TPrimaryAndSlices
tPrimaryAndSlicesWithPrimary :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAndSlicesWithSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tPrimaryAtom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndArguments variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndArguments :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryPrimaryAndArguments x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndArguments"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndGenexp variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndGenexp :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryPrimaryAndGenexp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndGenexp"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndName variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryPrimaryAndName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndName"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primaryAndSlices variant of hydra.python.syntax.TPrimary
tPrimaryPrimaryAndSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tPrimaryPrimaryAndSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TPrimary"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primaryAndSlices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the atom variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomAtom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
targetWithStarAtomAtom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "atom"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the project variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomProject :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
targetWithStarAtomProject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slices variant of hydra.python.syntax.TargetWithStarAtom
targetWithStarAtomSlices :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
targetWithStarAtomSlices x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TargetWithStarAtom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slices"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.Term
term :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
termLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
termLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.TermLhs
termLhs2 :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
termLhsOperand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
termLhsOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.python.syntax.TermLhs
termLhsOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
termLhsOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TermLhs"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the operand field of hydra.python.syntax.TermLhs
termLhsWithOperand :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
termLhsWithOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
termOpDiv :: Typed.TypedTerm t0
termOpDiv =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "div"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the floordiv variant of hydra.python.syntax.TermOp
termOpFloordiv :: Typed.TypedTerm t0
termOpFloordiv =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "floordiv"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the matmul variant of hydra.python.syntax.TermOp
termOpMatmul :: Typed.TypedTerm t0
termOpMatmul =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "matmul"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mod variant of hydra.python.syntax.TermOp
termOpMod :: Typed.TypedTerm t0
termOpMod =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mod"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the mul variant of hydra.python.syntax.TermOp
termOpMul :: Typed.TypedTerm t0
termOpMul =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TermOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mul"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the rhs field of hydra.python.syntax.Term
termRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
termRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.Term"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.Term
termWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
termWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStarStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
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
tryExceptStarStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStarStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStarStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the excepts field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementExcepts :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStarStatementExcepts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "excepts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStarStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStarStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryExceptStarStatement
tryExceptStarStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStarStatementWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStarStatementWithExcepts :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStarStatementWithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
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
tryExceptStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.TryExceptStatement
tryExceptStatementElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the excepts field of hydra.python.syntax.TryExceptStatement
tryExceptStatementExcepts :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStatementExcepts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "excepts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryExceptStatement
tryExceptStatementFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryExceptStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryExceptStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryExceptStatement
tryExceptStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStatementWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStatementWithExcepts :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryExceptStatementWithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryFinallyStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryFinallyStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryFinallyStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finally field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryFinallyStatementFinally x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TryFinallyStatement"),
        Core.projectionFieldName = (Core.Name "finally")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.TryFinallyStatement
tryFinallyStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryFinallyStatementWithFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
tryStatementExcept :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatementExcept x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "except"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the exceptStar variant of hydra.python.syntax.TryStatement
tryStatementExceptStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatementExceptStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exceptStar"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the finally variant of hydra.python.syntax.TryStatement
tryStatementFinally :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tryStatementFinally x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TryStatement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finally"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.Tuple wrapper
tuple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
tuple x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.Tuple"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.TypeAlias
typeAlias :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
typeAliasExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeAliasExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.python.syntax.TypeAlias
typeAliasName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeAliasName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.python.syntax.TypeAlias
typeAliasTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeAliasTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypeAlias"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.python.syntax.TypeAlias
typeAliasWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
typeAliasWithName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
typeAliasWithTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
typeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeComment x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.TypeComment"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the doubleStarredExpression variant of hydra.python.syntax.TypeExpression
typeExpressionDoubleStarredExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeExpressionDoubleStarredExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStarredExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.python.syntax.TypeExpression
typeExpressionExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeExpressionExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the starredExpression variant of hydra.python.syntax.TypeExpression
typeExpressionStarredExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeExpressionStarredExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "starredExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the doubleStar variant of hydra.python.syntax.TypeParameter
typeParameterDoubleStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterDoubleStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleStar"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.TypeParameter
typeParameterSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the star variant of hydra.python.syntax.TypeParameter
typeParameterStar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typeParameterStar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.TypeParameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.python.syntax.TypedAssignment
typedAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
typedAssignmentLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typedAssignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.python.syntax.TypedAssignment
typedAssignmentRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typedAssignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.python.syntax.TypedAssignment
typedAssignmentType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
typedAssignmentType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.TypedAssignment"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.python.syntax.TypedAssignment
typedAssignmentWithLhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
typedAssignmentWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
typedAssignmentWithType :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
unAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Annotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Attribute
unAttribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unAttribute x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Attribute")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.CapturePattern
unCapturePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unCapturePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.CapturePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Conjunction
unConjunction :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unConjunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Conjunction")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Decorators
unDecorators :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDecorators x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Decorators")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Default
unDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Default")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DelStatement
unDelStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDelStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DelStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DelTargets
unDelTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDelTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DelTargets")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Dict
unDict :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDict x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Dict")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Disjunction
unDisjunction :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDisjunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Disjunction")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DottedName
unDottedName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDottedName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DottedName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.DoubleStarPattern
unDoubleStarPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unDoubleStarPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.DoubleStarPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Eval
unEval :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unEval x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Eval")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.File
unFile :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unFile x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.File")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ForIfClauses
unForIfClauses :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unForIfClauses x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ForIfClauses")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.FuncTypeComment
unFuncTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unFuncTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.FuncTypeComment")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.GroupPattern
unGroupPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unGroupPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.GroupPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Guard
unGuard :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Guard")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ImaginaryNumber
unImaginaryNumber :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unImaginaryNumber x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ImaginaryNumber")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ImportName
unImportName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unImportName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ImportName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Interactive
unInteractive :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unInteractive x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Interactive")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ItemsPattern
unItemsPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unItemsPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ItemsPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.KeywordPatterns
unKeywordPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unKeywordPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.KeywordPatterns")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Keywords
unKeywords :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unKeywords x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Keywords")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.LambdaKwds
unLambdaKwds :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unLambdaKwds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.LambdaKwds")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.LambdaParamNoDefault
unLambdaParamNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unLambdaParamNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.LambdaParamNoDefault")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.List
unList :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unList x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.List")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.MaybeSequencePattern
unMaybeSequencePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unMaybeSequencePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.MaybeSequencePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Module
unModule :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Module")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Name
unName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Name")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.NameOrAttribute
unNameOrAttribute :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unNameOrAttribute x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.NameOrAttribute")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.OrPattern
unOrPattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unOrPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.OrPattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.PatternCaptureTarget
unPatternCaptureTarget :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPatternCaptureTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.PatternCaptureTarget")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.PositionalPatterns
unPositionalPatterns :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unPositionalPatterns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.PositionalPatterns")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.RaiseStatement
unRaiseStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unRaiseStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.RaiseStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ReturnStatement
unReturnStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unReturnStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ReturnStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Set
unSet :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unSet x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Set")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.SlashNoDefault
unSlashNoDefault :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unSlashNoDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.SlashNoDefault")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarAnnotation
unStarAnnotation :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStarAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarAnnotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarNamedExpressions
unStarNamedExpressions :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStarNamedExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarNamedExpressions")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarTargetsListSeq
unStarTargetsListSeq :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStarTargetsListSeq x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarTargetsListSeq")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarTargetsTupleSeq
unStarTargetsTupleSeq :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStarTargetsTupleSeq x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarTargetsTupleSeq")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.StarredExpression
unStarredExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unStarredExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.StarredExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.Tuple
unTuple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTuple x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.Tuple")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.TypeComment
unTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.TypeComment")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.ValuePattern
unValuePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unValuePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.ValuePattern")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.python.syntax.YieldStatement
unYieldStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
unYieldStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.python.syntax.YieldStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.UntypedAssignment
untypedAssignment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
untypedAssignmentRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
untypedAssignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the targets field of hydra.python.syntax.UntypedAssignment
untypedAssignmentTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
untypedAssignmentTargets x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionFieldName = (Core.Name "targets")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.UntypedAssignment
untypedAssignmentTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
untypedAssignmentTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.UntypedAssignment"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the rhs field of hydra.python.syntax.UntypedAssignment
untypedAssignmentWithRhs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
untypedAssignmentWithTargets :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
untypedAssignmentWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
valuePattern :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
valuePattern x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.ValuePattern"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.python.syntax.WhileStatement
whileStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3
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
whileStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the condition field of hydra.python.syntax.WhileStatement
whileStatementCondition :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.python.syntax.WhileStatement
whileStatementElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
whileStatementElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.python.syntax.WhileStatement
whileStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
whileStatementWithCondition :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
whileStatementWithElse :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withItem :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withItemAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
withItemAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.python.syntax.WithItem
withItemExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
withItemExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithItem"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the as field of hydra.python.syntax.WithItem
withItemWithAs :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withItemWithExpression :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4
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
withStatementAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
withStatementAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.python.syntax.WithStatement
withStatementBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
withStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the items field of hydra.python.syntax.WithStatement
withStatementItems :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
withStatementItems x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "items")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeComment field of hydra.python.syntax.WithStatement
withStatementTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
withStatementTypeComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.syntax.WithStatement"),
        Core.projectionFieldName = (Core.Name "typeComment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.python.syntax.WithStatement
withStatementWithAsync :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withStatementWithBody :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withStatementWithItems :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
withStatementWithTypeComment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
yieldExpressionFrom :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
yieldExpressionFrom x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.YieldExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "from"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.python.syntax.YieldExpression
yieldExpressionSimple :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
yieldExpressionSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.syntax.YieldExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.python.syntax.YieldStatement wrapper
yieldStatement :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
yieldStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.python.syntax.YieldStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
