-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.go.syntax

module Hydra.Dsl.Go.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Go.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the add variant of hydra.go.syntax.AddOp
addOpAdd :: Typed.TypedTerm Syntax.AddOp
addOpAdd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseOr variant of hydra.go.syntax.AddOp
addOpBitwiseOr :: Typed.TypedTerm Syntax.AddOp
addOpBitwiseOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseXor variant of hydra.go.syntax.AddOp
addOpBitwiseXor :: Typed.TypedTerm Syntax.AddOp
addOpBitwiseXor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the subtract variant of hydra.go.syntax.AddOp
addOpSubtract :: Typed.TypedTerm Syntax.AddOp
addOpSubtract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.AliasDecl
aliasDecl :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AliasDecl
aliasDecl name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.AliasDecl
aliasDeclName :: Typed.TypedTerm Syntax.AliasDecl -> Typed.TypedTerm Syntax.Identifier
aliasDeclName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.AliasDecl
aliasDeclType :: Typed.TypedTerm Syntax.AliasDecl -> Typed.TypedTerm Syntax.Type
aliasDeclType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.AliasDecl
aliasDeclWithName :: Typed.TypedTerm Syntax.AliasDecl -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.AliasDecl
aliasDeclWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.AliasDecl
aliasDeclWithType :: Typed.TypedTerm Syntax.AliasDecl -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.AliasDecl
aliasDeclWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.AnnotatedDeclaration
annotatedDeclaration :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.TopLevelDecl -> Typed.TypedTerm Syntax.AnnotatedDeclaration
annotatedDeclaration comment declaration =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Typed.unTypedTerm declaration)}]}))
-- | DSL accessor for the comment field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationComment :: Typed.TypedTerm Syntax.AnnotatedDeclaration -> Typed.TypedTerm String
annotatedDeclarationComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the declaration field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationDeclaration :: Typed.TypedTerm Syntax.AnnotatedDeclaration -> Typed.TypedTerm Syntax.TopLevelDecl
annotatedDeclarationDeclaration x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
        Core.projectionFieldName = (Core.Name "declaration")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comment field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationWithComment :: Typed.TypedTerm Syntax.AnnotatedDeclaration -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.AnnotatedDeclaration
annotatedDeclarationWithComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
              Core.projectionFieldName = (Core.Name "declaration")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the declaration field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationWithDeclaration :: Typed.TypedTerm Syntax.AnnotatedDeclaration -> Typed.TypedTerm Syntax.TopLevelDecl -> Typed.TypedTerm Syntax.AnnotatedDeclaration
annotatedDeclarationWithDeclaration original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.Arguments
arguments :: Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Arguments
arguments typeArg expressions ellipsis =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Typed.unTypedTerm typeArg)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Typed.unTypedTerm ellipsis)}]}))
-- | DSL accessor for the ellipsis field of hydra.go.syntax.Arguments
argumentsEllipsis :: Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm Bool
argumentsEllipsis x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
        Core.projectionFieldName = (Core.Name "ellipsis")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expressions field of hydra.go.syntax.Arguments
argumentsExpressions :: Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm [Syntax.Expression]
argumentsExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArg field of hydra.go.syntax.Arguments
argumentsTypeArg :: Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm (Maybe Syntax.Type)
argumentsTypeArg x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
        Core.projectionFieldName = (Core.Name "typeArg")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ellipsis field of hydra.go.syntax.Arguments
argumentsWithEllipsis :: Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Arguments
argumentsWithEllipsis original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "typeArg")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expressions field of hydra.go.syntax.Arguments
argumentsWithExpressions :: Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Arguments
argumentsWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "typeArg")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "ellipsis")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArg field of hydra.go.syntax.Arguments
argumentsWithTypeArg :: Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.Arguments
argumentsWithTypeArg original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "ellipsis")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.ArrayType
arrayType :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ArrayType
arrayType length element =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Typed.unTypedTerm length)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Typed.unTypedTerm element)}]}))
-- | DSL accessor for the element field of hydra.go.syntax.ArrayType
arrayTypeElement :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.Type
arrayTypeElement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the length field of hydra.go.syntax.ArrayType
arrayTypeLength :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.Expression
arrayTypeLength x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "length")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the element field of hydra.go.syntax.ArrayType
arrayTypeWithElement :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ArrayType
arrayTypeWithElement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
              Core.projectionFieldName = (Core.Name "length")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the length field of hydra.go.syntax.ArrayType
arrayTypeWithLength :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ArrayType
arrayTypeWithLength original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the add variant of hydra.go.syntax.AssignOp
assignOpAdd :: Typed.TypedTerm Syntax.AddOp -> Typed.TypedTerm Syntax.AssignOp
assignOpAdd x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the mul variant of hydra.go.syntax.AssignOp
assignOpMul :: Typed.TypedTerm Syntax.MulOp -> Typed.TypedTerm Syntax.AssignOp
assignOpMul x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mul"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.go.syntax.AssignOp
assignOpSimple :: Typed.TypedTerm Syntax.AssignOp
assignOpSimple =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.Assignment
assignment :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.AssignOp -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Assignment
assignment lhs op rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.go.syntax.Assignment
assignmentLhs :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm [Syntax.Expression]
assignmentLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.go.syntax.Assignment
assignmentOp :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.AssignOp
assignmentOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.go.syntax.Assignment
assignmentRhs :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm [Syntax.Expression]
assignmentRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.go.syntax.Assignment
assignmentWithLhs :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Assignment
assignmentWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.go.syntax.Assignment
assignmentWithOp :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.AssignOp -> Typed.TypedTerm Syntax.Assignment
assignmentWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.go.syntax.Assignment
assignmentWithRhs :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Assignment
assignmentWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the float variant of hydra.go.syntax.BasicLit
basicLitFloat :: Typed.TypedTerm Syntax.FloatLit -> Typed.TypedTerm Syntax.BasicLit
basicLitFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the imaginary variant of hydra.go.syntax.BasicLit
basicLitImaginary :: Typed.TypedTerm Syntax.ImaginaryLit -> Typed.TypedTerm Syntax.BasicLit
basicLitImaginary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "imaginary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int variant of hydra.go.syntax.BasicLit
basicLitInt :: Typed.TypedTerm Syntax.IntLit -> Typed.TypedTerm Syntax.BasicLit
basicLitInt x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rune variant of hydra.go.syntax.BasicLit
basicLitRune :: Typed.TypedTerm Syntax.RuneLit -> Typed.TypedTerm Syntax.BasicLit
basicLitRune x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rune"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.go.syntax.BasicLit
basicLitString :: Typed.TypedTerm Syntax.StringLit -> Typed.TypedTerm Syntax.BasicLit
basicLitString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.BinaryExpr
binaryExpr :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryOp -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryExpr
binaryExpr left op right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.go.syntax.BinaryExpr
binaryExprLeft :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.Expression
binaryExprLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the op field of hydra.go.syntax.BinaryExpr
binaryExprOp :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.BinaryOp
binaryExprOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.go.syntax.BinaryExpr
binaryExprRight :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.Expression
binaryExprRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.go.syntax.BinaryExpr
binaryExprWithLeft :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryExpr
binaryExprWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the op field of hydra.go.syntax.BinaryExpr
binaryExprWithOp :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.BinaryOp -> Typed.TypedTerm Syntax.BinaryExpr
binaryExprWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.go.syntax.BinaryExpr
binaryExprWithRight :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryExpr
binaryExprWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the add variant of hydra.go.syntax.BinaryOp
binaryOpAdd :: Typed.TypedTerm Syntax.BinaryOp
binaryOpAdd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the and variant of hydra.go.syntax.BinaryOp
binaryOpAnd :: Typed.TypedTerm Syntax.BinaryOp
binaryOpAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitClear variant of hydra.go.syntax.BinaryOp
binaryOpBitClear :: Typed.TypedTerm Syntax.BinaryOp
binaryOpBitClear =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitClear"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseAnd variant of hydra.go.syntax.BinaryOp
binaryOpBitwiseAnd :: Typed.TypedTerm Syntax.BinaryOp
binaryOpBitwiseAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseOr variant of hydra.go.syntax.BinaryOp
binaryOpBitwiseOr :: Typed.TypedTerm Syntax.BinaryOp
binaryOpBitwiseOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseXor variant of hydra.go.syntax.BinaryOp
binaryOpBitwiseXor :: Typed.TypedTerm Syntax.BinaryOp
binaryOpBitwiseXor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the divide variant of hydra.go.syntax.BinaryOp
binaryOpDivide :: Typed.TypedTerm Syntax.BinaryOp
binaryOpDivide =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the equal variant of hydra.go.syntax.BinaryOp
binaryOpEqual :: Typed.TypedTerm Syntax.BinaryOp
binaryOpEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greater variant of hydra.go.syntax.BinaryOp
binaryOpGreater :: Typed.TypedTerm Syntax.BinaryOp
binaryOpGreater =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterEqual variant of hydra.go.syntax.BinaryOp
binaryOpGreaterEqual :: Typed.TypedTerm Syntax.BinaryOp
binaryOpGreaterEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShift variant of hydra.go.syntax.BinaryOp
binaryOpLeftShift :: Typed.TypedTerm Syntax.BinaryOp
binaryOpLeftShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the less variant of hydra.go.syntax.BinaryOp
binaryOpLess :: Typed.TypedTerm Syntax.BinaryOp
binaryOpLess =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessEqual variant of hydra.go.syntax.BinaryOp
binaryOpLessEqual :: Typed.TypedTerm Syntax.BinaryOp
binaryOpLessEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the multiply variant of hydra.go.syntax.BinaryOp
binaryOpMultiply :: Typed.TypedTerm Syntax.BinaryOp
binaryOpMultiply =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.go.syntax.BinaryOp
binaryOpNotEqual :: Typed.TypedTerm Syntax.BinaryOp
binaryOpNotEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the or variant of hydra.go.syntax.BinaryOp
binaryOpOr :: Typed.TypedTerm Syntax.BinaryOp
binaryOpOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the remainder variant of hydra.go.syntax.BinaryOp
binaryOpRemainder :: Typed.TypedTerm Syntax.BinaryOp
binaryOpRemainder =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remainder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShift variant of hydra.go.syntax.BinaryOp
binaryOpRightShift :: Typed.TypedTerm Syntax.BinaryOp
binaryOpRightShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the subtract variant of hydra.go.syntax.BinaryOp
binaryOpSubtract :: Typed.TypedTerm Syntax.BinaryOp
binaryOpSubtract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.go.syntax.Block wrapper
block :: Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.Block
block x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Block"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.BreakStmt wrapper
breakStmt :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.BreakStmt
breakStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.BreakStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.CallExpr
callExpr :: Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm Syntax.CallExpr
callExpr function arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.go.syntax.CallExpr
callExprArguments :: Typed.TypedTerm Syntax.CallExpr -> Typed.TypedTerm Syntax.Arguments
callExprArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the function field of hydra.go.syntax.CallExpr
callExprFunction :: Typed.TypedTerm Syntax.CallExpr -> Typed.TypedTerm Syntax.PrimaryExpr
callExprFunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.go.syntax.CallExpr
callExprWithArguments :: Typed.TypedTerm Syntax.CallExpr -> Typed.TypedTerm Syntax.Arguments -> Typed.TypedTerm Syntax.CallExpr
callExprWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the function field of hydra.go.syntax.CallExpr
callExprWithFunction :: Typed.TypedTerm Syntax.CallExpr -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.CallExpr
callExprWithFunction original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the bidirectional variant of hydra.go.syntax.ChannelDirection
channelDirectionBidirectional :: Typed.TypedTerm Syntax.ChannelDirection
channelDirectionBidirectional =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ChannelDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bidirectional"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the receive variant of hydra.go.syntax.ChannelDirection
channelDirectionReceive :: Typed.TypedTerm Syntax.ChannelDirection
channelDirectionReceive =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ChannelDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "receive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the send variant of hydra.go.syntax.ChannelDirection
channelDirectionSend :: Typed.TypedTerm Syntax.ChannelDirection
channelDirectionSend =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ChannelDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "send"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.ChannelType
channelType :: Typed.TypedTerm Syntax.ChannelDirection -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ChannelType
channelType direction element =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Typed.unTypedTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Typed.unTypedTerm element)}]}))
-- | DSL accessor for the direction field of hydra.go.syntax.ChannelType
channelTypeDirection :: Typed.TypedTerm Syntax.ChannelType -> Typed.TypedTerm Syntax.ChannelDirection
channelTypeDirection x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
        Core.projectionFieldName = (Core.Name "direction")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the element field of hydra.go.syntax.ChannelType
channelTypeElement :: Typed.TypedTerm Syntax.ChannelType -> Typed.TypedTerm Syntax.Type
channelTypeElement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the direction field of hydra.go.syntax.ChannelType
channelTypeWithDirection :: Typed.TypedTerm Syntax.ChannelType -> Typed.TypedTerm Syntax.ChannelDirection -> Typed.TypedTerm Syntax.ChannelType
channelTypeWithDirection original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the element field of hydra.go.syntax.ChannelType
channelTypeWithElement :: Typed.TypedTerm Syntax.ChannelType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ChannelType
channelTypeWithElement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the default variant of hydra.go.syntax.CommCase
commCaseDefault :: Typed.TypedTerm Syntax.CommCase
commCaseDefault =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.CommCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the receive variant of hydra.go.syntax.CommCase
commCaseReceive :: Typed.TypedTerm Syntax.ReceiveCase -> Typed.TypedTerm Syntax.CommCase
commCaseReceive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.CommCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "receive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the send variant of hydra.go.syntax.CommCase
commCaseSend :: Typed.TypedTerm Syntax.SendStmt -> Typed.TypedTerm Syntax.CommCase
commCaseSend x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.CommCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "send"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.CommClause
commClause :: Typed.TypedTerm Syntax.CommCase -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.CommClause
commClause case_ statements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CommClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm statements)}]}))
-- | DSL accessor for the case field of hydra.go.syntax.CommClause
commClauseCase :: Typed.TypedTerm Syntax.CommClause -> Typed.TypedTerm Syntax.CommCase
commClauseCase x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.go.syntax.CommClause
commClauseStatements :: Typed.TypedTerm Syntax.CommClause -> Typed.TypedTerm [Syntax.Statement]
commClauseStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the case field of hydra.go.syntax.CommClause
commClauseWithCase :: Typed.TypedTerm Syntax.CommClause -> Typed.TypedTerm Syntax.CommCase -> Typed.TypedTerm Syntax.CommClause
commClauseWithCase original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CommClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.go.syntax.CommClause
commClauseWithStatements :: Typed.TypedTerm Syntax.CommClause -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.CommClause
commClauseWithStatements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CommClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.CompositeLit
compositeLit :: Typed.TypedTerm Syntax.LiteralType -> Typed.TypedTerm Syntax.LiteralValue -> Typed.TypedTerm Syntax.CompositeLit
compositeLit type_ value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the type field of hydra.go.syntax.CompositeLit
compositeLitType :: Typed.TypedTerm Syntax.CompositeLit -> Typed.TypedTerm Syntax.LiteralType
compositeLitType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.go.syntax.CompositeLit
compositeLitValue :: Typed.TypedTerm Syntax.CompositeLit -> Typed.TypedTerm Syntax.LiteralValue
compositeLitValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.go.syntax.CompositeLit
compositeLitWithType :: Typed.TypedTerm Syntax.CompositeLit -> Typed.TypedTerm Syntax.LiteralType -> Typed.TypedTerm Syntax.CompositeLit
compositeLitWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.go.syntax.CompositeLit
compositeLitWithValue :: Typed.TypedTerm Syntax.CompositeLit -> Typed.TypedTerm Syntax.LiteralValue -> Typed.TypedTerm Syntax.CompositeLit
compositeLitWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.ConstDecl wrapper
constDecl :: Typed.TypedTerm [Syntax.ConstSpec] -> Typed.TypedTerm Syntax.ConstDecl
constDecl x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ConstDecl"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.ConstSpec
constSpec :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ConstSpec
constSpec names type_ values =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm values)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.ConstSpec
constSpecNames :: Typed.TypedTerm Syntax.ConstSpec -> Typed.TypedTerm [Syntax.Identifier]
constSpecNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.ConstSpec
constSpecType :: Typed.TypedTerm Syntax.ConstSpec -> Typed.TypedTerm (Maybe Syntax.Type)
constSpecType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the values field of hydra.go.syntax.ConstSpec
constSpecValues :: Typed.TypedTerm Syntax.ConstSpec -> Typed.TypedTerm [Syntax.Expression]
constSpecValues x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.ConstSpec
constSpecWithNames :: Typed.TypedTerm Syntax.ConstSpec -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.ConstSpec
constSpecWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.ConstSpec
constSpecWithType :: Typed.TypedTerm Syntax.ConstSpec -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.ConstSpec
constSpecWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the values field of hydra.go.syntax.ConstSpec
constSpecWithValues :: Typed.TypedTerm Syntax.ConstSpec -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ConstSpec
constSpecWithValues original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.ContinueStmt wrapper
continueStmt :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.ContinueStmt
continueStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ContinueStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.Conversion
conversion :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Conversion
conversion type_ expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Conversion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.Conversion
conversionExpression :: Typed.TypedTerm Syntax.Conversion -> Typed.TypedTerm Syntax.Expression
conversionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.Conversion
conversionType :: Typed.TypedTerm Syntax.Conversion -> Typed.TypedTerm Syntax.Type
conversionType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.Conversion
conversionWithExpression :: Typed.TypedTerm Syntax.Conversion -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Conversion
conversionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Conversion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.go.syntax.Conversion
conversionWithType :: Typed.TypedTerm Syntax.Conversion -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Conversion
conversionWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Conversion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the const variant of hydra.go.syntax.Declaration
declarationConst :: Typed.TypedTerm Syntax.ConstDecl -> Typed.TypedTerm Syntax.Declaration
declarationConst x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.Declaration
declarationType :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm Syntax.Declaration
declarationType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the var variant of hydra.go.syntax.Declaration
declarationVar :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm Syntax.Declaration
declarationVar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.DeferStmt wrapper
deferStmt :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DeferStmt
deferStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.DeferStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the expression variant of hydra.go.syntax.Element
elementExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Element
elementExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ElementList wrapper
elementList :: Typed.TypedTerm [Syntax.KeyedElement] -> Typed.TypedTerm Syntax.ElementList
elementList x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ElementList"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the literal variant of hydra.go.syntax.Element
elementLiteral :: Typed.TypedTerm Syntax.LiteralValue -> Typed.TypedTerm Syntax.Element
elementLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the block variant of hydra.go.syntax.ElseClause
elseClauseBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ElseClause
elseClauseBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ElseClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.go.syntax.ElseClause
elseClauseIf :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm Syntax.ElseClause
elseClauseIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ElseClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.EmbeddedField
embeddedField :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm (Maybe Syntax.Tag) -> Typed.TypedTerm Syntax.EmbeddedField
embeddedField pointer type_ tag =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Typed.unTypedTerm pointer)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Typed.unTypedTerm tag)}]}))
-- | DSL accessor for the pointer field of hydra.go.syntax.EmbeddedField
embeddedFieldPointer :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm Bool
embeddedFieldPointer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
        Core.projectionFieldName = (Core.Name "pointer")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tag field of hydra.go.syntax.EmbeddedField
embeddedFieldTag :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm (Maybe Syntax.Tag)
embeddedFieldTag x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
        Core.projectionFieldName = (Core.Name "tag")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.EmbeddedField
embeddedFieldType :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm Syntax.TypeName
embeddedFieldType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pointer field of hydra.go.syntax.EmbeddedField
embeddedFieldWithPointer :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.EmbeddedField
embeddedFieldWithPointer original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tag field of hydra.go.syntax.EmbeddedField
embeddedFieldWithTag :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm (Maybe Syntax.Tag) -> Typed.TypedTerm Syntax.EmbeddedField
embeddedFieldWithTag original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "pointer")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.go.syntax.EmbeddedField
embeddedFieldWithType :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.EmbeddedField
embeddedFieldWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "pointer")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.EmptyStmt wrapper
emptyStmt :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.EmptyStmt
emptyStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.EmptyStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.ExprCaseClause
exprCaseClause :: Typed.TypedTerm (Maybe [Syntax.Expression]) -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.ExprCaseClause
exprCaseClause case_ statements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm statements)}]}))
-- | DSL accessor for the case field of hydra.go.syntax.ExprCaseClause
exprCaseClauseCase :: Typed.TypedTerm Syntax.ExprCaseClause -> Typed.TypedTerm (Maybe [Syntax.Expression])
exprCaseClauseCase x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.go.syntax.ExprCaseClause
exprCaseClauseStatements :: Typed.TypedTerm Syntax.ExprCaseClause -> Typed.TypedTerm [Syntax.Statement]
exprCaseClauseStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the case field of hydra.go.syntax.ExprCaseClause
exprCaseClauseWithCase :: Typed.TypedTerm Syntax.ExprCaseClause -> Typed.TypedTerm (Maybe [Syntax.Expression]) -> Typed.TypedTerm Syntax.ExprCaseClause
exprCaseClauseWithCase original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.go.syntax.ExprCaseClause
exprCaseClauseWithStatements :: Typed.TypedTerm Syntax.ExprCaseClause -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.ExprCaseClause
exprCaseClauseWithStatements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.ExprSwitchStmt
exprSwitchStmt :: Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm [Syntax.ExprCaseClause] -> Typed.TypedTerm Syntax.ExprSwitchStmt
exprSwitchStmt init expression cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtCases :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm [Syntax.ExprCaseClause]
exprSwitchStmtCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the expression field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtExpression :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm (Maybe Syntax.Expression)
exprSwitchStmtExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtInit :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm (Maybe Syntax.SimpleStmt)
exprSwitchStmtInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtWithCases :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm [Syntax.ExprCaseClause] -> Typed.TypedTerm Syntax.ExprSwitchStmt
exprSwitchStmtWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtWithExpression :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ExprSwitchStmt
exprSwitchStmtWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the init field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtWithInit :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.ExprSwitchStmt
exprSwitchStmtWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the binary variant of hydra.go.syntax.Expression
expressionBinary :: Typed.TypedTerm Syntax.BinaryExpr -> Typed.TypedTerm Syntax.Expression
expressionBinary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ExpressionStmt wrapper
expressionStmt :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ExpressionStmt
expressionStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ExpressionStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the unary variant of hydra.go.syntax.Expression
expressionUnary :: Typed.TypedTerm Syntax.UnaryExpr -> Typed.TypedTerm Syntax.Expression
expressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.FallthroughStmt wrapper
fallthroughStmt :: Typed.TypedTerm () -> Typed.TypedTerm Syntax.FallthroughStmt
fallthroughStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FallthroughStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the embedded variant of hydra.go.syntax.FieldDecl
fieldDeclEmbedded :: Typed.TypedTerm Syntax.EmbeddedField -> Typed.TypedTerm Syntax.FieldDecl
fieldDeclEmbedded x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.FieldDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "embedded"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.go.syntax.FieldDecl
fieldDeclNamed :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm Syntax.FieldDecl
fieldDeclNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.FieldDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.FloatLit wrapper
floatLit :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.FloatLit
floatLit x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FloatLit"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.ForClause
forClause :: Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.ForClause
forClause init condition post =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Typed.unTypedTerm post)}]}))
-- | DSL accessor for the condition field of hydra.go.syntax.ForClause
forClauseCondition :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm (Maybe Syntax.Expression)
forClauseCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.ForClause
forClauseInit :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm (Maybe Syntax.SimpleStmt)
forClauseInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the clause variant of hydra.go.syntax.ForClauseOrRange
forClauseOrRangeClause :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm Syntax.ForClauseOrRange
forClauseOrRangeClause x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ForClauseOrRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "clause"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the condition variant of hydra.go.syntax.ForClauseOrRange
forClauseOrRangeCondition :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ForClauseOrRange
forClauseOrRangeCondition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ForClauseOrRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "condition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the range variant of hydra.go.syntax.ForClauseOrRange
forClauseOrRangeRange :: Typed.TypedTerm Syntax.RangeClause -> Typed.TypedTerm Syntax.ForClauseOrRange
forClauseOrRangeRange x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ForClauseOrRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL accessor for the post field of hydra.go.syntax.ForClause
forClausePost :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm (Maybe Syntax.SimpleStmt)
forClausePost x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
        Core.projectionFieldName = (Core.Name "post")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the condition field of hydra.go.syntax.ForClause
forClauseWithCondition :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ForClause
forClauseWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "post")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the init field of hydra.go.syntax.ForClause
forClauseWithInit :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.ForClause
forClauseWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "post")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the post field of hydra.go.syntax.ForClause
forClauseWithPost :: Typed.TypedTerm Syntax.ForClause -> Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.ForClause
forClauseWithPost original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.ForStmt
forStmt :: Typed.TypedTerm (Maybe Syntax.ForClauseOrRange) -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ForStmt
forStmt clause body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clause"),
          Core.fieldTerm = (Typed.unTypedTerm clause)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.ForStmt
forStmtBody :: Typed.TypedTerm Syntax.ForStmt -> Typed.TypedTerm Syntax.Block
forStmtBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the clause field of hydra.go.syntax.ForStmt
forStmtClause :: Typed.TypedTerm Syntax.ForStmt -> Typed.TypedTerm (Maybe Syntax.ForClauseOrRange)
forStmtClause x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
        Core.projectionFieldName = (Core.Name "clause")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.ForStmt
forStmtWithBody :: Typed.TypedTerm Syntax.ForStmt -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.ForStmt
forStmtWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
              Core.projectionFieldName = (Core.Name "clause")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the clause field of hydra.go.syntax.ForStmt
forStmtWithClause :: Typed.TypedTerm Syntax.ForStmt -> Typed.TypedTerm (Maybe Syntax.ForClauseOrRange) -> Typed.TypedTerm Syntax.ForStmt
forStmtWithClause original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clause"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.FullSlice
fullSlice :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FullSlice
fullSlice low high max =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Typed.unTypedTerm low)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Typed.unTypedTerm high)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Typed.unTypedTerm max)}]}))
-- | DSL accessor for the high field of hydra.go.syntax.FullSlice
fullSliceHigh :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm Syntax.Expression
fullSliceHigh x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
        Core.projectionFieldName = (Core.Name "high")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the low field of hydra.go.syntax.FullSlice
fullSliceLow :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm (Maybe Syntax.Expression)
fullSliceLow x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
        Core.projectionFieldName = (Core.Name "low")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the max field of hydra.go.syntax.FullSlice
fullSliceMax :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm Syntax.Expression
fullSliceMax x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
        Core.projectionFieldName = (Core.Name "max")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the high field of hydra.go.syntax.FullSlice
fullSliceWithHigh :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FullSlice
fullSliceWithHigh original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "low")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the low field of hydra.go.syntax.FullSlice
fullSliceWithLow :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.FullSlice
fullSliceWithLow original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "high")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the max field of hydra.go.syntax.FullSlice
fullSliceWithMax :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FullSlice
fullSliceWithMax original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "low")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "high")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.FunctionBody wrapper
functionBody :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.FunctionBody
functionBody x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FunctionBody"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.FunctionDecl
functionDecl :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.TypeParameters) -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm (Maybe Syntax.FunctionBody) -> Typed.TypedTerm Syntax.FunctionDecl
functionDecl name typeParams signature body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.FunctionDecl
functionDeclBody :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm (Maybe Syntax.FunctionBody)
functionDeclBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.go.syntax.FunctionDecl
functionDeclName :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm Syntax.Identifier
functionDeclName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.FunctionDecl
functionDeclSignature :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm Syntax.Signature
functionDeclSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.go.syntax.FunctionDecl
functionDeclTypeParams :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm (Maybe Syntax.TypeParameters)
functionDeclTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.FunctionDecl
functionDeclWithBody :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm (Maybe Syntax.FunctionBody) -> Typed.TypedTerm Syntax.FunctionDecl
functionDeclWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.go.syntax.FunctionDecl
functionDeclWithName :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.FunctionDecl
functionDeclWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.go.syntax.FunctionDecl
functionDeclWithSignature :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.FunctionDecl
functionDeclWithSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.go.syntax.FunctionDecl
functionDeclWithTypeParams :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm (Maybe Syntax.TypeParameters) -> Typed.TypedTerm Syntax.FunctionDecl
functionDeclWithTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.FunctionLit
functionLit :: Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.FunctionBody -> Typed.TypedTerm Syntax.FunctionLit
functionLit signature body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.FunctionLit
functionLitBody :: Typed.TypedTerm Syntax.FunctionLit -> Typed.TypedTerm Syntax.FunctionBody
functionLitBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.FunctionLit
functionLitSignature :: Typed.TypedTerm Syntax.FunctionLit -> Typed.TypedTerm Syntax.Signature
functionLitSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.FunctionLit
functionLitWithBody :: Typed.TypedTerm Syntax.FunctionLit -> Typed.TypedTerm Syntax.FunctionBody -> Typed.TypedTerm Syntax.FunctionLit
functionLitWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the signature field of hydra.go.syntax.FunctionLit
functionLitWithSignature :: Typed.TypedTerm Syntax.FunctionLit -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.FunctionLit
functionLitWithSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.FunctionType wrapper
functionType :: Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.FunctionType
functionType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FunctionType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.GoStmt wrapper
goStmt :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.GoStmt
goStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.GoStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.GotoStmt wrapper
gotoStmt :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.GotoStmt
gotoStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.GotoStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.Identifier wrapper
identifier :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Identifier
identifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Identifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.IfStmt
ifStmt :: Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm (Maybe Syntax.ElseClause) -> Typed.TypedTerm Syntax.IfStmt
ifStmt init condition then_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm else_)}]}))
-- | DSL accessor for the condition field of hydra.go.syntax.IfStmt
ifStmtCondition :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm Syntax.Expression
ifStmtCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.go.syntax.IfStmt
ifStmtElse :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm (Maybe Syntax.ElseClause)
ifStmtElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.IfStmt
ifStmtInit :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm (Maybe Syntax.SimpleStmt)
ifStmtInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.go.syntax.IfStmt
ifStmtThen :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm Syntax.Block
ifStmtThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the condition field of hydra.go.syntax.IfStmt
ifStmtWithCondition :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfStmt
ifStmtWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.go.syntax.IfStmt
ifStmtWithElse :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm (Maybe Syntax.ElseClause) -> Typed.TypedTerm Syntax.IfStmt
ifStmtWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the init field of hydra.go.syntax.IfStmt
ifStmtWithInit :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.IfStmt
ifStmtWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the then field of hydra.go.syntax.IfStmt
ifStmtWithThen :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.IfStmt
ifStmtWithThen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.ImaginaryLit wrapper
imaginaryLit :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.ImaginaryLit
imaginaryLit x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ImaginaryLit"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the dot variant of hydra.go.syntax.ImportAlias
importAliasDot :: Typed.TypedTerm Syntax.ImportAlias
importAliasDot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ImportAlias"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the name variant of hydra.go.syntax.ImportAlias
importAliasName :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ImportAlias
importAliasName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ImportAlias"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ImportDecl wrapper
importDecl :: Typed.TypedTerm [Syntax.ImportSpec] -> Typed.TypedTerm Syntax.ImportDecl
importDecl x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ImportDecl"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.ImportPath wrapper
importPath :: Typed.TypedTerm Syntax.StringLit -> Typed.TypedTerm Syntax.ImportPath
importPath x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ImportPath"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.ImportSpec
importSpec :: Typed.TypedTerm (Maybe Syntax.ImportAlias) -> Typed.TypedTerm Syntax.ImportPath -> Typed.TypedTerm Syntax.ImportSpec
importSpec alias path =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Typed.unTypedTerm alias)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm path)}]}))
-- | DSL accessor for the alias field of hydra.go.syntax.ImportSpec
importSpecAlias :: Typed.TypedTerm Syntax.ImportSpec -> Typed.TypedTerm (Maybe Syntax.ImportAlias)
importSpecAlias x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
        Core.projectionFieldName = (Core.Name "alias")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the path field of hydra.go.syntax.ImportSpec
importSpecPath :: Typed.TypedTerm Syntax.ImportSpec -> Typed.TypedTerm Syntax.ImportPath
importSpecPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the alias field of hydra.go.syntax.ImportSpec
importSpecWithAlias :: Typed.TypedTerm Syntax.ImportSpec -> Typed.TypedTerm (Maybe Syntax.ImportAlias) -> Typed.TypedTerm Syntax.ImportSpec
importSpecWithAlias original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the path field of hydra.go.syntax.ImportSpec
importSpecWithPath :: Typed.TypedTerm Syntax.ImportSpec -> Typed.TypedTerm Syntax.ImportPath -> Typed.TypedTerm Syntax.ImportSpec
importSpecWithPath original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
              Core.projectionFieldName = (Core.Name "alias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.IncDecStmt
incDecStmt :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.IncDecStmt
incDecStmt expression increment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Typed.unTypedTerm increment)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.IncDecStmt
incDecStmtExpression :: Typed.TypedTerm Syntax.IncDecStmt -> Typed.TypedTerm Syntax.Expression
incDecStmtExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the increment field of hydra.go.syntax.IncDecStmt
incDecStmtIncrement :: Typed.TypedTerm Syntax.IncDecStmt -> Typed.TypedTerm Bool
incDecStmtIncrement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
        Core.projectionFieldName = (Core.Name "increment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.IncDecStmt
incDecStmtWithExpression :: Typed.TypedTerm Syntax.IncDecStmt -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IncDecStmt
incDecStmtWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
              Core.projectionFieldName = (Core.Name "increment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the increment field of hydra.go.syntax.IncDecStmt
incDecStmtWithIncrement :: Typed.TypedTerm Syntax.IncDecStmt -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.IncDecStmt
incDecStmtWithIncrement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.Index wrapper
index :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Index
index x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Index"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.IndexExpr
indexExpr :: Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IndexExpr
indexExpr expr index =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Typed.unTypedTerm index)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.IndexExpr
indexExprExpr :: Typed.TypedTerm Syntax.IndexExpr -> Typed.TypedTerm Syntax.PrimaryExpr
indexExprExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the index field of hydra.go.syntax.IndexExpr
indexExprIndex :: Typed.TypedTerm Syntax.IndexExpr -> Typed.TypedTerm Syntax.Expression
indexExprIndex x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
        Core.projectionFieldName = (Core.Name "index")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.IndexExpr
indexExprWithExpr :: Typed.TypedTerm Syntax.IndexExpr -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.IndexExpr
indexExprWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
              Core.projectionFieldName = (Core.Name "index")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the index field of hydra.go.syntax.IndexExpr
indexExprWithIndex :: Typed.TypedTerm Syntax.IndexExpr -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IndexExpr
indexExprWithIndex original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.IntLit wrapper
intLit :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.IntLit
intLit x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.IntLit"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the method variant of hydra.go.syntax.InterfaceElem
interfaceElemMethod :: Typed.TypedTerm Syntax.MethodElem -> Typed.TypedTerm Syntax.InterfaceElem
interfaceElemMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.InterfaceElem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.InterfaceElem
interfaceElemType :: Typed.TypedTerm Syntax.TypeElem -> Typed.TypedTerm Syntax.InterfaceElem
interfaceElemType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.InterfaceElem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.InterfaceType wrapper
interfaceType :: Typed.TypedTerm [Syntax.InterfaceElem] -> Typed.TypedTerm Syntax.InterfaceType
interfaceType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.InterfaceType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.InterpretedStringLit wrapper
interpretedStringLit :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.InterpretedStringLit
interpretedStringLit x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.InterpretedStringLit"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the expression variant of hydra.go.syntax.Key
keyExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Key
keyExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Key"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the field variant of hydra.go.syntax.Key
keyField :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Key
keyField x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Key"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.go.syntax.Key
keyLiteral :: Typed.TypedTerm Syntax.LiteralValue -> Typed.TypedTerm Syntax.Key
keyLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Key"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.KeyedElement
keyedElement :: Typed.TypedTerm (Maybe Syntax.Key) -> Typed.TypedTerm Syntax.Element -> Typed.TypedTerm Syntax.KeyedElement
keyedElement key element =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Typed.unTypedTerm element)}]}))
-- | DSL accessor for the element field of hydra.go.syntax.KeyedElement
keyedElementElement :: Typed.TypedTerm Syntax.KeyedElement -> Typed.TypedTerm Syntax.Element
keyedElementElement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the key field of hydra.go.syntax.KeyedElement
keyedElementKey :: Typed.TypedTerm Syntax.KeyedElement -> Typed.TypedTerm (Maybe Syntax.Key)
keyedElementKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the element field of hydra.go.syntax.KeyedElement
keyedElementWithElement :: Typed.TypedTerm Syntax.KeyedElement -> Typed.TypedTerm Syntax.Element -> Typed.TypedTerm Syntax.KeyedElement
keyedElementWithElement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the key field of hydra.go.syntax.KeyedElement
keyedElementWithKey :: Typed.TypedTerm Syntax.KeyedElement -> Typed.TypedTerm (Maybe Syntax.Key) -> Typed.TypedTerm Syntax.KeyedElement
keyedElementWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.LabeledStmt
labeledStmt :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.LabeledStmt
labeledStmt label statement =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm statement)}]}))
-- | DSL accessor for the label field of hydra.go.syntax.LabeledStmt
labeledStmtLabel :: Typed.TypedTerm Syntax.LabeledStmt -> Typed.TypedTerm Syntax.Identifier
labeledStmtLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statement field of hydra.go.syntax.LabeledStmt
labeledStmtStatement :: Typed.TypedTerm Syntax.LabeledStmt -> Typed.TypedTerm Syntax.Statement
labeledStmtStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the label field of hydra.go.syntax.LabeledStmt
labeledStmtWithLabel :: Typed.TypedTerm Syntax.LabeledStmt -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.LabeledStmt
labeledStmtWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
              Core.projectionFieldName = (Core.Name "statement")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.go.syntax.LabeledStmt
labeledStmtWithStatement :: Typed.TypedTerm Syntax.LabeledStmt -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.LabeledStmt
labeledStmtWithStatement original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the basic variant of hydra.go.syntax.Literal
literalBasic :: Typed.TypedTerm Syntax.BasicLit -> Typed.TypedTerm Syntax.Literal
literalBasic x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the composite variant of hydra.go.syntax.Literal
literalComposite :: Typed.TypedTerm Syntax.CompositeLit -> Typed.TypedTerm Syntax.Literal
literalComposite x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "composite"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.go.syntax.Literal
literalFunction :: Typed.TypedTerm Syntax.FunctionLit -> Typed.TypedTerm Syntax.Literal
literalFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the array variant of hydra.go.syntax.LiteralType
literalTypeArray :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.LiteralType
literalTypeArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the inferredArray variant of hydra.go.syntax.LiteralType
literalTypeInferredArray :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.LiteralType
literalTypeInferredArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inferredArray"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the map variant of hydra.go.syntax.LiteralType
literalTypeMap :: Typed.TypedTerm Syntax.MapType -> Typed.TypedTerm Syntax.LiteralType
literalTypeMap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.go.syntax.LiteralType
literalTypeName :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.LiteralType
literalTypeName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice variant of hydra.go.syntax.LiteralType
literalTypeSlice :: Typed.TypedTerm Syntax.SliceType -> Typed.TypedTerm Syntax.LiteralType
literalTypeSlice x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the struct variant of hydra.go.syntax.LiteralType
literalTypeStruct :: Typed.TypedTerm Syntax.StructType -> Typed.TypedTerm Syntax.LiteralType
literalTypeStruct x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.LiteralValue wrapper
literalValue :: Typed.TypedTerm [Syntax.KeyedElement] -> Typed.TypedTerm Syntax.LiteralValue
literalValue x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.LiteralValue"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.MapType
mapType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MapType
mapType key value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.go.syntax.MapType
mapTypeKey :: Typed.TypedTerm Syntax.MapType -> Typed.TypedTerm Syntax.Type
mapTypeKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.go.syntax.MapType
mapTypeValue :: Typed.TypedTerm Syntax.MapType -> Typed.TypedTerm Syntax.Type
mapTypeValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.go.syntax.MapType
mapTypeWithKey :: Typed.TypedTerm Syntax.MapType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MapType
mapTypeWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.go.syntax.MapType
mapTypeWithValue :: Typed.TypedTerm Syntax.MapType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MapType
mapTypeWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.MethodDecl
methodDecl :: Typed.TypedTerm Syntax.Receiver -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm (Maybe Syntax.FunctionBody) -> Typed.TypedTerm Syntax.MethodDecl
methodDecl receiver name signature body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Typed.unTypedTerm receiver)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.MethodDecl
methodDeclBody :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm (Maybe Syntax.FunctionBody)
methodDeclBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.go.syntax.MethodDecl
methodDeclName :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.Identifier
methodDeclName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the receiver field of hydra.go.syntax.MethodDecl
methodDeclReceiver :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.Receiver
methodDeclReceiver x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "receiver")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.MethodDecl
methodDeclSignature :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.Signature
methodDeclSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.MethodDecl
methodDeclWithBody :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm (Maybe Syntax.FunctionBody) -> Typed.TypedTerm Syntax.MethodDecl
methodDeclWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.go.syntax.MethodDecl
methodDeclWithName :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodDecl
methodDeclWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the receiver field of hydra.go.syntax.MethodDecl
methodDeclWithReceiver :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.Receiver -> Typed.TypedTerm Syntax.MethodDecl
methodDeclWithReceiver original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.go.syntax.MethodDecl
methodDeclWithSignature :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.MethodDecl
methodDeclWithSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.MethodElem
methodElem :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.MethodElem
methodElem name signature =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm signature)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.MethodElem
methodElemName :: Typed.TypedTerm Syntax.MethodElem -> Typed.TypedTerm Syntax.Identifier
methodElemName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.MethodElem
methodElemSignature :: Typed.TypedTerm Syntax.MethodElem -> Typed.TypedTerm Syntax.Signature
methodElemSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.MethodElem
methodElemWithName :: Typed.TypedTerm Syntax.MethodElem -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodElem
methodElemWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.go.syntax.MethodElem
methodElemWithSignature :: Typed.TypedTerm Syntax.MethodElem -> Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.MethodElem
methodElemWithSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.MethodExpr
methodExpr :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodExpr
methodExpr receiver method =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Typed.unTypedTerm receiver)},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Typed.unTypedTerm method)}]}))
-- | DSL accessor for the method field of hydra.go.syntax.MethodExpr
methodExprMethod :: Typed.TypedTerm Syntax.MethodExpr -> Typed.TypedTerm Syntax.Identifier
methodExprMethod x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
        Core.projectionFieldName = (Core.Name "method")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the receiver field of hydra.go.syntax.MethodExpr
methodExprReceiver :: Typed.TypedTerm Syntax.MethodExpr -> Typed.TypedTerm Syntax.Type
methodExprReceiver x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
        Core.projectionFieldName = (Core.Name "receiver")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the method field of hydra.go.syntax.MethodExpr
methodExprWithMethod :: Typed.TypedTerm Syntax.MethodExpr -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.MethodExpr
methodExprWithMethod original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the receiver field of hydra.go.syntax.MethodExpr
methodExprWithReceiver :: Typed.TypedTerm Syntax.MethodExpr -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.MethodExpr
methodExprWithReceiver original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
              Core.projectionFieldName = (Core.Name "method")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.Module
module_ :: Typed.TypedTerm Syntax.PackageClause -> Typed.TypedTerm [Syntax.ImportDecl] -> Typed.TypedTerm [Syntax.TopLevelDecl] -> Typed.TypedTerm Syntax.Module
module_ package imports declarations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.go.syntax.Module
moduleDeclarations :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.TopLevelDecl]
moduleDeclarations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the imports field of hydra.go.syntax.Module
moduleImports :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.ImportDecl]
moduleImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.go.syntax.Module
modulePackage :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm Syntax.PackageClause
modulePackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarations field of hydra.go.syntax.Module
moduleWithDeclarations :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.TopLevelDecl] -> Typed.TypedTerm Syntax.Module
moduleWithDeclarations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the imports field of hydra.go.syntax.Module
moduleWithImports :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.ImportDecl] -> Typed.TypedTerm Syntax.Module
moduleWithImports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the package field of hydra.go.syntax.Module
moduleWithPackage :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm Syntax.PackageClause -> Typed.TypedTerm Syntax.Module
moduleWithPackage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the bitClear variant of hydra.go.syntax.MulOp
mulOpBitClear :: Typed.TypedTerm Syntax.MulOp
mulOpBitClear =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitClear"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseAnd variant of hydra.go.syntax.MulOp
mulOpBitwiseAnd :: Typed.TypedTerm Syntax.MulOp
mulOpBitwiseAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the divide variant of hydra.go.syntax.MulOp
mulOpDivide :: Typed.TypedTerm Syntax.MulOp
mulOpDivide =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShift variant of hydra.go.syntax.MulOp
mulOpLeftShift :: Typed.TypedTerm Syntax.MulOp
mulOpLeftShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the multiply variant of hydra.go.syntax.MulOp
mulOpMultiply :: Typed.TypedTerm Syntax.MulOp
mulOpMultiply =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the remainder variant of hydra.go.syntax.MulOp
mulOpRemainder :: Typed.TypedTerm Syntax.MulOp
mulOpRemainder =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remainder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShift variant of hydra.go.syntax.MulOp
mulOpRightShift :: Typed.TypedTerm Syntax.MulOp
mulOpRightShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.NamedField
namedField :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm (Maybe Syntax.Tag) -> Typed.TypedTerm Syntax.NamedField
namedField names type_ tag =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Typed.unTypedTerm tag)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.NamedField
namedFieldNames :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm [Syntax.Identifier]
namedFieldNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tag field of hydra.go.syntax.NamedField
namedFieldTag :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm (Maybe Syntax.Tag)
namedFieldTag x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
        Core.projectionFieldName = (Core.Name "tag")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.NamedField
namedFieldType :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm Syntax.Type
namedFieldType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.NamedField
namedFieldWithNames :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.NamedField
namedFieldWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tag field of hydra.go.syntax.NamedField
namedFieldWithTag :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm (Maybe Syntax.Tag) -> Typed.TypedTerm Syntax.NamedField
namedFieldWithTag original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the type field of hydra.go.syntax.NamedField
namedFieldWithType :: Typed.TypedTerm Syntax.NamedField -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.NamedField
namedFieldWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the literal variant of hydra.go.syntax.Operand
operandLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Operand
operandLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Operand"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.go.syntax.Operand
operandName :: Typed.TypedTerm Syntax.OperandName -> Typed.TypedTerm Syntax.Operand
operandName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Operand"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.OperandName
operandName2 :: Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.OperandName
operandName2 name typeArgs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.OperandName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Typed.unTypedTerm typeArgs)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.OperandName
operandNameName :: Typed.TypedTerm Syntax.OperandName -> Typed.TypedTerm Syntax.QualifiedIdent
operandNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArgs field of hydra.go.syntax.OperandName
operandNameTypeArgs :: Typed.TypedTerm Syntax.OperandName -> Typed.TypedTerm [Syntax.Type]
operandNameTypeArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
        Core.projectionFieldName = (Core.Name "typeArgs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.OperandName
operandNameWithName :: Typed.TypedTerm Syntax.OperandName -> Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm Syntax.OperandName
operandNameWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.OperandName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
              Core.projectionFieldName = (Core.Name "typeArgs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArgs field of hydra.go.syntax.OperandName
operandNameWithTypeArgs :: Typed.TypedTerm Syntax.OperandName -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.OperandName
operandNameWithTypeArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.OperandName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the paren variant of hydra.go.syntax.Operand
operandParen :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Operand
operandParen x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Operand"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.PackageClause wrapper
packageClause :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.PackageClause
packageClause x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.PackageClause"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.ParameterDecl
parameterDecl :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ParameterDecl
parameterDecl names variadic type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Typed.unTypedTerm variadic)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.ParameterDecl
parameterDeclNames :: Typed.TypedTerm Syntax.ParameterDecl -> Typed.TypedTerm [Syntax.Identifier]
parameterDeclNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.ParameterDecl
parameterDeclType :: Typed.TypedTerm Syntax.ParameterDecl -> Typed.TypedTerm Syntax.Type
parameterDeclType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the variadic field of hydra.go.syntax.ParameterDecl
parameterDeclVariadic :: Typed.TypedTerm Syntax.ParameterDecl -> Typed.TypedTerm Bool
parameterDeclVariadic x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
        Core.projectionFieldName = (Core.Name "variadic")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.ParameterDecl
parameterDeclWithNames :: Typed.TypedTerm Syntax.ParameterDecl -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.ParameterDecl
parameterDeclWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "variadic")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.ParameterDecl
parameterDeclWithType :: Typed.TypedTerm Syntax.ParameterDecl -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ParameterDecl
parameterDeclWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "variadic")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the variadic field of hydra.go.syntax.ParameterDecl
parameterDeclWithVariadic :: Typed.TypedTerm Syntax.ParameterDecl -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ParameterDecl
parameterDeclWithVariadic original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.Parameters wrapper
parameters :: Typed.TypedTerm [Syntax.ParameterDecl] -> Typed.TypedTerm Syntax.Parameters
parameters x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Parameters"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.PointerType wrapper
pointerType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.PointerType
pointerType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.PointerType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the call variant of hydra.go.syntax.PrimaryExpr
primaryExprCall :: Typed.TypedTerm Syntax.CallExpr -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprCall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the conversion variant of hydra.go.syntax.PrimaryExpr
primaryExprConversion :: Typed.TypedTerm Syntax.Conversion -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprConversion x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conversion"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the index variant of hydra.go.syntax.PrimaryExpr
primaryExprIndex :: Typed.TypedTerm Syntax.IndexExpr -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprIndex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the methodExpr variant of hydra.go.syntax.PrimaryExpr
primaryExprMethodExpr :: Typed.TypedTerm Syntax.MethodExpr -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprMethodExpr x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodExpr"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the operand variant of hydra.go.syntax.PrimaryExpr
primaryExprOperand :: Typed.TypedTerm Syntax.Operand -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprOperand x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "operand"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the selector variant of hydra.go.syntax.PrimaryExpr
primaryExprSelector :: Typed.TypedTerm Syntax.SelectorExpr -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprSelector x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selector"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice variant of hydra.go.syntax.PrimaryExpr
primaryExprSlice :: Typed.TypedTerm Syntax.SliceExpr -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprSlice x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeAssertion variant of hydra.go.syntax.PrimaryExpr
primaryExprTypeAssertion :: Typed.TypedTerm Syntax.TypeAssertionExpr -> Typed.TypedTerm Syntax.PrimaryExpr
primaryExprTypeAssertion x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAssertion"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.QualifiedIdent
qualifiedIdent :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.QualifiedIdent
qualifiedIdent package name =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.QualifiedIdent
qualifiedIdentName :: Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm Syntax.Identifier
qualifiedIdentName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.go.syntax.QualifiedIdent
qualifiedIdentPackage :: Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm (Maybe Syntax.Identifier)
qualifiedIdentPackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.QualifiedIdent
qualifiedIdentWithName :: Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.QualifiedIdent
qualifiedIdentWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the package field of hydra.go.syntax.QualifiedIdent
qualifiedIdentWithPackage :: Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.QualifiedIdent
qualifiedIdentWithPackage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.RangeClause
rangeClause :: Typed.TypedTerm (Maybe Syntax.RangeVars) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.RangeClause
rangeClause vars expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Typed.unTypedTerm vars)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.RangeClause
rangeClauseExpression :: Typed.TypedTerm Syntax.RangeClause -> Typed.TypedTerm Syntax.Expression
rangeClauseExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vars field of hydra.go.syntax.RangeClause
rangeClauseVars :: Typed.TypedTerm Syntax.RangeClause -> Typed.TypedTerm (Maybe Syntax.RangeVars)
rangeClauseVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
        Core.projectionFieldName = (Core.Name "vars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.RangeClause
rangeClauseWithExpression :: Typed.TypedTerm Syntax.RangeClause -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.RangeClause
rangeClauseWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
              Core.projectionFieldName = (Core.Name "vars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vars field of hydra.go.syntax.RangeClause
rangeClauseWithVars :: Typed.TypedTerm Syntax.RangeClause -> Typed.TypedTerm (Maybe Syntax.RangeVars) -> Typed.TypedTerm Syntax.RangeClause
rangeClauseWithVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the assign variant of hydra.go.syntax.RangeVars
rangeVarsAssign :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.RangeVars
rangeVarsAssign x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RangeVars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the declare variant of hydra.go.syntax.RangeVars
rangeVarsDeclare :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.RangeVars
rangeVarsDeclare x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RangeVars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declare"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.RawStringLit wrapper
rawStringLit :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.RawStringLit
rawStringLit x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.RawStringLit"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.ReceiveCase
receiveCase :: Typed.TypedTerm (Maybe Syntax.RangeVars) -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ReceiveCase
receiveCase vars expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Typed.unTypedTerm vars)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.ReceiveCase
receiveCaseExpression :: Typed.TypedTerm Syntax.ReceiveCase -> Typed.TypedTerm Syntax.Expression
receiveCaseExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vars field of hydra.go.syntax.ReceiveCase
receiveCaseVars :: Typed.TypedTerm Syntax.ReceiveCase -> Typed.TypedTerm (Maybe Syntax.RangeVars)
receiveCaseVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
        Core.projectionFieldName = (Core.Name "vars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.ReceiveCase
receiveCaseWithExpression :: Typed.TypedTerm Syntax.ReceiveCase -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ReceiveCase
receiveCaseWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
              Core.projectionFieldName = (Core.Name "vars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vars field of hydra.go.syntax.ReceiveCase
receiveCaseWithVars :: Typed.TypedTerm Syntax.ReceiveCase -> Typed.TypedTerm (Maybe Syntax.RangeVars) -> Typed.TypedTerm Syntax.ReceiveCase
receiveCaseWithVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.Receiver
receiver :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Receiver
receiver name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Receiver"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.Receiver
receiverName :: Typed.TypedTerm Syntax.Receiver -> Typed.TypedTerm (Maybe Syntax.Identifier)
receiverName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.Receiver
receiverType :: Typed.TypedTerm Syntax.Receiver -> Typed.TypedTerm Syntax.Type
receiverType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.Receiver
receiverWithName :: Typed.TypedTerm Syntax.Receiver -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.Receiver
receiverWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Receiver"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.Receiver
receiverWithType :: Typed.TypedTerm Syntax.Receiver -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Receiver
receiverWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Receiver"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the equal variant of hydra.go.syntax.RelOp
relOpEqual :: Typed.TypedTerm Syntax.RelOp
relOpEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greater variant of hydra.go.syntax.RelOp
relOpGreater :: Typed.TypedTerm Syntax.RelOp
relOpGreater =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterEqual variant of hydra.go.syntax.RelOp
relOpGreaterEqual :: Typed.TypedTerm Syntax.RelOp
relOpGreaterEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the less variant of hydra.go.syntax.RelOp
relOpLess :: Typed.TypedTerm Syntax.RelOp
relOpLess =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessEqual variant of hydra.go.syntax.RelOp
relOpLessEqual :: Typed.TypedTerm Syntax.RelOp
relOpLessEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.go.syntax.RelOp
relOpNotEqual :: Typed.TypedTerm Syntax.RelOp
relOpNotEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the parameters variant of hydra.go.syntax.Result
resultParameters :: Typed.TypedTerm Syntax.Parameters -> Typed.TypedTerm Syntax.Result
resultParameters x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameters"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.Result
resultType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Result
resultType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ReturnStmt wrapper
returnStmt :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ReturnStmt
returnStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ReturnStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.RuneLit wrapper
runeLit :: Typed.TypedTerm Int -> Typed.TypedTerm Syntax.RuneLit
runeLit x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.RuneLit"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.SelectStmt wrapper
selectStmt :: Typed.TypedTerm [Syntax.CommClause] -> Typed.TypedTerm Syntax.SelectStmt
selectStmt x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.SelectStmt"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.Selector wrapper
selector :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Selector
selector x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Selector"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.SelectorExpr
selectorExpr :: Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.SelectorExpr
selectorExpr expr selector =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "selector"),
          Core.fieldTerm = (Typed.unTypedTerm selector)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.SelectorExpr
selectorExprExpr :: Typed.TypedTerm Syntax.SelectorExpr -> Typed.TypedTerm Syntax.PrimaryExpr
selectorExprExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the selector field of hydra.go.syntax.SelectorExpr
selectorExprSelector :: Typed.TypedTerm Syntax.SelectorExpr -> Typed.TypedTerm Syntax.Identifier
selectorExprSelector x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
        Core.projectionFieldName = (Core.Name "selector")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.SelectorExpr
selectorExprWithExpr :: Typed.TypedTerm Syntax.SelectorExpr -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.SelectorExpr
selectorExprWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "selector"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
              Core.projectionFieldName = (Core.Name "selector")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the selector field of hydra.go.syntax.SelectorExpr
selectorExprWithSelector :: Typed.TypedTerm Syntax.SelectorExpr -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.SelectorExpr
selectorExprWithSelector original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selector"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.SendStmt
sendStmt :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SendStmt
sendStmt channel value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "channel"),
          Core.fieldTerm = (Typed.unTypedTerm channel)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the channel field of hydra.go.syntax.SendStmt
sendStmtChannel :: Typed.TypedTerm Syntax.SendStmt -> Typed.TypedTerm Syntax.Expression
sendStmtChannel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
        Core.projectionFieldName = (Core.Name "channel")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.go.syntax.SendStmt
sendStmtValue :: Typed.TypedTerm Syntax.SendStmt -> Typed.TypedTerm Syntax.Expression
sendStmtValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the channel field of hydra.go.syntax.SendStmt
sendStmtWithChannel :: Typed.TypedTerm Syntax.SendStmt -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SendStmt
sendStmtWithChannel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "channel"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.go.syntax.SendStmt
sendStmtWithValue :: Typed.TypedTerm Syntax.SendStmt -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SendStmt
sendStmtWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "channel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
              Core.projectionFieldName = (Core.Name "channel")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.ShortVarDecl
shortVarDecl :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ShortVarDecl
shortVarDecl names values =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm values)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.ShortVarDecl
shortVarDeclNames :: Typed.TypedTerm Syntax.ShortVarDecl -> Typed.TypedTerm [Syntax.Identifier]
shortVarDeclNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the values field of hydra.go.syntax.ShortVarDecl
shortVarDeclValues :: Typed.TypedTerm Syntax.ShortVarDecl -> Typed.TypedTerm [Syntax.Expression]
shortVarDeclValues x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.ShortVarDecl
shortVarDeclWithNames :: Typed.TypedTerm Syntax.ShortVarDecl -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.ShortVarDecl
shortVarDeclWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the values field of hydra.go.syntax.ShortVarDecl
shortVarDeclWithValues :: Typed.TypedTerm Syntax.ShortVarDecl -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.ShortVarDecl
shortVarDeclWithValues original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.Signature
signature :: Typed.TypedTerm Syntax.Parameters -> Typed.TypedTerm (Maybe Syntax.Result) -> Typed.TypedTerm Syntax.Signature
signature parameters result =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Signature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Typed.unTypedTerm result)}]}))
-- | DSL accessor for the parameters field of hydra.go.syntax.Signature
signatureParameters :: Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.Parameters
signatureParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the result field of hydra.go.syntax.Signature
signatureResult :: Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm (Maybe Syntax.Result)
signatureResult x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
        Core.projectionFieldName = (Core.Name "result")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the parameters field of hydra.go.syntax.Signature
signatureWithParameters :: Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm Syntax.Parameters -> Typed.TypedTerm Syntax.Signature
signatureWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Signature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the result field of hydra.go.syntax.Signature
signatureWithResult :: Typed.TypedTerm Syntax.Signature -> Typed.TypedTerm (Maybe Syntax.Result) -> Typed.TypedTerm Syntax.Signature
signatureWithResult original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Signature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.SimpleSlice
simpleSlice :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SimpleSlice
simpleSlice low high =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Typed.unTypedTerm low)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Typed.unTypedTerm high)}]}))
-- | DSL accessor for the high field of hydra.go.syntax.SimpleSlice
simpleSliceHigh :: Typed.TypedTerm Syntax.SimpleSlice -> Typed.TypedTerm (Maybe Syntax.Expression)
simpleSliceHigh x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
        Core.projectionFieldName = (Core.Name "high")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the low field of hydra.go.syntax.SimpleSlice
simpleSliceLow :: Typed.TypedTerm Syntax.SimpleSlice -> Typed.TypedTerm (Maybe Syntax.Expression)
simpleSliceLow x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
        Core.projectionFieldName = (Core.Name "low")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the high field of hydra.go.syntax.SimpleSlice
simpleSliceWithHigh :: Typed.TypedTerm Syntax.SimpleSlice -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SimpleSlice
simpleSliceWithHigh original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
              Core.projectionFieldName = (Core.Name "low")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the low field of hydra.go.syntax.SimpleSlice
simpleSliceWithLow :: Typed.TypedTerm Syntax.SimpleSlice -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SimpleSlice
simpleSliceWithLow original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
              Core.projectionFieldName = (Core.Name "high")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the assignment variant of hydra.go.syntax.SimpleStmt
simpleStmtAssignment :: Typed.TypedTerm Syntax.Assignment -> Typed.TypedTerm Syntax.SimpleStmt
simpleStmtAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the empty variant of hydra.go.syntax.SimpleStmt
simpleStmtEmpty :: Typed.TypedTerm Syntax.EmptyStmt -> Typed.TypedTerm Syntax.SimpleStmt
simpleStmtEmpty x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.go.syntax.SimpleStmt
simpleStmtExpression :: Typed.TypedTerm Syntax.ExpressionStmt -> Typed.TypedTerm Syntax.SimpleStmt
simpleStmtExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the incDec variant of hydra.go.syntax.SimpleStmt
simpleStmtIncDec :: Typed.TypedTerm Syntax.IncDecStmt -> Typed.TypedTerm Syntax.SimpleStmt
simpleStmtIncDec x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incDec"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the send variant of hydra.go.syntax.SimpleStmt
simpleStmtSend :: Typed.TypedTerm Syntax.SendStmt -> Typed.TypedTerm Syntax.SimpleStmt
simpleStmtSend x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "send"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the shortVarDecl variant of hydra.go.syntax.SimpleStmt
simpleStmtShortVarDecl :: Typed.TypedTerm Syntax.ShortVarDecl -> Typed.TypedTerm Syntax.SimpleStmt
simpleStmtShortVarDecl x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortVarDecl"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.SliceExpr
sliceExpr :: Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.Slice -> Typed.TypedTerm Syntax.SliceExpr
sliceExpr expr slice =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "slice"),
          Core.fieldTerm = (Typed.unTypedTerm slice)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.SliceExpr
sliceExprExpr :: Typed.TypedTerm Syntax.SliceExpr -> Typed.TypedTerm Syntax.PrimaryExpr
sliceExprExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the slice field of hydra.go.syntax.SliceExpr
sliceExprSlice :: Typed.TypedTerm Syntax.SliceExpr -> Typed.TypedTerm Syntax.Slice
sliceExprSlice x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
        Core.projectionFieldName = (Core.Name "slice")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.SliceExpr
sliceExprWithExpr :: Typed.TypedTerm Syntax.SliceExpr -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.SliceExpr
sliceExprWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "slice"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
              Core.projectionFieldName = (Core.Name "slice")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the slice field of hydra.go.syntax.SliceExpr
sliceExprWithSlice :: Typed.TypedTerm Syntax.SliceExpr -> Typed.TypedTerm Syntax.Slice -> Typed.TypedTerm Syntax.SliceExpr
sliceExprWithSlice original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "slice"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the full variant of hydra.go.syntax.Slice
sliceFull :: Typed.TypedTerm Syntax.FullSlice -> Typed.TypedTerm Syntax.Slice
sliceFull x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "full"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.go.syntax.Slice
sliceSimple :: Typed.TypedTerm Syntax.SimpleSlice -> Typed.TypedTerm Syntax.Slice
sliceSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.SliceType wrapper
sliceType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.SliceType
sliceType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.SliceType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.SourceFile
sourceFile :: Typed.TypedTerm Syntax.PackageClause -> Typed.TypedTerm [Syntax.ImportDecl] -> Typed.TypedTerm [Syntax.TopLevelDecl] -> Typed.TypedTerm Syntax.SourceFile
sourceFile package imports declarations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.go.syntax.SourceFile
sourceFileDeclarations :: Typed.TypedTerm Syntax.SourceFile -> Typed.TypedTerm [Syntax.TopLevelDecl]
sourceFileDeclarations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the imports field of hydra.go.syntax.SourceFile
sourceFileImports :: Typed.TypedTerm Syntax.SourceFile -> Typed.TypedTerm [Syntax.ImportDecl]
sourceFileImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.go.syntax.SourceFile
sourceFilePackage :: Typed.TypedTerm Syntax.SourceFile -> Typed.TypedTerm Syntax.PackageClause
sourceFilePackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarations field of hydra.go.syntax.SourceFile
sourceFileWithDeclarations :: Typed.TypedTerm Syntax.SourceFile -> Typed.TypedTerm [Syntax.TopLevelDecl] -> Typed.TypedTerm Syntax.SourceFile
sourceFileWithDeclarations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the imports field of hydra.go.syntax.SourceFile
sourceFileWithImports :: Typed.TypedTerm Syntax.SourceFile -> Typed.TypedTerm [Syntax.ImportDecl] -> Typed.TypedTerm Syntax.SourceFile
sourceFileWithImports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the package field of hydra.go.syntax.SourceFile
sourceFileWithPackage :: Typed.TypedTerm Syntax.SourceFile -> Typed.TypedTerm Syntax.PackageClause -> Typed.TypedTerm Syntax.SourceFile
sourceFileWithPackage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the block variant of hydra.go.syntax.Statement
statementBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm Syntax.Statement
statementBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the break variant of hydra.go.syntax.Statement
statementBreak :: Typed.TypedTerm Syntax.BreakStmt -> Typed.TypedTerm Syntax.Statement
statementBreak x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the continue variant of hydra.go.syntax.Statement
statementContinue :: Typed.TypedTerm Syntax.ContinueStmt -> Typed.TypedTerm Syntax.Statement
statementContinue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the declaration variant of hydra.go.syntax.Statement
statementDeclaration :: Typed.TypedTerm Syntax.Declaration -> Typed.TypedTerm Syntax.Statement
statementDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the defer variant of hydra.go.syntax.Statement
statementDefer :: Typed.TypedTerm Syntax.DeferStmt -> Typed.TypedTerm Syntax.Statement
statementDefer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "defer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the fallthrough variant of hydra.go.syntax.Statement
statementFallthrough :: Typed.TypedTerm Syntax.FallthroughStmt -> Typed.TypedTerm Syntax.Statement
statementFallthrough x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fallthrough"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.go.syntax.Statement
statementFor :: Typed.TypedTerm Syntax.ForStmt -> Typed.TypedTerm Syntax.Statement
statementFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the go variant of hydra.go.syntax.Statement
statementGo :: Typed.TypedTerm Syntax.GoStmt -> Typed.TypedTerm Syntax.Statement
statementGo x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "go"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the goto variant of hydra.go.syntax.Statement
statementGoto :: Typed.TypedTerm Syntax.GotoStmt -> Typed.TypedTerm Syntax.Statement
statementGoto x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "goto"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.go.syntax.Statement
statementIf :: Typed.TypedTerm Syntax.IfStmt -> Typed.TypedTerm Syntax.Statement
statementIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the labeled variant of hydra.go.syntax.Statement
statementLabeled :: Typed.TypedTerm Syntax.LabeledStmt -> Typed.TypedTerm Syntax.Statement
statementLabeled x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.go.syntax.Statement
statementReturn :: Typed.TypedTerm Syntax.ReturnStmt -> Typed.TypedTerm Syntax.Statement
statementReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the select variant of hydra.go.syntax.Statement
statementSelect :: Typed.TypedTerm Syntax.SelectStmt -> Typed.TypedTerm Syntax.Statement
statementSelect x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.go.syntax.Statement
statementSimple :: Typed.TypedTerm Syntax.SimpleStmt -> Typed.TypedTerm Syntax.Statement
statementSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the switch variant of hydra.go.syntax.Statement
statementSwitch :: Typed.TypedTerm Syntax.SwitchStmt -> Typed.TypedTerm Syntax.Statement
statementSwitch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interpreted variant of hydra.go.syntax.StringLit
stringLitInterpreted :: Typed.TypedTerm Syntax.InterpretedStringLit -> Typed.TypedTerm Syntax.StringLit
stringLitInterpreted x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.StringLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpreted"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the raw variant of hydra.go.syntax.StringLit
stringLitRaw :: Typed.TypedTerm Syntax.RawStringLit -> Typed.TypedTerm Syntax.StringLit
stringLitRaw x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.StringLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.StructType wrapper
structType :: Typed.TypedTerm [Syntax.FieldDecl] -> Typed.TypedTerm Syntax.StructType
structType x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.StructType"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the expression variant of hydra.go.syntax.SwitchStmt
switchStmtExpression :: Typed.TypedTerm Syntax.ExprSwitchStmt -> Typed.TypedTerm Syntax.SwitchStmt
switchStmtExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SwitchStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.SwitchStmt
switchStmtType :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm Syntax.SwitchStmt
switchStmtType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SwitchStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.Tag wrapper
tag :: Typed.TypedTerm Syntax.StringLit -> Typed.TypedTerm Syntax.Tag
tag x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Tag"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the declaration variant of hydra.go.syntax.TopLevelDecl
topLevelDeclDeclaration :: Typed.TypedTerm Syntax.Declaration -> Typed.TypedTerm Syntax.TopLevelDecl
topLevelDeclDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TopLevelDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.go.syntax.TopLevelDecl
topLevelDeclFunction :: Typed.TypedTerm Syntax.FunctionDecl -> Typed.TypedTerm Syntax.TopLevelDecl
topLevelDeclFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TopLevelDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the method variant of hydra.go.syntax.TopLevelDecl
topLevelDeclMethod :: Typed.TypedTerm Syntax.MethodDecl -> Typed.TypedTerm Syntax.TopLevelDecl
topLevelDeclMethod x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TopLevelDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.TypeAssertion wrapper
typeAssertion :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeAssertion
typeAssertion x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeAssertion"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.TypeAssertionExpr
typeAssertionExpr :: Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeAssertionExpr
typeAssertionExpr expr type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprExpr :: Typed.TypedTerm Syntax.TypeAssertionExpr -> Typed.TypedTerm Syntax.PrimaryExpr
typeAssertionExprExpr x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprType :: Typed.TypedTerm Syntax.TypeAssertionExpr -> Typed.TypedTerm Syntax.Type
typeAssertionExprType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprWithExpr :: Typed.TypedTerm Syntax.TypeAssertionExpr -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.TypeAssertionExpr
typeAssertionExprWithExpr original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprWithType :: Typed.TypedTerm Syntax.TypeAssertionExpr -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeAssertionExpr
typeAssertionExprWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.TypeCaseClause
typeCaseClause :: Typed.TypedTerm (Maybe [Syntax.Type]) -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.TypeCaseClause
typeCaseClause case_ statements =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm statements)}]}))
-- | DSL accessor for the case field of hydra.go.syntax.TypeCaseClause
typeCaseClauseCase :: Typed.TypedTerm Syntax.TypeCaseClause -> Typed.TypedTerm (Maybe [Syntax.Type])
typeCaseClauseCase x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the statements field of hydra.go.syntax.TypeCaseClause
typeCaseClauseStatements :: Typed.TypedTerm Syntax.TypeCaseClause -> Typed.TypedTerm [Syntax.Statement]
typeCaseClauseStatements x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the case field of hydra.go.syntax.TypeCaseClause
typeCaseClauseWithCase :: Typed.TypedTerm Syntax.TypeCaseClause -> Typed.TypedTerm (Maybe [Syntax.Type]) -> Typed.TypedTerm Syntax.TypeCaseClause
typeCaseClauseWithCase original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.go.syntax.TypeCaseClause
typeCaseClauseWithStatements :: Typed.TypedTerm Syntax.TypeCaseClause -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.TypeCaseClause
typeCaseClauseWithStatements original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.TypeConstraint wrapper
typeConstraint :: Typed.TypedTerm Syntax.TypeElem -> Typed.TypedTerm Syntax.TypeConstraint
typeConstraint x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeConstraint"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.go.syntax.TypeDecl wrapper
typeDecl :: Typed.TypedTerm [Syntax.TypeSpec] -> Typed.TypedTerm Syntax.TypeDecl
typeDecl x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeDecl"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.TypeDef
typeDef :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.TypeParameters) -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeDef
typeDef name typeParams type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.TypeDef
typeDefName :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm Syntax.Identifier
typeDefName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.TypeDef
typeDefType :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm Syntax.Type
typeDefType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.go.syntax.TypeDef
typeDefTypeParams :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm (Maybe Syntax.TypeParameters)
typeDefTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.TypeDef
typeDefWithName :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.TypeDef
typeDefWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.TypeDef
typeDefWithType :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeDef
typeDefWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeParams field of hydra.go.syntax.TypeDef
typeDefWithTypeParams :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm (Maybe Syntax.TypeParameters) -> Typed.TypedTerm Syntax.TypeDef
typeDefWithTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.TypeElem wrapper
typeElem :: Typed.TypedTerm [Syntax.TypeTerm] -> Typed.TypedTerm Syntax.TypeElem
typeElem x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeElem"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the array variant of hydra.go.syntax.TypeLit
typeLitArray :: Typed.TypedTerm Syntax.ArrayType -> Typed.TypedTerm Syntax.TypeLit
typeLitArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the channel variant of hydra.go.syntax.TypeLit
typeLitChannel :: Typed.TypedTerm Syntax.ChannelType -> Typed.TypedTerm Syntax.TypeLit
typeLitChannel x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "channel"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.go.syntax.TypeLit
typeLitFunction :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.TypeLit
typeLitFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.go.syntax.TypeLit
typeLitInterface :: Typed.TypedTerm Syntax.InterfaceType -> Typed.TypedTerm Syntax.TypeLit
typeLitInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the map variant of hydra.go.syntax.TypeLit
typeLitMap :: Typed.TypedTerm Syntax.MapType -> Typed.TypedTerm Syntax.TypeLit
typeLitMap x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pointer variant of hydra.go.syntax.TypeLit
typeLitPointer :: Typed.TypedTerm Syntax.PointerType -> Typed.TypedTerm Syntax.TypeLit
typeLitPointer x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pointer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the slice variant of hydra.go.syntax.TypeLit
typeLitSlice :: Typed.TypedTerm Syntax.SliceType -> Typed.TypedTerm Syntax.TypeLit
typeLitSlice x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the struct variant of hydra.go.syntax.TypeLit
typeLitStruct :: Typed.TypedTerm Syntax.StructType -> Typed.TypedTerm Syntax.TypeLit
typeLitStruct x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.go.syntax.Type
typeLiteral :: Typed.TypedTerm Syntax.TypeLit -> Typed.TypedTerm Syntax.Type
typeLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.go.syntax.Type
typeName :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.Type
typeName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.TypeName
typeName2 :: Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.TypeName
typeName2 name typeArgs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Typed.unTypedTerm typeArgs)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.TypeName
typeNameName :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.QualifiedIdent
typeNameName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeArgs field of hydra.go.syntax.TypeName
typeNameTypeArgs :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm [Syntax.Type]
typeNameTypeArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "typeArgs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.TypeName
typeNameWithName :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm Syntax.QualifiedIdent -> Typed.TypedTerm Syntax.TypeName
typeNameWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
              Core.projectionFieldName = (Core.Name "typeArgs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeArgs field of hydra.go.syntax.TypeName
typeNameWithTypeArgs :: Typed.TypedTerm Syntax.TypeName -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.TypeName
typeNameWithTypeArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.TypeParamDecl
typeParamDecl :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.TypeConstraint -> Typed.TypedTerm Syntax.TypeParamDecl
typeParamDecl names constraint =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Typed.unTypedTerm constraint)}]}))
-- | DSL accessor for the constraint field of hydra.go.syntax.TypeParamDecl
typeParamDeclConstraint :: Typed.TypedTerm Syntax.TypeParamDecl -> Typed.TypedTerm Syntax.TypeConstraint
typeParamDeclConstraint x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
        Core.projectionFieldName = (Core.Name "constraint")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the names field of hydra.go.syntax.TypeParamDecl
typeParamDeclNames :: Typed.TypedTerm Syntax.TypeParamDecl -> Typed.TypedTerm [Syntax.Identifier]
typeParamDeclNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the constraint field of hydra.go.syntax.TypeParamDecl
typeParamDeclWithConstraint :: Typed.TypedTerm Syntax.TypeParamDecl -> Typed.TypedTerm Syntax.TypeConstraint -> Typed.TypedTerm Syntax.TypeParamDecl
typeParamDeclWithConstraint original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the names field of hydra.go.syntax.TypeParamDecl
typeParamDeclWithNames :: Typed.TypedTerm Syntax.TypeParamDecl -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.TypeParamDecl
typeParamDeclWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
              Core.projectionFieldName = (Core.Name "constraint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.TypeParameters wrapper
typeParameters :: Typed.TypedTerm [Syntax.TypeParamDecl] -> Typed.TypedTerm Syntax.TypeParameters
typeParameters x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeParameters"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the paren variant of hydra.go.syntax.Type
typeParen :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type
typeParen x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the alias variant of hydra.go.syntax.TypeSpec
typeSpecAlias :: Typed.TypedTerm Syntax.AliasDecl -> Typed.TypedTerm Syntax.TypeSpec
typeSpecAlias x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alias"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the definition variant of hydra.go.syntax.TypeSpec
typeSpecDefinition :: Typed.TypedTerm Syntax.TypeDef -> Typed.TypedTerm Syntax.TypeSpec
typeSpecDefinition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.go.syntax.TypeSwitchGuard
typeSwitchGuard :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.TypeSwitchGuard
typeSwitchGuard name expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardExpression :: Typed.TypedTerm Syntax.TypeSwitchGuard -> Typed.TypedTerm Syntax.PrimaryExpr
typeSwitchGuardExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardName :: Typed.TypedTerm Syntax.TypeSwitchGuard -> Typed.TypedTerm (Maybe Syntax.Identifier)
typeSwitchGuardName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardWithExpression :: Typed.TypedTerm Syntax.TypeSwitchGuard -> Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.TypeSwitchGuard
typeSwitchGuardWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardWithName :: Typed.TypedTerm Syntax.TypeSwitchGuard -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.TypeSwitchGuard
typeSwitchGuardWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.TypeSwitchStmt
typeSwitchStmt :: Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.TypeSwitchGuard -> Typed.TypedTerm [Syntax.TypeCaseClause] -> Typed.TypedTerm Syntax.TypeSwitchStmt
typeSwitchStmt init guard cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Typed.unTypedTerm guard)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtCases :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm [Syntax.TypeCaseClause]
typeSwitchStmtCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the guard field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtGuard :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm Syntax.TypeSwitchGuard
typeSwitchStmtGuard x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
        Core.projectionFieldName = (Core.Name "guard")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtInit :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm (Maybe Syntax.SimpleStmt)
typeSwitchStmtInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtWithCases :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm [Syntax.TypeCaseClause] -> Typed.TypedTerm Syntax.TypeSwitchStmt
typeSwitchStmtWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the guard field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtWithGuard :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm Syntax.TypeSwitchGuard -> Typed.TypedTerm Syntax.TypeSwitchStmt
typeSwitchStmtWithGuard original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the init field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtWithInit :: Typed.TypedTerm Syntax.TypeSwitchStmt -> Typed.TypedTerm (Maybe Syntax.SimpleStmt) -> Typed.TypedTerm Syntax.TypeSwitchStmt
typeSwitchStmtWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.TypeTerm
typeTerm :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeTerm
typeTerm underlying type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "underlying"),
          Core.fieldTerm = (Typed.unTypedTerm underlying)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the type field of hydra.go.syntax.TypeTerm
typeTermType :: Typed.TypedTerm Syntax.TypeTerm -> Typed.TypedTerm Syntax.Type
typeTermType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the underlying field of hydra.go.syntax.TypeTerm
typeTermUnderlying :: Typed.TypedTerm Syntax.TypeTerm -> Typed.TypedTerm Bool
typeTermUnderlying x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
        Core.projectionFieldName = (Core.Name "underlying")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the type field of hydra.go.syntax.TypeTerm
typeTermWithType :: Typed.TypedTerm Syntax.TypeTerm -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeTerm
typeTermWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "underlying"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
              Core.projectionFieldName = (Core.Name "underlying")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the underlying field of hydra.go.syntax.TypeTerm
typeTermWithUnderlying :: Typed.TypedTerm Syntax.TypeTerm -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.TypeTerm
typeTermWithUnderlying original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "underlying"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the body of hydra.go.syntax.Block
unBlock :: Typed.TypedTerm Syntax.Block -> Typed.TypedTerm [Syntax.Statement]
unBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Block")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.BreakStmt
unBreakStmt :: Typed.TypedTerm Syntax.BreakStmt -> Typed.TypedTerm (Maybe Syntax.Identifier)
unBreakStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.BreakStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ConstDecl
unConstDecl :: Typed.TypedTerm Syntax.ConstDecl -> Typed.TypedTerm [Syntax.ConstSpec]
unConstDecl x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ConstDecl")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ContinueStmt
unContinueStmt :: Typed.TypedTerm Syntax.ContinueStmt -> Typed.TypedTerm (Maybe Syntax.Identifier)
unContinueStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ContinueStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.DeferStmt
unDeferStmt :: Typed.TypedTerm Syntax.DeferStmt -> Typed.TypedTerm Syntax.Expression
unDeferStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.DeferStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ElementList
unElementList :: Typed.TypedTerm Syntax.ElementList -> Typed.TypedTerm [Syntax.KeyedElement]
unElementList x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ElementList")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.EmptyStmt
unEmptyStmt :: Typed.TypedTerm Syntax.EmptyStmt -> Typed.TypedTerm ()
unEmptyStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.EmptyStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ExpressionStmt
unExpressionStmt :: Typed.TypedTerm Syntax.ExpressionStmt -> Typed.TypedTerm Syntax.Expression
unExpressionStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ExpressionStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FallthroughStmt
unFallthroughStmt :: Typed.TypedTerm Syntax.FallthroughStmt -> Typed.TypedTerm ()
unFallthroughStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FallthroughStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FloatLit
unFloatLit :: Typed.TypedTerm Syntax.FloatLit -> Typed.TypedTerm Double
unFloatLit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FloatLit")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FunctionBody
unFunctionBody :: Typed.TypedTerm Syntax.FunctionBody -> Typed.TypedTerm Syntax.Block
unFunctionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FunctionBody")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FunctionType
unFunctionType :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Signature
unFunctionType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FunctionType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.GoStmt
unGoStmt :: Typed.TypedTerm Syntax.GoStmt -> Typed.TypedTerm Syntax.Expression
unGoStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.GoStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.GotoStmt
unGotoStmt :: Typed.TypedTerm Syntax.GotoStmt -> Typed.TypedTerm Syntax.Identifier
unGotoStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.GotoStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Identifier
unIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm String
unIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Identifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ImaginaryLit
unImaginaryLit :: Typed.TypedTerm Syntax.ImaginaryLit -> Typed.TypedTerm Double
unImaginaryLit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ImaginaryLit")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ImportDecl
unImportDecl :: Typed.TypedTerm Syntax.ImportDecl -> Typed.TypedTerm [Syntax.ImportSpec]
unImportDecl x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ImportDecl")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ImportPath
unImportPath :: Typed.TypedTerm Syntax.ImportPath -> Typed.TypedTerm Syntax.StringLit
unImportPath x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ImportPath")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Index
unIndex :: Typed.TypedTerm Syntax.Index -> Typed.TypedTerm [Syntax.Expression]
unIndex x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Index")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.IntLit
unIntLit :: Typed.TypedTerm Syntax.IntLit -> Typed.TypedTerm Integer
unIntLit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.IntLit")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.InterfaceType
unInterfaceType :: Typed.TypedTerm Syntax.InterfaceType -> Typed.TypedTerm [Syntax.InterfaceElem]
unInterfaceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.InterfaceType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.InterpretedStringLit
unInterpretedStringLit :: Typed.TypedTerm Syntax.InterpretedStringLit -> Typed.TypedTerm String
unInterpretedStringLit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.InterpretedStringLit")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.LiteralValue
unLiteralValue :: Typed.TypedTerm Syntax.LiteralValue -> Typed.TypedTerm [Syntax.KeyedElement]
unLiteralValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.LiteralValue")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.PackageClause
unPackageClause :: Typed.TypedTerm Syntax.PackageClause -> Typed.TypedTerm Syntax.Identifier
unPackageClause x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.PackageClause")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Parameters
unParameters :: Typed.TypedTerm Syntax.Parameters -> Typed.TypedTerm [Syntax.ParameterDecl]
unParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Parameters")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.PointerType
unPointerType :: Typed.TypedTerm Syntax.PointerType -> Typed.TypedTerm Syntax.Type
unPointerType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.PointerType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.RawStringLit
unRawStringLit :: Typed.TypedTerm Syntax.RawStringLit -> Typed.TypedTerm String
unRawStringLit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.RawStringLit")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ReturnStmt
unReturnStmt :: Typed.TypedTerm Syntax.ReturnStmt -> Typed.TypedTerm [Syntax.Expression]
unReturnStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ReturnStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.RuneLit
unRuneLit :: Typed.TypedTerm Syntax.RuneLit -> Typed.TypedTerm Int
unRuneLit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.RuneLit")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.SelectStmt
unSelectStmt :: Typed.TypedTerm Syntax.SelectStmt -> Typed.TypedTerm [Syntax.CommClause]
unSelectStmt x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.SelectStmt")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Selector
unSelector :: Typed.TypedTerm Syntax.Selector -> Typed.TypedTerm Syntax.Identifier
unSelector x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Selector")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.SliceType
unSliceType :: Typed.TypedTerm Syntax.SliceType -> Typed.TypedTerm Syntax.Type
unSliceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.SliceType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.StructType
unStructType :: Typed.TypedTerm Syntax.StructType -> Typed.TypedTerm [Syntax.FieldDecl]
unStructType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.StructType")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Tag
unTag :: Typed.TypedTerm Syntax.Tag -> Typed.TypedTerm Syntax.StringLit
unTag x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Tag")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeAssertion
unTypeAssertion :: Typed.TypedTerm Syntax.TypeAssertion -> Typed.TypedTerm Syntax.Type
unTypeAssertion x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeAssertion")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeConstraint
unTypeConstraint :: Typed.TypedTerm Syntax.TypeConstraint -> Typed.TypedTerm Syntax.TypeElem
unTypeConstraint x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeConstraint")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeDecl
unTypeDecl :: Typed.TypedTerm Syntax.TypeDecl -> Typed.TypedTerm [Syntax.TypeSpec]
unTypeDecl x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeDecl")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeElem
unTypeElem :: Typed.TypedTerm Syntax.TypeElem -> Typed.TypedTerm [Syntax.TypeTerm]
unTypeElem x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeElem")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeParameters
unTypeParameters :: Typed.TypedTerm Syntax.TypeParameters -> Typed.TypedTerm [Syntax.TypeParamDecl]
unTypeParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeParameters")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.VarDecl
unVarDecl :: Typed.TypedTerm Syntax.VarDecl -> Typed.TypedTerm [Syntax.VarSpec]
unVarDecl x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.VarDecl")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the op variant of hydra.go.syntax.UnaryExpr
unaryExprOp :: Typed.TypedTerm Syntax.UnaryOperation -> Typed.TypedTerm Syntax.UnaryExpr
unaryExprOp x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "op"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primary variant of hydra.go.syntax.UnaryExpr
unaryExprPrimary :: Typed.TypedTerm Syntax.PrimaryExpr -> Typed.TypedTerm Syntax.UnaryExpr
unaryExprPrimary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the addressOf variant of hydra.go.syntax.UnaryOp
unaryOpAddressOf :: Typed.TypedTerm Syntax.UnaryOp
unaryOpAddressOf =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addressOf"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the deref variant of hydra.go.syntax.UnaryOp
unaryOpDeref :: Typed.TypedTerm Syntax.UnaryOp
unaryOpDeref =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deref"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minus variant of hydra.go.syntax.UnaryOp
unaryOpMinus :: Typed.TypedTerm Syntax.UnaryOp
unaryOpMinus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the not variant of hydra.go.syntax.UnaryOp
unaryOpNot :: Typed.TypedTerm Syntax.UnaryOp
unaryOpNot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.go.syntax.UnaryOp
unaryOpPlus :: Typed.TypedTerm Syntax.UnaryOp
unaryOpPlus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the receive variant of hydra.go.syntax.UnaryOp
unaryOpReceive :: Typed.TypedTerm Syntax.UnaryOp
unaryOpReceive =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "receive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the xor variant of hydra.go.syntax.UnaryOp
unaryOpXor :: Typed.TypedTerm Syntax.UnaryOp
unaryOpXor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.UnaryOperation
unaryOperation :: Typed.TypedTerm Syntax.UnaryOp -> Typed.TypedTerm Syntax.UnaryExpr -> Typed.TypedTerm Syntax.UnaryOperation
unaryOperation op operand =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm operand)}]}))
-- | DSL accessor for the op field of hydra.go.syntax.UnaryOperation
unaryOperationOp :: Typed.TypedTerm Syntax.UnaryOperation -> Typed.TypedTerm Syntax.UnaryOp
unaryOperationOp x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operand field of hydra.go.syntax.UnaryOperation
unaryOperationOperand :: Typed.TypedTerm Syntax.UnaryOperation -> Typed.TypedTerm Syntax.UnaryExpr
unaryOperationOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the op field of hydra.go.syntax.UnaryOperation
unaryOperationWithOp :: Typed.TypedTerm Syntax.UnaryOperation -> Typed.TypedTerm Syntax.UnaryOp -> Typed.TypedTerm Syntax.UnaryOperation
unaryOperationWithOp original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operand field of hydra.go.syntax.UnaryOperation
unaryOperationWithOperand :: Typed.TypedTerm Syntax.UnaryOperation -> Typed.TypedTerm Syntax.UnaryExpr -> Typed.TypedTerm Syntax.UnaryOperation
unaryOperationWithOperand original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.VarDecl wrapper
varDecl :: Typed.TypedTerm [Syntax.VarSpec] -> Typed.TypedTerm Syntax.VarDecl
varDecl x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.VarDecl"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.go.syntax.VarSpec
varSpec :: Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.VarSpec
varSpec names type_ values =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm values)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.VarSpec
varSpecNames :: Typed.TypedTerm Syntax.VarSpec -> Typed.TypedTerm [Syntax.Identifier]
varSpecNames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.VarSpec
varSpecType :: Typed.TypedTerm Syntax.VarSpec -> Typed.TypedTerm (Maybe Syntax.Type)
varSpecType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the values field of hydra.go.syntax.VarSpec
varSpecValues :: Typed.TypedTerm Syntax.VarSpec -> Typed.TypedTerm [Syntax.Expression]
varSpecValues x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.VarSpec
varSpecWithNames :: Typed.TypedTerm Syntax.VarSpec -> Typed.TypedTerm [Syntax.Identifier] -> Typed.TypedTerm Syntax.VarSpec
varSpecWithNames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.VarSpec
varSpecWithType :: Typed.TypedTerm Syntax.VarSpec -> Typed.TypedTerm (Maybe Syntax.Type) -> Typed.TypedTerm Syntax.VarSpec
varSpecWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the values field of hydra.go.syntax.VarSpec
varSpecWithValues :: Typed.TypedTerm Syntax.VarSpec -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.VarSpec
varSpecWithValues original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
