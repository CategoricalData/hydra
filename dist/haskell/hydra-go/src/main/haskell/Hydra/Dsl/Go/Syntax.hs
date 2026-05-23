-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.go.syntax

module Hydra.Dsl.Go.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Go.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the add variant of hydra.go.syntax.AddOp
addOpAdd :: Phantoms.TTerm Syntax.AddOp
addOpAdd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseOr variant of hydra.go.syntax.AddOp
addOpBitwiseOr :: Phantoms.TTerm Syntax.AddOp
addOpBitwiseOr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseXor variant of hydra.go.syntax.AddOp
addOpBitwiseXor :: Phantoms.TTerm Syntax.AddOp
addOpBitwiseXor =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the subtract variant of hydra.go.syntax.AddOp
addOpSubtract :: Phantoms.TTerm Syntax.AddOp
addOpSubtract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AddOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.AliasDecl
aliasDecl :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AliasDecl
aliasDecl name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.AliasDecl
aliasDeclName :: Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Identifier
aliasDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.AliasDecl
aliasDeclType :: Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Type
aliasDeclType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.AliasDecl
aliasDeclWithName :: Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.AliasDecl
aliasDeclWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.AliasDecl
aliasDeclWithType :: Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.AliasDecl
aliasDeclWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AliasDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.AnnotatedDeclaration
annotatedDeclaration :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.TopLevelDecl -> Phantoms.TTerm Syntax.AnnotatedDeclaration
annotatedDeclaration comment declaration =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm comment)},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Phantoms.unTTerm declaration)}]}))
-- | DSL accessor for the comment field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationComment :: Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm String
annotatedDeclarationComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the declaration field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationDeclaration :: Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm Syntax.TopLevelDecl
annotatedDeclarationDeclaration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
        Core.projectionFieldName = (Core.Name "declaration")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comment field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationWithComment :: Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.AnnotatedDeclaration
annotatedDeclarationWithComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
              Core.projectionFieldName = (Core.Name "declaration")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the declaration field of hydra.go.syntax.AnnotatedDeclaration
annotatedDeclarationWithDeclaration :: Phantoms.TTerm Syntax.AnnotatedDeclaration -> Phantoms.TTerm Syntax.TopLevelDecl -> Phantoms.TTerm Syntax.AnnotatedDeclaration
annotatedDeclarationWithDeclaration original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.AnnotatedDeclaration"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.Arguments
arguments :: Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Arguments
arguments typeArg expressions ellipsis =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Phantoms.unTTerm typeArg)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Phantoms.unTTerm ellipsis)}]}))
-- | DSL accessor for the ellipsis field of hydra.go.syntax.Arguments
argumentsEllipsis :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Bool
argumentsEllipsis x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
        Core.projectionFieldName = (Core.Name "ellipsis")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expressions field of hydra.go.syntax.Arguments
argumentsExpressions :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm [Syntax.Expression]
argumentsExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArg field of hydra.go.syntax.Arguments
argumentsTypeArg :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm (Maybe Syntax.Type)
argumentsTypeArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
        Core.projectionFieldName = (Core.Name "typeArg")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ellipsis field of hydra.go.syntax.Arguments
argumentsWithEllipsis :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Arguments
argumentsWithEllipsis original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "typeArg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expressions field of hydra.go.syntax.Arguments
argumentsWithExpressions :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Arguments
argumentsWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "typeArg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "ellipsis")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArg field of hydra.go.syntax.Arguments
argumentsWithTypeArg :: Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.Arguments
argumentsWithTypeArg original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Arguments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeArg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ellipsis"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Arguments"),
              Core.projectionFieldName = (Core.Name "ellipsis")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.ArrayType
arrayType :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ArrayType
arrayType length element =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm length)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)}]}))
-- | DSL accessor for the element field of hydra.go.syntax.ArrayType
arrayTypeElement :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type
arrayTypeElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the length field of hydra.go.syntax.ArrayType
arrayTypeLength :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Expression
arrayTypeLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
        Core.projectionFieldName = (Core.Name "length")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the element field of hydra.go.syntax.ArrayType
arrayTypeWithElement :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
              Core.projectionFieldName = (Core.Name "length")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the length field of hydra.go.syntax.ArrayType
arrayTypeWithLength :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayType
arrayTypeWithLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ArrayType"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the add variant of hydra.go.syntax.AssignOp
assignOpAdd :: Phantoms.TTerm Syntax.AddOp -> Phantoms.TTerm Syntax.AssignOp
assignOpAdd x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the mul variant of hydra.go.syntax.AssignOp
assignOpMul :: Phantoms.TTerm Syntax.MulOp -> Phantoms.TTerm Syntax.AssignOp
assignOpMul x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mul"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.go.syntax.AssignOp
assignOpSimple :: Phantoms.TTerm Syntax.AssignOp
assignOpSimple =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.AssignOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.Assignment
assignment :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.AssignOp -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Assignment
assignment lhs op rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.go.syntax.Assignment
assignmentLhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression]
assignmentLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.go.syntax.Assignment
assignmentOp :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignOp
assignmentOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.go.syntax.Assignment
assignmentRhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression]
assignmentRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.go.syntax.Assignment
assignmentWithLhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Assignment
assignmentWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.go.syntax.Assignment
assignmentWithOp :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.AssignOp -> Phantoms.TTerm Syntax.Assignment
assignmentWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.go.syntax.Assignment
assignmentWithRhs :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Assignment
assignmentWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Assignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Assignment"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the float variant of hydra.go.syntax.BasicLit
basicLitFloat :: Phantoms.TTerm Syntax.FloatLit -> Phantoms.TTerm Syntax.BasicLit
basicLitFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the imaginary variant of hydra.go.syntax.BasicLit
basicLitImaginary :: Phantoms.TTerm Syntax.ImaginaryLit -> Phantoms.TTerm Syntax.BasicLit
basicLitImaginary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "imaginary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int variant of hydra.go.syntax.BasicLit
basicLitInt :: Phantoms.TTerm Syntax.IntLit -> Phantoms.TTerm Syntax.BasicLit
basicLitInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the rune variant of hydra.go.syntax.BasicLit
basicLitRune :: Phantoms.TTerm Syntax.RuneLit -> Phantoms.TTerm Syntax.BasicLit
basicLitRune x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rune"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.go.syntax.BasicLit
basicLitString :: Phantoms.TTerm Syntax.StringLit -> Phantoms.TTerm Syntax.BasicLit
basicLitString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BasicLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.BinaryExpr
binaryExpr :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryOp -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr
binaryExpr left op right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))
-- | DSL accessor for the left field of hydra.go.syntax.BinaryExpr
binaryExprLeft :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression
binaryExprLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the op field of hydra.go.syntax.BinaryExpr
binaryExprOp :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.BinaryOp
binaryExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the right field of hydra.go.syntax.BinaryExpr
binaryExprRight :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression
binaryExprRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the left field of hydra.go.syntax.BinaryExpr
binaryExprWithLeft :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr
binaryExprWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the op field of hydra.go.syntax.BinaryExpr
binaryExprWithOp :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.BinaryOp -> Phantoms.TTerm Syntax.BinaryExpr
binaryExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the right field of hydra.go.syntax.BinaryExpr
binaryExprWithRight :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpr
binaryExprWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.BinaryExpr"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the add variant of hydra.go.syntax.BinaryOp
binaryOpAdd :: Phantoms.TTerm Syntax.BinaryOp
binaryOpAdd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the and variant of hydra.go.syntax.BinaryOp
binaryOpAnd :: Phantoms.TTerm Syntax.BinaryOp
binaryOpAnd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitClear variant of hydra.go.syntax.BinaryOp
binaryOpBitClear :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitClear =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitClear"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseAnd variant of hydra.go.syntax.BinaryOp
binaryOpBitwiseAnd :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitwiseAnd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseOr variant of hydra.go.syntax.BinaryOp
binaryOpBitwiseOr :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitwiseOr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseXor variant of hydra.go.syntax.BinaryOp
binaryOpBitwiseXor :: Phantoms.TTerm Syntax.BinaryOp
binaryOpBitwiseXor =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the divide variant of hydra.go.syntax.BinaryOp
binaryOpDivide :: Phantoms.TTerm Syntax.BinaryOp
binaryOpDivide =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the equal variant of hydra.go.syntax.BinaryOp
binaryOpEqual :: Phantoms.TTerm Syntax.BinaryOp
binaryOpEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greater variant of hydra.go.syntax.BinaryOp
binaryOpGreater :: Phantoms.TTerm Syntax.BinaryOp
binaryOpGreater =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterEqual variant of hydra.go.syntax.BinaryOp
binaryOpGreaterEqual :: Phantoms.TTerm Syntax.BinaryOp
binaryOpGreaterEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShift variant of hydra.go.syntax.BinaryOp
binaryOpLeftShift :: Phantoms.TTerm Syntax.BinaryOp
binaryOpLeftShift =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the less variant of hydra.go.syntax.BinaryOp
binaryOpLess :: Phantoms.TTerm Syntax.BinaryOp
binaryOpLess =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessEqual variant of hydra.go.syntax.BinaryOp
binaryOpLessEqual :: Phantoms.TTerm Syntax.BinaryOp
binaryOpLessEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the multiply variant of hydra.go.syntax.BinaryOp
binaryOpMultiply :: Phantoms.TTerm Syntax.BinaryOp
binaryOpMultiply =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.go.syntax.BinaryOp
binaryOpNotEqual :: Phantoms.TTerm Syntax.BinaryOp
binaryOpNotEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the or variant of hydra.go.syntax.BinaryOp
binaryOpOr :: Phantoms.TTerm Syntax.BinaryOp
binaryOpOr =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the remainder variant of hydra.go.syntax.BinaryOp
binaryOpRemainder :: Phantoms.TTerm Syntax.BinaryOp
binaryOpRemainder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remainder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShift variant of hydra.go.syntax.BinaryOp
binaryOpRightShift :: Phantoms.TTerm Syntax.BinaryOp
binaryOpRightShift =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the subtract variant of hydra.go.syntax.BinaryOp
binaryOpSubtract :: Phantoms.TTerm Syntax.BinaryOp
binaryOpSubtract =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.BinaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.go.syntax.Block wrapper
block :: Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.Block
block x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Block"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.BreakStmt wrapper
breakStmt :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.BreakStmt
breakStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.BreakStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.CallExpr
callExpr :: Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Syntax.CallExpr
callExpr function arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.go.syntax.CallExpr
callExprArguments :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Arguments
callExprArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the function field of hydra.go.syntax.CallExpr
callExprFunction :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.PrimaryExpr
callExprFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the arguments field of hydra.go.syntax.CallExpr
callExprWithArguments :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.Arguments -> Phantoms.TTerm Syntax.CallExpr
callExprWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the function field of hydra.go.syntax.CallExpr
callExprWithFunction :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.CallExpr
callExprWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CallExpr"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the bidirectional variant of hydra.go.syntax.ChannelDirection
channelDirectionBidirectional :: Phantoms.TTerm Syntax.ChannelDirection
channelDirectionBidirectional =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ChannelDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bidirectional"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the receive variant of hydra.go.syntax.ChannelDirection
channelDirectionReceive :: Phantoms.TTerm Syntax.ChannelDirection
channelDirectionReceive =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ChannelDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "receive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the send variant of hydra.go.syntax.ChannelDirection
channelDirectionSend :: Phantoms.TTerm Syntax.ChannelDirection
channelDirectionSend =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ChannelDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "send"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.ChannelType
channelType :: Phantoms.TTerm Syntax.ChannelDirection -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ChannelType
channelType direction element =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)}]}))
-- | DSL accessor for the direction field of hydra.go.syntax.ChannelType
channelTypeDirection :: Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.ChannelDirection
channelTypeDirection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
        Core.projectionFieldName = (Core.Name "direction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the element field of hydra.go.syntax.ChannelType
channelTypeElement :: Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.Type
channelTypeElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the direction field of hydra.go.syntax.ChannelType
channelTypeWithDirection :: Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.ChannelDirection -> Phantoms.TTerm Syntax.ChannelType
channelTypeWithDirection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the element field of hydra.go.syntax.ChannelType
channelTypeWithElement :: Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ChannelType
channelTypeWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ChannelType"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the default variant of hydra.go.syntax.CommCase
commCaseDefault :: Phantoms.TTerm Syntax.CommCase
commCaseDefault =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.CommCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the receive variant of hydra.go.syntax.CommCase
commCaseReceive :: Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm Syntax.CommCase
commCaseReceive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.CommCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "receive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the send variant of hydra.go.syntax.CommCase
commCaseSend :: Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.CommCase
commCaseSend x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.CommCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "send"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.CommClause
commClause :: Phantoms.TTerm Syntax.CommCase -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.CommClause
commClause case_ statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CommClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))
-- | DSL accessor for the case field of hydra.go.syntax.CommClause
commClauseCase :: Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm Syntax.CommCase
commClauseCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statements field of hydra.go.syntax.CommClause
commClauseStatements :: Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm [Syntax.Statement]
commClauseStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the case field of hydra.go.syntax.CommClause
commClauseWithCase :: Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm Syntax.CommCase -> Phantoms.TTerm Syntax.CommClause
commClauseWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CommClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.go.syntax.CommClause
commClauseWithStatements :: Phantoms.TTerm Syntax.CommClause -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.CommClause
commClauseWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CommClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CommClause"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.CompositeLit
compositeLit :: Phantoms.TTerm Syntax.LiteralType -> Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.CompositeLit
compositeLit type_ value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the type field of hydra.go.syntax.CompositeLit
compositeLitType :: Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralType
compositeLitType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.go.syntax.CompositeLit
compositeLitValue :: Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralValue
compositeLitValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the type field of hydra.go.syntax.CompositeLit
compositeLitWithType :: Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralType -> Phantoms.TTerm Syntax.CompositeLit
compositeLitWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.go.syntax.CompositeLit
compositeLitWithValue :: Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.CompositeLit
compositeLitWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.CompositeLit"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.ConstDecl wrapper
constDecl :: Phantoms.TTerm [Syntax.ConstSpec] -> Phantoms.TTerm Syntax.ConstDecl
constDecl x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ConstDecl"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.ConstSpec
constSpec :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ConstSpec
constSpec names type_ values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.ConstSpec
constSpecNames :: Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Identifier]
constSpecNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.ConstSpec
constSpecType :: Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm (Maybe Syntax.Type)
constSpecType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the values field of hydra.go.syntax.ConstSpec
constSpecValues :: Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Expression]
constSpecValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.ConstSpec
constSpecWithNames :: Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ConstSpec
constSpecWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.ConstSpec
constSpecWithType :: Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.ConstSpec
constSpecWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the values field of hydra.go.syntax.ConstSpec
constSpecWithValues :: Phantoms.TTerm Syntax.ConstSpec -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ConstSpec
constSpecWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ConstSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.ContinueStmt wrapper
continueStmt :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ContinueStmt
continueStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ContinueStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.Conversion
conversion :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conversion
conversion type_ expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Conversion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.Conversion
conversionExpression :: Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Expression
conversionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.Conversion
conversionType :: Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Type
conversionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.Conversion
conversionWithExpression :: Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Conversion
conversionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Conversion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.go.syntax.Conversion
conversionWithType :: Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Conversion
conversionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Conversion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Conversion"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the const variant of hydra.go.syntax.Declaration
declarationConst :: Phantoms.TTerm Syntax.ConstDecl -> Phantoms.TTerm Syntax.Declaration
declarationConst x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.Declaration
declarationType :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm Syntax.Declaration
declarationType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the var variant of hydra.go.syntax.Declaration
declarationVar :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm Syntax.Declaration
declarationVar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.DeferStmt wrapper
deferStmt :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DeferStmt
deferStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.DeferStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the expression variant of hydra.go.syntax.Element
elementExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Element
elementExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ElementList wrapper
elementList :: Phantoms.TTerm [Syntax.KeyedElement] -> Phantoms.TTerm Syntax.ElementList
elementList x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ElementList"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the literal variant of hydra.go.syntax.Element
elementLiteral :: Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.Element
elementLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the block variant of hydra.go.syntax.ElseClause
elseClauseBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ElseClause
elseClauseBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ElseClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the if variant of hydra.go.syntax.ElseClause
elseClauseIf :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.ElseClause
elseClauseIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ElseClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.EmbeddedField
embeddedField :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.EmbeddedField
embeddedField pointer type_ tag =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Phantoms.unTTerm pointer)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Phantoms.unTTerm tag)}]}))
-- | DSL accessor for the pointer field of hydra.go.syntax.EmbeddedField
embeddedFieldPointer :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Bool
embeddedFieldPointer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
        Core.projectionFieldName = (Core.Name "pointer")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tag field of hydra.go.syntax.EmbeddedField
embeddedFieldTag :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm (Maybe Syntax.Tag)
embeddedFieldTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
        Core.projectionFieldName = (Core.Name "tag")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.EmbeddedField
embeddedFieldType :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Syntax.TypeName
embeddedFieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the pointer field of hydra.go.syntax.EmbeddedField
embeddedFieldWithPointer :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.EmbeddedField
embeddedFieldWithPointer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tag field of hydra.go.syntax.EmbeddedField
embeddedFieldWithTag :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.EmbeddedField
embeddedFieldWithTag original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "pointer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.go.syntax.EmbeddedField
embeddedFieldWithType :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.EmbeddedField
embeddedFieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pointer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "pointer")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.EmbeddedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.EmptyStmt wrapper
emptyStmt :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.EmptyStmt
emptyStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.EmptyStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.ExprCaseClause
exprCaseClause :: Phantoms.TTerm (Maybe [Syntax.Expression]) -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.ExprCaseClause
exprCaseClause case_ statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))
-- | DSL accessor for the case field of hydra.go.syntax.ExprCaseClause
exprCaseClauseCase :: Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm (Maybe [Syntax.Expression])
exprCaseClauseCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statements field of hydra.go.syntax.ExprCaseClause
exprCaseClauseStatements :: Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm [Syntax.Statement]
exprCaseClauseStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the case field of hydra.go.syntax.ExprCaseClause
exprCaseClauseWithCase :: Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm (Maybe [Syntax.Expression]) -> Phantoms.TTerm Syntax.ExprCaseClause
exprCaseClauseWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.go.syntax.ExprCaseClause
exprCaseClauseWithStatements :: Phantoms.TTerm Syntax.ExprCaseClause -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.ExprCaseClause
exprCaseClauseWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprCaseClause"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.ExprSwitchStmt
exprSwitchStmt :: Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm [Syntax.ExprCaseClause] -> Phantoms.TTerm Syntax.ExprSwitchStmt
exprSwitchStmt init expression cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtCases :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm [Syntax.ExprCaseClause]
exprSwitchStmtCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the expression field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtExpression :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.Expression)
exprSwitchStmtExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtInit :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt)
exprSwitchStmtInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtWithCases :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm [Syntax.ExprCaseClause] -> Phantoms.TTerm Syntax.ExprSwitchStmt
exprSwitchStmtWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the expression field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtWithExpression :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ExprSwitchStmt
exprSwitchStmtWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the init field of hydra.go.syntax.ExprSwitchStmt
exprSwitchStmtWithInit :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ExprSwitchStmt
exprSwitchStmtWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ExprSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the binary variant of hydra.go.syntax.Expression
expressionBinary :: Phantoms.TTerm Syntax.BinaryExpr -> Phantoms.TTerm Syntax.Expression
expressionBinary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ExpressionStmt wrapper
expressionStmt :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExpressionStmt
expressionStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ExpressionStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the unary variant of hydra.go.syntax.Expression
expressionUnary :: Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.Expression
expressionUnary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.FallthroughStmt wrapper
fallthroughStmt :: Phantoms.TTerm () -> Phantoms.TTerm Syntax.FallthroughStmt
fallthroughStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FallthroughStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the embedded variant of hydra.go.syntax.FieldDecl
fieldDeclEmbedded :: Phantoms.TTerm Syntax.EmbeddedField -> Phantoms.TTerm Syntax.FieldDecl
fieldDeclEmbedded x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.FieldDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "embedded"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the named variant of hydra.go.syntax.FieldDecl
fieldDeclNamed :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm Syntax.FieldDecl
fieldDeclNamed x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.FieldDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.FloatLit wrapper
floatLit :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.FloatLit
floatLit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FloatLit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.ForClause
forClause :: Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ForClause
forClause init condition post =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Phantoms.unTTerm post)}]}))
-- | DSL accessor for the condition field of hydra.go.syntax.ForClause
forClauseCondition :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.Expression)
forClauseCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.ForClause
forClauseInit :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt)
forClauseInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the clause variant of hydra.go.syntax.ForClauseOrRange
forClauseOrRangeClause :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm Syntax.ForClauseOrRange
forClauseOrRangeClause x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ForClauseOrRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "clause"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the condition variant of hydra.go.syntax.ForClauseOrRange
forClauseOrRangeCondition :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForClauseOrRange
forClauseOrRangeCondition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ForClauseOrRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "condition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the range variant of hydra.go.syntax.ForClauseOrRange
forClauseOrRangeRange :: Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm Syntax.ForClauseOrRange
forClauseOrRangeRange x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ForClauseOrRange"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL accessor for the post field of hydra.go.syntax.ForClause
forClausePost :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt)
forClausePost x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
        Core.projectionFieldName = (Core.Name "post")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the condition field of hydra.go.syntax.ForClause
forClauseWithCondition :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ForClause
forClauseWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "post")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the init field of hydra.go.syntax.ForClause
forClauseWithInit :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ForClause
forClauseWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "post")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the post field of hydra.go.syntax.ForClause
forClauseWithPost :: Phantoms.TTerm Syntax.ForClause -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.ForClause
forClauseWithPost original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForClause"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "post"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.ForStmt
forStmt :: Phantoms.TTerm (Maybe Syntax.ForClauseOrRange) -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForStmt
forStmt clause body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clause"),
          Core.fieldTerm = (Phantoms.unTTerm clause)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.ForStmt
forStmtBody :: Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm Syntax.Block
forStmtBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the clause field of hydra.go.syntax.ForStmt
forStmtClause :: Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm (Maybe Syntax.ForClauseOrRange)
forStmtClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
        Core.projectionFieldName = (Core.Name "clause")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.ForStmt
forStmtWithBody :: Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.ForStmt
forStmtWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clause"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
              Core.projectionFieldName = (Core.Name "clause")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the clause field of hydra.go.syntax.ForStmt
forStmtWithClause :: Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm (Maybe Syntax.ForClauseOrRange) -> Phantoms.TTerm Syntax.ForStmt
forStmtWithClause original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "clause"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ForStmt"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.FullSlice
fullSlice :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FullSlice
fullSlice low high max =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Phantoms.unTTerm low)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Phantoms.unTTerm high)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm max)}]}))
-- | DSL accessor for the high field of hydra.go.syntax.FullSlice
fullSliceHigh :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression
fullSliceHigh x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
        Core.projectionFieldName = (Core.Name "high")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the low field of hydra.go.syntax.FullSlice
fullSliceLow :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm (Maybe Syntax.Expression)
fullSliceLow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
        Core.projectionFieldName = (Core.Name "low")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the max field of hydra.go.syntax.FullSlice
fullSliceMax :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression
fullSliceMax x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
        Core.projectionFieldName = (Core.Name "max")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the high field of hydra.go.syntax.FullSlice
fullSliceWithHigh :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FullSlice
fullSliceWithHigh original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "low")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the low field of hydra.go.syntax.FullSlice
fullSliceWithLow :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.FullSlice
fullSliceWithLow original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "high")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "max")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the max field of hydra.go.syntax.FullSlice
fullSliceWithMax :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FullSlice
fullSliceWithMax original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "low")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FullSlice"),
              Core.projectionFieldName = (Core.Name "high")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "max"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.FunctionBody wrapper
functionBody :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.FunctionBody
functionBody x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FunctionBody"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.FunctionDecl
functionDecl :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.FunctionDecl
functionDecl name typeParams signature body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.FunctionDecl
functionDeclBody :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody)
functionDeclBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.go.syntax.FunctionDecl
functionDeclName :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Identifier
functionDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.FunctionDecl
functionDeclSignature :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Signature
functionDeclSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeParams field of hydra.go.syntax.FunctionDecl
functionDeclTypeParams :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.TypeParameters)
functionDeclTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.FunctionDecl
functionDeclWithBody :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.FunctionDecl
functionDeclWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.go.syntax.FunctionDecl
functionDeclWithName :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FunctionDecl
functionDeclWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.go.syntax.FunctionDecl
functionDeclWithSignature :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionDecl
functionDeclWithSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.go.syntax.FunctionDecl
functionDeclWithTypeParams :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.FunctionDecl
functionDeclWithTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.FunctionLit
functionLit :: Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.FunctionLit
functionLit signature body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.FunctionLit
functionLitBody :: Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.FunctionBody
functionLitBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.FunctionLit
functionLitSignature :: Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.Signature
functionLitSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.FunctionLit
functionLitWithBody :: Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.FunctionLit
functionLitWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the signature field of hydra.go.syntax.FunctionLit
functionLitWithSignature :: Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionLit
functionLitWithSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.FunctionLit"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.FunctionType wrapper
functionType :: Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.FunctionType
functionType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.FunctionType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.GoStmt wrapper
goStmt :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.GoStmt
goStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.GoStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.GotoStmt wrapper
gotoStmt :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.GotoStmt
gotoStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.GotoStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.Identifier wrapper
identifier :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Identifier
identifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Identifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.IfStmt
ifStmt :: Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm (Maybe Syntax.ElseClause) -> Phantoms.TTerm Syntax.IfStmt
ifStmt init condition then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm then_)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))
-- | DSL accessor for the condition field of hydra.go.syntax.IfStmt
ifStmtCondition :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Expression
ifStmtCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.go.syntax.IfStmt
ifStmtElse :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.ElseClause)
ifStmtElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.IfStmt
ifStmtInit :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt)
ifStmtInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the then field of hydra.go.syntax.IfStmt
ifStmtThen :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Block
ifStmtThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the condition field of hydra.go.syntax.IfStmt
ifStmtWithCondition :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfStmt
ifStmtWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.go.syntax.IfStmt
ifStmtWithElse :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.ElseClause) -> Phantoms.TTerm Syntax.IfStmt
ifStmtWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the init field of hydra.go.syntax.IfStmt
ifStmtWithInit :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.IfStmt
ifStmtWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the then field of hydra.go.syntax.IfStmt
ifStmtWithThen :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.IfStmt
ifStmtWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IfStmt"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.ImaginaryLit wrapper
imaginaryLit :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.ImaginaryLit
imaginaryLit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ImaginaryLit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the dot variant of hydra.go.syntax.ImportAlias
importAliasDot :: Phantoms.TTerm Syntax.ImportAlias
importAliasDot =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ImportAlias"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the name variant of hydra.go.syntax.ImportAlias
importAliasName :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportAlias
importAliasName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.ImportAlias"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ImportDecl wrapper
importDecl :: Phantoms.TTerm [Syntax.ImportSpec] -> Phantoms.TTerm Syntax.ImportDecl
importDecl x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ImportDecl"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.ImportPath wrapper
importPath :: Phantoms.TTerm Syntax.StringLit -> Phantoms.TTerm Syntax.ImportPath
importPath x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ImportPath"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.ImportSpec
importSpec :: Phantoms.TTerm (Maybe Syntax.ImportAlias) -> Phantoms.TTerm Syntax.ImportPath -> Phantoms.TTerm Syntax.ImportSpec
importSpec alias path =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm alias)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm path)}]}))
-- | DSL accessor for the alias field of hydra.go.syntax.ImportSpec
importSpecAlias :: Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm (Maybe Syntax.ImportAlias)
importSpecAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
        Core.projectionFieldName = (Core.Name "alias")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the path field of hydra.go.syntax.ImportSpec
importSpecPath :: Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportPath
importSpecPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
        Core.projectionFieldName = (Core.Name "path")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the alias field of hydra.go.syntax.ImportSpec
importSpecWithAlias :: Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm (Maybe Syntax.ImportAlias) -> Phantoms.TTerm Syntax.ImportSpec
importSpecWithAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
              Core.projectionFieldName = (Core.Name "path")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the path field of hydra.go.syntax.ImportSpec
importSpecWithPath :: Phantoms.TTerm Syntax.ImportSpec -> Phantoms.TTerm Syntax.ImportPath -> Phantoms.TTerm Syntax.ImportSpec
importSpecWithPath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ImportSpec"),
              Core.projectionFieldName = (Core.Name "alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.IncDecStmt
incDecStmt :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IncDecStmt
incDecStmt expression increment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Phantoms.unTTerm increment)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.IncDecStmt
incDecStmtExpression :: Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Syntax.Expression
incDecStmtExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the increment field of hydra.go.syntax.IncDecStmt
incDecStmtIncrement :: Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Bool
incDecStmtIncrement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
        Core.projectionFieldName = (Core.Name "increment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.IncDecStmt
incDecStmtWithExpression :: Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IncDecStmt
incDecStmtWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
              Core.projectionFieldName = (Core.Name "increment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the increment field of hydra.go.syntax.IncDecStmt
incDecStmtWithIncrement :: Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.IncDecStmt
incDecStmtWithIncrement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IncDecStmt"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "increment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.Index wrapper
index :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Index
index x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Index"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.IndexExpr
indexExpr :: Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr
indexExpr expr index =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm index)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.IndexExpr
indexExprExpr :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.PrimaryExpr
indexExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the index field of hydra.go.syntax.IndexExpr
indexExprIndex :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression
indexExprIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
        Core.projectionFieldName = (Core.Name "index")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.IndexExpr
indexExprWithExpr :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.IndexExpr
indexExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
              Core.projectionFieldName = (Core.Name "index")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the index field of hydra.go.syntax.IndexExpr
indexExprWithIndex :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IndexExpr
indexExprWithIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.IndexExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.IntLit wrapper
intLit :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.IntLit
intLit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.IntLit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the method variant of hydra.go.syntax.InterfaceElem
interfaceElemMethod :: Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.InterfaceElem
interfaceElemMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.InterfaceElem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.InterfaceElem
interfaceElemType :: Phantoms.TTerm Syntax.TypeElem -> Phantoms.TTerm Syntax.InterfaceElem
interfaceElemType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.InterfaceElem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.InterfaceType wrapper
interfaceType :: Phantoms.TTerm [Syntax.InterfaceElem] -> Phantoms.TTerm Syntax.InterfaceType
interfaceType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.InterfaceType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.InterpretedStringLit wrapper
interpretedStringLit :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.InterpretedStringLit
interpretedStringLit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.InterpretedStringLit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the expression variant of hydra.go.syntax.Key
keyExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Key
keyExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Key"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the field variant of hydra.go.syntax.Key
keyField :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Key
keyField x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Key"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "field"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.go.syntax.Key
keyLiteral :: Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm Syntax.Key
keyLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Key"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.KeyedElement
keyedElement :: Phantoms.TTerm (Maybe Syntax.Key) -> Phantoms.TTerm Syntax.Element -> Phantoms.TTerm Syntax.KeyedElement
keyedElement key element =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)}]}))
-- | DSL accessor for the element field of hydra.go.syntax.KeyedElement
keyedElementElement :: Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm Syntax.Element
keyedElementElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the key field of hydra.go.syntax.KeyedElement
keyedElementKey :: Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm (Maybe Syntax.Key)
keyedElementKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the element field of hydra.go.syntax.KeyedElement
keyedElementWithElement :: Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm Syntax.Element -> Phantoms.TTerm Syntax.KeyedElement
keyedElementWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the key field of hydra.go.syntax.KeyedElement
keyedElementWithKey :: Phantoms.TTerm Syntax.KeyedElement -> Phantoms.TTerm (Maybe Syntax.Key) -> Phantoms.TTerm Syntax.KeyedElement
keyedElementWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.KeyedElement"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.LabeledStmt
labeledStmt :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStmt
labeledStmt label statement =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm statement)}]}))
-- | DSL accessor for the label field of hydra.go.syntax.LabeledStmt
labeledStmtLabel :: Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Identifier
labeledStmtLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statement field of hydra.go.syntax.LabeledStmt
labeledStmtStatement :: Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Statement
labeledStmtStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
        Core.projectionFieldName = (Core.Name "statement")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the label field of hydra.go.syntax.LabeledStmt
labeledStmtWithLabel :: Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStmt
labeledStmtWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
              Core.projectionFieldName = (Core.Name "statement")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statement field of hydra.go.syntax.LabeledStmt
labeledStmtWithStatement :: Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStmt
labeledStmtWithStatement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.LabeledStmt"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statement"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the basic variant of hydra.go.syntax.Literal
literalBasic :: Phantoms.TTerm Syntax.BasicLit -> Phantoms.TTerm Syntax.Literal
literalBasic x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "basic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the composite variant of hydra.go.syntax.Literal
literalComposite :: Phantoms.TTerm Syntax.CompositeLit -> Phantoms.TTerm Syntax.Literal
literalComposite x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "composite"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.go.syntax.Literal
literalFunction :: Phantoms.TTerm Syntax.FunctionLit -> Phantoms.TTerm Syntax.Literal
literalFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the array variant of hydra.go.syntax.LiteralType
literalTypeArray :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.LiteralType
literalTypeArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the inferredArray variant of hydra.go.syntax.LiteralType
literalTypeInferredArray :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.LiteralType
literalTypeInferredArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inferredArray"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the map variant of hydra.go.syntax.LiteralType
literalTypeMap :: Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.LiteralType
literalTypeMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.go.syntax.LiteralType
literalTypeName :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.LiteralType
literalTypeName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slice variant of hydra.go.syntax.LiteralType
literalTypeSlice :: Phantoms.TTerm Syntax.SliceType -> Phantoms.TTerm Syntax.LiteralType
literalTypeSlice x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the struct variant of hydra.go.syntax.LiteralType
literalTypeStruct :: Phantoms.TTerm Syntax.StructType -> Phantoms.TTerm Syntax.LiteralType
literalTypeStruct x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.LiteralType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.LiteralValue wrapper
literalValue :: Phantoms.TTerm [Syntax.KeyedElement] -> Phantoms.TTerm Syntax.LiteralValue
literalValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.LiteralValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.MapType
mapType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MapType
mapType key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the key field of hydra.go.syntax.MapType
mapTypeKey :: Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type
mapTypeKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.go.syntax.MapType
mapTypeValue :: Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type
mapTypeValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.go.syntax.MapType
mapTypeWithKey :: Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MapType
mapTypeWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.go.syntax.MapType
mapTypeWithValue :: Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MapType
mapTypeWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MapType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MapType"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.MethodDecl
methodDecl :: Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.MethodDecl
methodDecl receiver name signature body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Phantoms.unTTerm receiver)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))
-- | DSL accessor for the body field of hydra.go.syntax.MethodDecl
methodDeclBody :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody)
methodDeclBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.go.syntax.MethodDecl
methodDeclName :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Identifier
methodDeclName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the receiver field of hydra.go.syntax.MethodDecl
methodDeclReceiver :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Receiver
methodDeclReceiver x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "receiver")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.MethodDecl
methodDeclSignature :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Signature
methodDeclSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the body field of hydra.go.syntax.MethodDecl
methodDeclWithBody :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm (Maybe Syntax.FunctionBody) -> Phantoms.TTerm Syntax.MethodDecl
methodDeclWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.go.syntax.MethodDecl
methodDeclWithName :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodDecl
methodDeclWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the receiver field of hydra.go.syntax.MethodDecl
methodDeclWithReceiver :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.MethodDecl
methodDeclWithReceiver original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.go.syntax.MethodDecl
methodDeclWithSignature :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.MethodDecl
methodDeclWithSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodDecl"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.MethodElem
methodElem :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.MethodElem
methodElem name signature =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm signature)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.MethodElem
methodElemName :: Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Identifier
methodElemName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the signature field of hydra.go.syntax.MethodElem
methodElemSignature :: Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Signature
methodElemSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.MethodElem
methodElemWithName :: Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodElem
methodElemWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.go.syntax.MethodElem
methodElemWithSignature :: Phantoms.TTerm Syntax.MethodElem -> Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.MethodElem
methodElemWithSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodElem"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.MethodExpr
methodExpr :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodExpr
methodExpr receiver method =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Phantoms.unTTerm receiver)},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Phantoms.unTTerm method)}]}))
-- | DSL accessor for the method field of hydra.go.syntax.MethodExpr
methodExprMethod :: Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Identifier
methodExprMethod x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
        Core.projectionFieldName = (Core.Name "method")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the receiver field of hydra.go.syntax.MethodExpr
methodExprReceiver :: Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Type
methodExprReceiver x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
        Core.projectionFieldName = (Core.Name "receiver")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the method field of hydra.go.syntax.MethodExpr
methodExprWithMethod :: Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.MethodExpr
methodExprWithMethod original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
              Core.projectionFieldName = (Core.Name "receiver")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the receiver field of hydra.go.syntax.MethodExpr
methodExprWithReceiver :: Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.MethodExpr
methodExprWithReceiver original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "receiver"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "method"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.MethodExpr"),
              Core.projectionFieldName = (Core.Name "method")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.Module
module_ :: Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.Module
module_ package imports declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.go.syntax.Module
moduleDeclarations :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.TopLevelDecl]
moduleDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the imports field of hydra.go.syntax.Module
moduleImports :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.ImportDecl]
moduleImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the package field of hydra.go.syntax.Module
modulePackage :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm Syntax.PackageClause
modulePackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the declarations field of hydra.go.syntax.Module
moduleWithDeclarations :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.Module
moduleWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the imports field of hydra.go.syntax.Module
moduleWithImports :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm Syntax.Module
moduleWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the package field of hydra.go.syntax.Module
moduleWithPackage :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm Syntax.Module
moduleWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the bitClear variant of hydra.go.syntax.MulOp
mulOpBitClear :: Phantoms.TTerm Syntax.MulOp
mulOpBitClear =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitClear"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseAnd variant of hydra.go.syntax.MulOp
mulOpBitwiseAnd :: Phantoms.TTerm Syntax.MulOp
mulOpBitwiseAnd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the divide variant of hydra.go.syntax.MulOp
mulOpDivide :: Phantoms.TTerm Syntax.MulOp
mulOpDivide =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShift variant of hydra.go.syntax.MulOp
mulOpLeftShift :: Phantoms.TTerm Syntax.MulOp
mulOpLeftShift =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the multiply variant of hydra.go.syntax.MulOp
mulOpMultiply :: Phantoms.TTerm Syntax.MulOp
mulOpMultiply =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the remainder variant of hydra.go.syntax.MulOp
mulOpRemainder :: Phantoms.TTerm Syntax.MulOp
mulOpRemainder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remainder"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShift variant of hydra.go.syntax.MulOp
mulOpRightShift :: Phantoms.TTerm Syntax.MulOp
mulOpRightShift =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.MulOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.NamedField
namedField :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.NamedField
namedField names type_ tag =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Phantoms.unTTerm tag)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.NamedField
namedFieldNames :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm [Syntax.Identifier]
namedFieldNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the tag field of hydra.go.syntax.NamedField
namedFieldTag :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm (Maybe Syntax.Tag)
namedFieldTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
        Core.projectionFieldName = (Core.Name "tag")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.NamedField
namedFieldType :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm Syntax.Type
namedFieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.NamedField
namedFieldWithNames :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.NamedField
namedFieldWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the tag field of hydra.go.syntax.NamedField
namedFieldWithTag :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm (Maybe Syntax.Tag) -> Phantoms.TTerm Syntax.NamedField
namedFieldWithTag original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the type field of hydra.go.syntax.NamedField
namedFieldWithType :: Phantoms.TTerm Syntax.NamedField -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.NamedField
namedFieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.NamedField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tag"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.NamedField"),
              Core.projectionFieldName = (Core.Name "tag")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the literal variant of hydra.go.syntax.Operand
operandLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Operand
operandLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Operand"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.go.syntax.Operand
operandName :: Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm Syntax.Operand
operandName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Operand"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.OperandName
operandName2 :: Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.OperandName
operandName2 name typeArgs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.OperandName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Phantoms.unTTerm typeArgs)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.OperandName
operandNameName :: Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm Syntax.QualifiedIdent
operandNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArgs field of hydra.go.syntax.OperandName
operandNameTypeArgs :: Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm [Syntax.Type]
operandNameTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
        Core.projectionFieldName = (Core.Name "typeArgs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.OperandName
operandNameWithName :: Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.OperandName
operandNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.OperandName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
              Core.projectionFieldName = (Core.Name "typeArgs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArgs field of hydra.go.syntax.OperandName
operandNameWithTypeArgs :: Phantoms.TTerm Syntax.OperandName -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.OperandName
operandNameWithTypeArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.OperandName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.OperandName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the paren variant of hydra.go.syntax.Operand
operandParen :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Operand
operandParen x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Operand"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.PackageClause wrapper
packageClause :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.PackageClause
packageClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.PackageClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.ParameterDecl
parameterDecl :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ParameterDecl
parameterDecl names variadic type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Phantoms.unTTerm variadic)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.ParameterDecl
parameterDeclNames :: Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm [Syntax.Identifier]
parameterDeclNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.ParameterDecl
parameterDeclType :: Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Syntax.Type
parameterDeclType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the variadic field of hydra.go.syntax.ParameterDecl
parameterDeclVariadic :: Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Bool
parameterDeclVariadic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
        Core.projectionFieldName = (Core.Name "variadic")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.ParameterDecl
parameterDeclWithNames :: Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ParameterDecl
parameterDeclWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "variadic")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.ParameterDecl
parameterDeclWithType :: Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ParameterDecl
parameterDeclWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "variadic")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the variadic field of hydra.go.syntax.ParameterDecl
parameterDeclWithVariadic :: Phantoms.TTerm Syntax.ParameterDecl -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ParameterDecl
parameterDeclWithVariadic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variadic"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ParameterDecl"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.Parameters wrapper
parameters :: Phantoms.TTerm [Syntax.ParameterDecl] -> Phantoms.TTerm Syntax.Parameters
parameters x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Parameters"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.PointerType wrapper
pointerType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.PointerType
pointerType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.PointerType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the call variant of hydra.go.syntax.PrimaryExpr
primaryExprCall :: Phantoms.TTerm Syntax.CallExpr -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprCall x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the conversion variant of hydra.go.syntax.PrimaryExpr
primaryExprConversion :: Phantoms.TTerm Syntax.Conversion -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprConversion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conversion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the index variant of hydra.go.syntax.PrimaryExpr
primaryExprIndex :: Phantoms.TTerm Syntax.IndexExpr -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprIndex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the methodExpr variant of hydra.go.syntax.PrimaryExpr
primaryExprMethodExpr :: Phantoms.TTerm Syntax.MethodExpr -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprMethodExpr x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "methodExpr"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the operand variant of hydra.go.syntax.PrimaryExpr
primaryExprOperand :: Phantoms.TTerm Syntax.Operand -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprOperand x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "operand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the selector variant of hydra.go.syntax.PrimaryExpr
primaryExprSelector :: Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprSelector x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selector"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slice variant of hydra.go.syntax.PrimaryExpr
primaryExprSlice :: Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprSlice x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeAssertion variant of hydra.go.syntax.PrimaryExpr
primaryExprTypeAssertion :: Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.PrimaryExpr
primaryExprTypeAssertion x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.PrimaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAssertion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.QualifiedIdent
qualifiedIdent :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.QualifiedIdent
qualifiedIdent package name =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.QualifiedIdent
qualifiedIdentName :: Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.Identifier
qualifiedIdentName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the package field of hydra.go.syntax.QualifiedIdent
qualifiedIdentPackage :: Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm (Maybe Syntax.Identifier)
qualifiedIdentPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.QualifiedIdent
qualifiedIdentWithName :: Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.QualifiedIdent
qualifiedIdentWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the package field of hydra.go.syntax.QualifiedIdent
qualifiedIdentWithPackage :: Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.QualifiedIdent
qualifiedIdentWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.QualifiedIdent"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.RangeClause
rangeClause :: Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RangeClause
rangeClause vars expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Phantoms.unTTerm vars)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.RangeClause
rangeClauseExpression :: Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm Syntax.Expression
rangeClauseExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vars field of hydra.go.syntax.RangeClause
rangeClauseVars :: Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm (Maybe Syntax.RangeVars)
rangeClauseVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
        Core.projectionFieldName = (Core.Name "vars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.RangeClause
rangeClauseWithExpression :: Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RangeClause
rangeClauseWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
              Core.projectionFieldName = (Core.Name "vars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vars field of hydra.go.syntax.RangeClause
rangeClauseWithVars :: Phantoms.TTerm Syntax.RangeClause -> Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.RangeClause
rangeClauseWithVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.RangeClause"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the assign variant of hydra.go.syntax.RangeVars
rangeVarsAssign :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.RangeVars
rangeVarsAssign x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RangeVars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the declare variant of hydra.go.syntax.RangeVars
rangeVarsDeclare :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.RangeVars
rangeVarsDeclare x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RangeVars"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declare"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.RawStringLit wrapper
rawStringLit :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.RawStringLit
rawStringLit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.RawStringLit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.ReceiveCase
receiveCase :: Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ReceiveCase
receiveCase vars expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Phantoms.unTTerm vars)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.ReceiveCase
receiveCaseExpression :: Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm Syntax.Expression
receiveCaseExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vars field of hydra.go.syntax.ReceiveCase
receiveCaseVars :: Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm (Maybe Syntax.RangeVars)
receiveCaseVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
        Core.projectionFieldName = (Core.Name "vars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.ReceiveCase
receiveCaseWithExpression :: Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ReceiveCase
receiveCaseWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
              Core.projectionFieldName = (Core.Name "vars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vars field of hydra.go.syntax.ReceiveCase
receiveCaseWithVars :: Phantoms.TTerm Syntax.ReceiveCase -> Phantoms.TTerm (Maybe Syntax.RangeVars) -> Phantoms.TTerm Syntax.ReceiveCase
receiveCaseWithVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ReceiveCase"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.Receiver
receiver :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Receiver
receiver name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Receiver"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.Receiver
receiverName :: Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm (Maybe Syntax.Identifier)
receiverName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.Receiver
receiverType :: Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.Type
receiverType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.Receiver
receiverWithName :: Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.Receiver
receiverWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Receiver"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.Receiver
receiverWithType :: Phantoms.TTerm Syntax.Receiver -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Receiver
receiverWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Receiver"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Receiver"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the equal variant of hydra.go.syntax.RelOp
relOpEqual :: Phantoms.TTerm Syntax.RelOp
relOpEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greater variant of hydra.go.syntax.RelOp
relOpGreater :: Phantoms.TTerm Syntax.RelOp
relOpGreater =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterEqual variant of hydra.go.syntax.RelOp
relOpGreaterEqual :: Phantoms.TTerm Syntax.RelOp
relOpGreaterEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the less variant of hydra.go.syntax.RelOp
relOpLess :: Phantoms.TTerm Syntax.RelOp
relOpLess =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessEqual variant of hydra.go.syntax.RelOp
relOpLessEqual :: Phantoms.TTerm Syntax.RelOp
relOpLessEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.go.syntax.RelOp
relOpNotEqual :: Phantoms.TTerm Syntax.RelOp
relOpNotEqual =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.RelOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the parameters variant of hydra.go.syntax.Result
resultParameters :: Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm Syntax.Result
resultParameters x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameters"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.Result
resultType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Result
resultType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Result"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.ReturnStmt wrapper
returnStmt :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ReturnStmt
returnStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.ReturnStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.RuneLit wrapper
runeLit :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.RuneLit
runeLit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.RuneLit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.SelectStmt wrapper
selectStmt :: Phantoms.TTerm [Syntax.CommClause] -> Phantoms.TTerm Syntax.SelectStmt
selectStmt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.SelectStmt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.Selector wrapper
selector :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Selector
selector x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Selector"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.SelectorExpr
selectorExpr :: Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SelectorExpr
selectorExpr expr selector =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "selector"),
          Core.fieldTerm = (Phantoms.unTTerm selector)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.SelectorExpr
selectorExprExpr :: Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.PrimaryExpr
selectorExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the selector field of hydra.go.syntax.SelectorExpr
selectorExprSelector :: Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.Identifier
selectorExprSelector x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
        Core.projectionFieldName = (Core.Name "selector")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.SelectorExpr
selectorExprWithExpr :: Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.SelectorExpr
selectorExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "selector"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
              Core.projectionFieldName = (Core.Name "selector")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the selector field of hydra.go.syntax.SelectorExpr
selectorExprWithSelector :: Phantoms.TTerm Syntax.SelectorExpr -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.SelectorExpr
selectorExprWithSelector original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SelectorExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "selector"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.SendStmt
sendStmt :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SendStmt
sendStmt channel value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "channel"),
          Core.fieldTerm = (Phantoms.unTTerm channel)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the channel field of hydra.go.syntax.SendStmt
sendStmtChannel :: Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression
sendStmtChannel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
        Core.projectionFieldName = (Core.Name "channel")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.go.syntax.SendStmt
sendStmtValue :: Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression
sendStmtValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the channel field of hydra.go.syntax.SendStmt
sendStmtWithChannel :: Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SendStmt
sendStmtWithChannel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "channel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.go.syntax.SendStmt
sendStmtWithValue :: Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SendStmt
sendStmtWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "channel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SendStmt"),
              Core.projectionFieldName = (Core.Name "channel")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.ShortVarDecl
shortVarDecl :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ShortVarDecl
shortVarDecl names values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.ShortVarDecl
shortVarDeclNames :: Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Identifier]
shortVarDeclNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the values field of hydra.go.syntax.ShortVarDecl
shortVarDeclValues :: Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Expression]
shortVarDeclValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.ShortVarDecl
shortVarDeclWithNames :: Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.ShortVarDecl
shortVarDeclWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the values field of hydra.go.syntax.ShortVarDecl
shortVarDeclWithValues :: Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.ShortVarDecl
shortVarDeclWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.ShortVarDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.Signature
signature :: Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm (Maybe Syntax.Result) -> Phantoms.TTerm Syntax.Signature
signature parameters result =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Signature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm result)}]}))
-- | DSL accessor for the parameters field of hydra.go.syntax.Signature
signatureParameters :: Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.Parameters
signatureParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the result field of hydra.go.syntax.Signature
signatureResult :: Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.Result)
signatureResult x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
        Core.projectionFieldName = (Core.Name "result")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the parameters field of hydra.go.syntax.Signature
signatureWithParameters :: Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm Syntax.Signature
signatureWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Signature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the result field of hydra.go.syntax.Signature
signatureWithResult :: Phantoms.TTerm Syntax.Signature -> Phantoms.TTerm (Maybe Syntax.Result) -> Phantoms.TTerm Syntax.Signature
signatureWithResult original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.Signature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.Signature"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.SimpleSlice
simpleSlice :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleSlice
simpleSlice low high =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Phantoms.unTTerm low)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Phantoms.unTTerm high)}]}))
-- | DSL accessor for the high field of hydra.go.syntax.SimpleSlice
simpleSliceHigh :: Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression)
simpleSliceHigh x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
        Core.projectionFieldName = (Core.Name "high")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the low field of hydra.go.syntax.SimpleSlice
simpleSliceLow :: Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression)
simpleSliceLow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
        Core.projectionFieldName = (Core.Name "low")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the high field of hydra.go.syntax.SimpleSlice
simpleSliceWithHigh :: Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleSlice
simpleSliceWithHigh original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
              Core.projectionFieldName = (Core.Name "low")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the low field of hydra.go.syntax.SimpleSlice
simpleSliceWithLow :: Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SimpleSlice
simpleSliceWithLow original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "low"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "high"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SimpleSlice"),
              Core.projectionFieldName = (Core.Name "high")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the assignment variant of hydra.go.syntax.SimpleStmt
simpleStmtAssignment :: Phantoms.TTerm Syntax.Assignment -> Phantoms.TTerm Syntax.SimpleStmt
simpleStmtAssignment x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the empty variant of hydra.go.syntax.SimpleStmt
simpleStmtEmpty :: Phantoms.TTerm Syntax.EmptyStmt -> Phantoms.TTerm Syntax.SimpleStmt
simpleStmtEmpty x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the expression variant of hydra.go.syntax.SimpleStmt
simpleStmtExpression :: Phantoms.TTerm Syntax.ExpressionStmt -> Phantoms.TTerm Syntax.SimpleStmt
simpleStmtExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the incDec variant of hydra.go.syntax.SimpleStmt
simpleStmtIncDec :: Phantoms.TTerm Syntax.IncDecStmt -> Phantoms.TTerm Syntax.SimpleStmt
simpleStmtIncDec x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incDec"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the send variant of hydra.go.syntax.SimpleStmt
simpleStmtSend :: Phantoms.TTerm Syntax.SendStmt -> Phantoms.TTerm Syntax.SimpleStmt
simpleStmtSend x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "send"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the shortVarDecl variant of hydra.go.syntax.SimpleStmt
simpleStmtShortVarDecl :: Phantoms.TTerm Syntax.ShortVarDecl -> Phantoms.TTerm Syntax.SimpleStmt
simpleStmtShortVarDecl x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SimpleStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortVarDecl"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.SliceExpr
sliceExpr :: Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.SliceExpr
sliceExpr expr slice =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "slice"),
          Core.fieldTerm = (Phantoms.unTTerm slice)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.SliceExpr
sliceExprExpr :: Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.PrimaryExpr
sliceExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the slice field of hydra.go.syntax.SliceExpr
sliceExprSlice :: Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.Slice
sliceExprSlice x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
        Core.projectionFieldName = (Core.Name "slice")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.SliceExpr
sliceExprWithExpr :: Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.SliceExpr
sliceExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "slice"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
              Core.projectionFieldName = (Core.Name "slice")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the slice field of hydra.go.syntax.SliceExpr
sliceExprWithSlice :: Phantoms.TTerm Syntax.SliceExpr -> Phantoms.TTerm Syntax.Slice -> Phantoms.TTerm Syntax.SliceExpr
sliceExprWithSlice original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SliceExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "slice"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the full variant of hydra.go.syntax.Slice
sliceFull :: Phantoms.TTerm Syntax.FullSlice -> Phantoms.TTerm Syntax.Slice
sliceFull x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "full"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.go.syntax.Slice
sliceSimple :: Phantoms.TTerm Syntax.SimpleSlice -> Phantoms.TTerm Syntax.Slice
sliceSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Slice"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.SliceType wrapper
sliceType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.SliceType
sliceType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.SliceType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.SourceFile
sourceFile :: Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.SourceFile
sourceFile package imports declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.go.syntax.SourceFile
sourceFileDeclarations :: Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.TopLevelDecl]
sourceFileDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the imports field of hydra.go.syntax.SourceFile
sourceFileImports :: Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.ImportDecl]
sourceFileImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the package field of hydra.go.syntax.SourceFile
sourceFilePackage :: Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm Syntax.PackageClause
sourceFilePackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the declarations field of hydra.go.syntax.SourceFile
sourceFileWithDeclarations :: Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.TopLevelDecl] -> Phantoms.TTerm Syntax.SourceFile
sourceFileWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the imports field of hydra.go.syntax.SourceFile
sourceFileWithImports :: Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm [Syntax.ImportDecl] -> Phantoms.TTerm Syntax.SourceFile
sourceFileWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the package field of hydra.go.syntax.SourceFile
sourceFileWithPackage :: Phantoms.TTerm Syntax.SourceFile -> Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm Syntax.SourceFile
sourceFileWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.SourceFile"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the block variant of hydra.go.syntax.Statement
statementBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm Syntax.Statement
statementBlock x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the break variant of hydra.go.syntax.Statement
statementBreak :: Phantoms.TTerm Syntax.BreakStmt -> Phantoms.TTerm Syntax.Statement
statementBreak x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the continue variant of hydra.go.syntax.Statement
statementContinue :: Phantoms.TTerm Syntax.ContinueStmt -> Phantoms.TTerm Syntax.Statement
statementContinue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the declaration variant of hydra.go.syntax.Statement
statementDeclaration :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.Statement
statementDeclaration x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the defer variant of hydra.go.syntax.Statement
statementDefer :: Phantoms.TTerm Syntax.DeferStmt -> Phantoms.TTerm Syntax.Statement
statementDefer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "defer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the fallthrough variant of hydra.go.syntax.Statement
statementFallthrough :: Phantoms.TTerm Syntax.FallthroughStmt -> Phantoms.TTerm Syntax.Statement
statementFallthrough x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fallthrough"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the for variant of hydra.go.syntax.Statement
statementFor :: Phantoms.TTerm Syntax.ForStmt -> Phantoms.TTerm Syntax.Statement
statementFor x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the go variant of hydra.go.syntax.Statement
statementGo :: Phantoms.TTerm Syntax.GoStmt -> Phantoms.TTerm Syntax.Statement
statementGo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "go"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the goto variant of hydra.go.syntax.Statement
statementGoto :: Phantoms.TTerm Syntax.GotoStmt -> Phantoms.TTerm Syntax.Statement
statementGoto x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "goto"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the if variant of hydra.go.syntax.Statement
statementIf :: Phantoms.TTerm Syntax.IfStmt -> Phantoms.TTerm Syntax.Statement
statementIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the labeled variant of hydra.go.syntax.Statement
statementLabeled :: Phantoms.TTerm Syntax.LabeledStmt -> Phantoms.TTerm Syntax.Statement
statementLabeled x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the return variant of hydra.go.syntax.Statement
statementReturn :: Phantoms.TTerm Syntax.ReturnStmt -> Phantoms.TTerm Syntax.Statement
statementReturn x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the select variant of hydra.go.syntax.Statement
statementSelect :: Phantoms.TTerm Syntax.SelectStmt -> Phantoms.TTerm Syntax.Statement
statementSelect x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.go.syntax.Statement
statementSimple :: Phantoms.TTerm Syntax.SimpleStmt -> Phantoms.TTerm Syntax.Statement
statementSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the switch variant of hydra.go.syntax.Statement
statementSwitch :: Phantoms.TTerm Syntax.SwitchStmt -> Phantoms.TTerm Syntax.Statement
statementSwitch x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interpreted variant of hydra.go.syntax.StringLit
stringLitInterpreted :: Phantoms.TTerm Syntax.InterpretedStringLit -> Phantoms.TTerm Syntax.StringLit
stringLitInterpreted x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.StringLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interpreted"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the raw variant of hydra.go.syntax.StringLit
stringLitRaw :: Phantoms.TTerm Syntax.RawStringLit -> Phantoms.TTerm Syntax.StringLit
stringLitRaw x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.StringLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "raw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.StructType wrapper
structType :: Phantoms.TTerm [Syntax.FieldDecl] -> Phantoms.TTerm Syntax.StructType
structType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.StructType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the expression variant of hydra.go.syntax.SwitchStmt
switchStmtExpression :: Phantoms.TTerm Syntax.ExprSwitchStmt -> Phantoms.TTerm Syntax.SwitchStmt
switchStmtExpression x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SwitchStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.go.syntax.SwitchStmt
switchStmtType :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm Syntax.SwitchStmt
switchStmtType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.SwitchStmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.Tag wrapper
tag :: Phantoms.TTerm Syntax.StringLit -> Phantoms.TTerm Syntax.Tag
tag x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.Tag"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the declaration variant of hydra.go.syntax.TopLevelDecl
topLevelDeclDeclaration :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.TopLevelDecl
topLevelDeclDeclaration x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TopLevelDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.go.syntax.TopLevelDecl
topLevelDeclFunction :: Phantoms.TTerm Syntax.FunctionDecl -> Phantoms.TTerm Syntax.TopLevelDecl
topLevelDeclFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TopLevelDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the method variant of hydra.go.syntax.TopLevelDecl
topLevelDeclMethod :: Phantoms.TTerm Syntax.MethodDecl -> Phantoms.TTerm Syntax.TopLevelDecl
topLevelDeclMethod x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TopLevelDecl"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.go.syntax.TypeAssertion wrapper
typeAssertion :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAssertion
typeAssertion x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeAssertion"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.TypeAssertionExpr
typeAssertionExpr :: Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAssertionExpr
typeAssertionExpr expr type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the expr field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprExpr :: Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.PrimaryExpr
typeAssertionExprExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
        Core.projectionFieldName = (Core.Name "expr")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprType :: Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.Type
typeAssertionExprType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expr field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprWithExpr :: Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.TypeAssertionExpr
typeAssertionExprWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.TypeAssertionExpr
typeAssertionExprWithType :: Phantoms.TTerm Syntax.TypeAssertionExpr -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeAssertionExpr
typeAssertionExprWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeAssertionExpr"),
              Core.projectionFieldName = (Core.Name "expr")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.TypeCaseClause
typeCaseClause :: Phantoms.TTerm (Maybe [Syntax.Type]) -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.TypeCaseClause
typeCaseClause case_ statements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm statements)}]}))
-- | DSL accessor for the case field of hydra.go.syntax.TypeCaseClause
typeCaseClauseCase :: Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm (Maybe [Syntax.Type])
typeCaseClauseCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the statements field of hydra.go.syntax.TypeCaseClause
typeCaseClauseStatements :: Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm [Syntax.Statement]
typeCaseClauseStatements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the case field of hydra.go.syntax.TypeCaseClause
typeCaseClauseWithCase :: Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm (Maybe [Syntax.Type]) -> Phantoms.TTerm Syntax.TypeCaseClause
typeCaseClauseWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the statements field of hydra.go.syntax.TypeCaseClause
typeCaseClauseWithStatements :: Phantoms.TTerm Syntax.TypeCaseClause -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.TypeCaseClause
typeCaseClauseWithStatements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeCaseClause"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.TypeConstraint wrapper
typeConstraint :: Phantoms.TTerm Syntax.TypeElem -> Phantoms.TTerm Syntax.TypeConstraint
typeConstraint x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeConstraint"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.go.syntax.TypeDecl wrapper
typeDecl :: Phantoms.TTerm [Syntax.TypeSpec] -> Phantoms.TTerm Syntax.TypeDecl
typeDecl x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeDecl"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.TypeDef
typeDef :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDef
typeDef name typeParams type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.TypeDef
typeDefName :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Identifier
typeDefName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.TypeDef
typeDefType :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Type
typeDefType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeParams field of hydra.go.syntax.TypeDef
typeDefTypeParams :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm (Maybe Syntax.TypeParameters)
typeDefTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.TypeDef
typeDefWithName :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeDef
typeDefWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.TypeDef
typeDefWithType :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDef
typeDefWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeParams field of hydra.go.syntax.TypeDef
typeDefWithTypeParams :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm (Maybe Syntax.TypeParameters) -> Phantoms.TTerm Syntax.TypeDef
typeDefWithTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeDef"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.TypeElem wrapper
typeElem :: Phantoms.TTerm [Syntax.TypeTerm] -> Phantoms.TTerm Syntax.TypeElem
typeElem x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeElem"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the array variant of hydra.go.syntax.TypeLit
typeLitArray :: Phantoms.TTerm Syntax.ArrayType -> Phantoms.TTerm Syntax.TypeLit
typeLitArray x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the channel variant of hydra.go.syntax.TypeLit
typeLitChannel :: Phantoms.TTerm Syntax.ChannelType -> Phantoms.TTerm Syntax.TypeLit
typeLitChannel x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "channel"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.go.syntax.TypeLit
typeLitFunction :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.TypeLit
typeLitFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the interface variant of hydra.go.syntax.TypeLit
typeLitInterface :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm Syntax.TypeLit
typeLitInterface x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the map variant of hydra.go.syntax.TypeLit
typeLitMap :: Phantoms.TTerm Syntax.MapType -> Phantoms.TTerm Syntax.TypeLit
typeLitMap x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pointer variant of hydra.go.syntax.TypeLit
typeLitPointer :: Phantoms.TTerm Syntax.PointerType -> Phantoms.TTerm Syntax.TypeLit
typeLitPointer x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pointer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the slice variant of hydra.go.syntax.TypeLit
typeLitSlice :: Phantoms.TTerm Syntax.SliceType -> Phantoms.TTerm Syntax.TypeLit
typeLitSlice x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "slice"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the struct variant of hydra.go.syntax.TypeLit
typeLitStruct :: Phantoms.TTerm Syntax.StructType -> Phantoms.TTerm Syntax.TypeLit
typeLitStruct x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeLit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "struct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.go.syntax.Type
typeLiteral :: Phantoms.TTerm Syntax.TypeLit -> Phantoms.TTerm Syntax.Type
typeLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.go.syntax.Type
typeName :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.Type
typeName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.TypeName
typeName2 :: Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TypeName
typeName2 name typeArgs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Phantoms.unTTerm typeArgs)}]}))
-- | DSL accessor for the name field of hydra.go.syntax.TypeName
typeNameName :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.QualifiedIdent
typeNameName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeArgs field of hydra.go.syntax.TypeName
typeNameTypeArgs :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.Type]
typeNameTypeArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
        Core.projectionFieldName = (Core.Name "typeArgs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.go.syntax.TypeName
typeNameWithName :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.QualifiedIdent -> Phantoms.TTerm Syntax.TypeName
typeNameWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
              Core.projectionFieldName = (Core.Name "typeArgs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeArgs field of hydra.go.syntax.TypeName
typeNameWithTypeArgs :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.TypeName
typeNameWithTypeArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeName"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeArgs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.go.syntax.TypeParamDecl
typeParamDecl :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.TypeConstraint -> Phantoms.TTerm Syntax.TypeParamDecl
typeParamDecl names constraint =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Phantoms.unTTerm constraint)}]}))
-- | DSL accessor for the constraint field of hydra.go.syntax.TypeParamDecl
typeParamDeclConstraint :: Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm Syntax.TypeConstraint
typeParamDeclConstraint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
        Core.projectionFieldName = (Core.Name "constraint")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the names field of hydra.go.syntax.TypeParamDecl
typeParamDeclNames :: Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm [Syntax.Identifier]
typeParamDeclNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the constraint field of hydra.go.syntax.TypeParamDecl
typeParamDeclWithConstraint :: Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm Syntax.TypeConstraint -> Phantoms.TTerm Syntax.TypeParamDecl
typeParamDeclWithConstraint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the names field of hydra.go.syntax.TypeParamDecl
typeParamDeclWithNames :: Phantoms.TTerm Syntax.TypeParamDecl -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.TypeParamDecl
typeParamDeclWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeParamDecl"),
              Core.projectionFieldName = (Core.Name "constraint")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.go.syntax.TypeParameters wrapper
typeParameters :: Phantoms.TTerm [Syntax.TypeParamDecl] -> Phantoms.TTerm Syntax.TypeParameters
typeParameters x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.TypeParameters"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the paren variant of hydra.go.syntax.Type
typeParen :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type
typeParen x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paren"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the alias variant of hydra.go.syntax.TypeSpec
typeSpecAlias :: Phantoms.TTerm Syntax.AliasDecl -> Phantoms.TTerm Syntax.TypeSpec
typeSpecAlias x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alias"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the definition variant of hydra.go.syntax.TypeSpec
typeSpecDefinition :: Phantoms.TTerm Syntax.TypeDef -> Phantoms.TTerm Syntax.TypeSpec
typeSpecDefinition x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.TypeSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.go.syntax.TypeSwitchGuard
typeSwitchGuard :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.TypeSwitchGuard
typeSwitchGuard name expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardExpression :: Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm Syntax.PrimaryExpr
typeSwitchGuardExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardName :: Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm (Maybe Syntax.Identifier)
typeSwitchGuardName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardWithExpression :: Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.TypeSwitchGuard
typeSwitchGuardWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.go.syntax.TypeSwitchGuard
typeSwitchGuardWithName :: Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.TypeSwitchGuard
typeSwitchGuardWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchGuard"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.TypeSwitchStmt
typeSwitchStmt :: Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm [Syntax.TypeCaseClause] -> Phantoms.TTerm Syntax.TypeSwitchStmt
typeSwitchStmt init guard cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm guard)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtCases :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm [Syntax.TypeCaseClause]
typeSwitchStmtCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the guard field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtGuard :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm Syntax.TypeSwitchGuard
typeSwitchStmtGuard x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
        Core.projectionFieldName = (Core.Name "guard")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the init field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtInit :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt)
typeSwitchStmtInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the cases field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtWithCases :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm [Syntax.TypeCaseClause] -> Phantoms.TTerm Syntax.TypeSwitchStmt
typeSwitchStmtWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the guard field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtWithGuard :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm Syntax.TypeSwitchGuard -> Phantoms.TTerm Syntax.TypeSwitchStmt
typeSwitchStmtWithGuard original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the init field of hydra.go.syntax.TypeSwitchStmt
typeSwitchStmtWithInit :: Phantoms.TTerm Syntax.TypeSwitchStmt -> Phantoms.TTerm (Maybe Syntax.SimpleStmt) -> Phantoms.TTerm Syntax.TypeSwitchStmt
typeSwitchStmtWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "guard"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "guard")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeSwitchStmt"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.go.syntax.TypeTerm
typeTerm :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeTerm
typeTerm underlying type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "underlying"),
          Core.fieldTerm = (Phantoms.unTTerm underlying)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the type field of hydra.go.syntax.TypeTerm
typeTermType :: Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Syntax.Type
typeTermType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the underlying field of hydra.go.syntax.TypeTerm
typeTermUnderlying :: Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Bool
typeTermUnderlying x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
        Core.projectionFieldName = (Core.Name "underlying")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the type field of hydra.go.syntax.TypeTerm
typeTermWithType :: Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeTerm
typeTermWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "underlying"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
              Core.projectionFieldName = (Core.Name "underlying")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the underlying field of hydra.go.syntax.TypeTerm
typeTermWithUnderlying :: Phantoms.TTerm Syntax.TypeTerm -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TypeTerm
typeTermWithUnderlying original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "underlying"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.TypeTerm"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the body of hydra.go.syntax.Block
unBlock :: Phantoms.TTerm Syntax.Block -> Phantoms.TTerm [Syntax.Statement]
unBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Block")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.BreakStmt
unBreakStmt :: Phantoms.TTerm Syntax.BreakStmt -> Phantoms.TTerm (Maybe Syntax.Identifier)
unBreakStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.BreakStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ConstDecl
unConstDecl :: Phantoms.TTerm Syntax.ConstDecl -> Phantoms.TTerm [Syntax.ConstSpec]
unConstDecl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ConstDecl")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ContinueStmt
unContinueStmt :: Phantoms.TTerm Syntax.ContinueStmt -> Phantoms.TTerm (Maybe Syntax.Identifier)
unContinueStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ContinueStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.DeferStmt
unDeferStmt :: Phantoms.TTerm Syntax.DeferStmt -> Phantoms.TTerm Syntax.Expression
unDeferStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.DeferStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ElementList
unElementList :: Phantoms.TTerm Syntax.ElementList -> Phantoms.TTerm [Syntax.KeyedElement]
unElementList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ElementList")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.EmptyStmt
unEmptyStmt :: Phantoms.TTerm Syntax.EmptyStmt -> Phantoms.TTerm ()
unEmptyStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.EmptyStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ExpressionStmt
unExpressionStmt :: Phantoms.TTerm Syntax.ExpressionStmt -> Phantoms.TTerm Syntax.Expression
unExpressionStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ExpressionStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FallthroughStmt
unFallthroughStmt :: Phantoms.TTerm Syntax.FallthroughStmt -> Phantoms.TTerm ()
unFallthroughStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FallthroughStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FloatLit
unFloatLit :: Phantoms.TTerm Syntax.FloatLit -> Phantoms.TTerm Double
unFloatLit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FloatLit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FunctionBody
unFunctionBody :: Phantoms.TTerm Syntax.FunctionBody -> Phantoms.TTerm Syntax.Block
unFunctionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FunctionBody")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.FunctionType
unFunctionType :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Signature
unFunctionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.FunctionType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.GoStmt
unGoStmt :: Phantoms.TTerm Syntax.GoStmt -> Phantoms.TTerm Syntax.Expression
unGoStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.GoStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.GotoStmt
unGotoStmt :: Phantoms.TTerm Syntax.GotoStmt -> Phantoms.TTerm Syntax.Identifier
unGotoStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.GotoStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Identifier
unIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm String
unIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Identifier")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ImaginaryLit
unImaginaryLit :: Phantoms.TTerm Syntax.ImaginaryLit -> Phantoms.TTerm Double
unImaginaryLit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ImaginaryLit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ImportDecl
unImportDecl :: Phantoms.TTerm Syntax.ImportDecl -> Phantoms.TTerm [Syntax.ImportSpec]
unImportDecl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ImportDecl")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ImportPath
unImportPath :: Phantoms.TTerm Syntax.ImportPath -> Phantoms.TTerm Syntax.StringLit
unImportPath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ImportPath")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Index
unIndex :: Phantoms.TTerm Syntax.Index -> Phantoms.TTerm [Syntax.Expression]
unIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Index")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.IntLit
unIntLit :: Phantoms.TTerm Syntax.IntLit -> Phantoms.TTerm Integer
unIntLit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.IntLit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.InterfaceType
unInterfaceType :: Phantoms.TTerm Syntax.InterfaceType -> Phantoms.TTerm [Syntax.InterfaceElem]
unInterfaceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.InterfaceType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.InterpretedStringLit
unInterpretedStringLit :: Phantoms.TTerm Syntax.InterpretedStringLit -> Phantoms.TTerm String
unInterpretedStringLit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.InterpretedStringLit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.LiteralValue
unLiteralValue :: Phantoms.TTerm Syntax.LiteralValue -> Phantoms.TTerm [Syntax.KeyedElement]
unLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.LiteralValue")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.PackageClause
unPackageClause :: Phantoms.TTerm Syntax.PackageClause -> Phantoms.TTerm Syntax.Identifier
unPackageClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.PackageClause")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Parameters
unParameters :: Phantoms.TTerm Syntax.Parameters -> Phantoms.TTerm [Syntax.ParameterDecl]
unParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Parameters")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.PointerType
unPointerType :: Phantoms.TTerm Syntax.PointerType -> Phantoms.TTerm Syntax.Type
unPointerType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.PointerType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.RawStringLit
unRawStringLit :: Phantoms.TTerm Syntax.RawStringLit -> Phantoms.TTerm String
unRawStringLit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.RawStringLit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.ReturnStmt
unReturnStmt :: Phantoms.TTerm Syntax.ReturnStmt -> Phantoms.TTerm [Syntax.Expression]
unReturnStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.ReturnStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.RuneLit
unRuneLit :: Phantoms.TTerm Syntax.RuneLit -> Phantoms.TTerm Int
unRuneLit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.RuneLit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.SelectStmt
unSelectStmt :: Phantoms.TTerm Syntax.SelectStmt -> Phantoms.TTerm [Syntax.CommClause]
unSelectStmt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.SelectStmt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Selector
unSelector :: Phantoms.TTerm Syntax.Selector -> Phantoms.TTerm Syntax.Identifier
unSelector x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Selector")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.SliceType
unSliceType :: Phantoms.TTerm Syntax.SliceType -> Phantoms.TTerm Syntax.Type
unSliceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.SliceType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.StructType
unStructType :: Phantoms.TTerm Syntax.StructType -> Phantoms.TTerm [Syntax.FieldDecl]
unStructType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.StructType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.Tag
unTag :: Phantoms.TTerm Syntax.Tag -> Phantoms.TTerm Syntax.StringLit
unTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.Tag")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeAssertion
unTypeAssertion :: Phantoms.TTerm Syntax.TypeAssertion -> Phantoms.TTerm Syntax.Type
unTypeAssertion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeAssertion")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeConstraint
unTypeConstraint :: Phantoms.TTerm Syntax.TypeConstraint -> Phantoms.TTerm Syntax.TypeElem
unTypeConstraint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeConstraint")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeDecl
unTypeDecl :: Phantoms.TTerm Syntax.TypeDecl -> Phantoms.TTerm [Syntax.TypeSpec]
unTypeDecl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeDecl")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeElem
unTypeElem :: Phantoms.TTerm Syntax.TypeElem -> Phantoms.TTerm [Syntax.TypeTerm]
unTypeElem x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeElem")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.TypeParameters
unTypeParameters :: Phantoms.TTerm Syntax.TypeParameters -> Phantoms.TTerm [Syntax.TypeParamDecl]
unTypeParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.TypeParameters")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.go.syntax.VarDecl
unVarDecl :: Phantoms.TTerm Syntax.VarDecl -> Phantoms.TTerm [Syntax.VarSpec]
unVarDecl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.go.syntax.VarDecl")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the op variant of hydra.go.syntax.UnaryExpr
unaryExprOp :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpr
unaryExprOp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "op"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the primary variant of hydra.go.syntax.UnaryExpr
unaryExprPrimary :: Phantoms.TTerm Syntax.PrimaryExpr -> Phantoms.TTerm Syntax.UnaryExpr
unaryExprPrimary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryExpr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the addressOf variant of hydra.go.syntax.UnaryOp
unaryOpAddressOf :: Phantoms.TTerm Syntax.UnaryOp
unaryOpAddressOf =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addressOf"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the deref variant of hydra.go.syntax.UnaryOp
unaryOpDeref :: Phantoms.TTerm Syntax.UnaryOp
unaryOpDeref =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deref"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the minus variant of hydra.go.syntax.UnaryOp
unaryOpMinus :: Phantoms.TTerm Syntax.UnaryOp
unaryOpMinus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the not variant of hydra.go.syntax.UnaryOp
unaryOpNot :: Phantoms.TTerm Syntax.UnaryOp
unaryOpNot =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.go.syntax.UnaryOp
unaryOpPlus :: Phantoms.TTerm Syntax.UnaryOp
unaryOpPlus =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the receive variant of hydra.go.syntax.UnaryOp
unaryOpReceive :: Phantoms.TTerm Syntax.UnaryOp
unaryOpReceive =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "receive"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the xor variant of hydra.go.syntax.UnaryOp
unaryOpXor :: Phantoms.TTerm Syntax.UnaryOp
unaryOpXor =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.go.syntax.UnaryOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.go.syntax.UnaryOperation
unaryOperation :: Phantoms.TTerm Syntax.UnaryOp -> Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.UnaryOperation
unaryOperation op operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))
-- | DSL accessor for the op field of hydra.go.syntax.UnaryOperation
unaryOperationOp :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryOp
unaryOperationOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
        Core.projectionFieldName = (Core.Name "op")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operand field of hydra.go.syntax.UnaryOperation
unaryOperationOperand :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpr
unaryOperationOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the op field of hydra.go.syntax.UnaryOperation
unaryOperationWithOp :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryOp -> Phantoms.TTerm Syntax.UnaryOperation
unaryOperationWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operand field of hydra.go.syntax.UnaryOperation
unaryOperationWithOperand :: Phantoms.TTerm Syntax.UnaryOperation -> Phantoms.TTerm Syntax.UnaryExpr -> Phantoms.TTerm Syntax.UnaryOperation
unaryOperationWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.UnaryOperation"),
              Core.projectionFieldName = (Core.Name "op")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for the hydra.go.syntax.VarDecl wrapper
varDecl :: Phantoms.TTerm [Syntax.VarSpec] -> Phantoms.TTerm Syntax.VarDecl
varDecl x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.go.syntax.VarDecl"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.go.syntax.VarSpec
varSpec :: Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VarSpec
varSpec names type_ values =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm names)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm values)}]}))
-- | DSL accessor for the names field of hydra.go.syntax.VarSpec
varSpecNames :: Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Identifier]
varSpecNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
        Core.projectionFieldName = (Core.Name "names")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.go.syntax.VarSpec
varSpecType :: Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm (Maybe Syntax.Type)
varSpecType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the values field of hydra.go.syntax.VarSpec
varSpecValues :: Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Expression]
varSpecValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
        Core.projectionFieldName = (Core.Name "values")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the names field of hydra.go.syntax.VarSpec
varSpecWithNames :: Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Identifier] -> Phantoms.TTerm Syntax.VarSpec
varSpecWithNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.go.syntax.VarSpec
varSpecWithType :: Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm (Maybe Syntax.Type) -> Phantoms.TTerm Syntax.VarSpec
varSpecWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "values")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the values field of hydra.go.syntax.VarSpec
varSpecWithValues :: Phantoms.TTerm Syntax.VarSpec -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.VarSpec
varSpecWithValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "names"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "names")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.go.syntax.VarSpec"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "values"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
