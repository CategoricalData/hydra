-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.haskell.syntax

module Hydra.Dsl.Haskell.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.haskell.syntax.Alternative
alternative :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.CaseRhs -> Typed.TypedTerm (Maybe Syntax.LocalBindings) -> Typed.TypedTerm Syntax.Alternative
alternative pattern rhs binds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Typed.unTypedTerm binds)}]}))
-- | DSL accessor for the binds field of hydra.haskell.syntax.Alternative
alternativeBinds :: Typed.TypedTerm Syntax.Alternative -> Typed.TypedTerm (Maybe Syntax.LocalBindings)
alternativeBinds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
        Core.projectionFieldName = (Core.Name "binds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.haskell.syntax.Alternative
alternativePattern :: Typed.TypedTerm Syntax.Alternative -> Typed.TypedTerm Syntax.Pattern
alternativePattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.Alternative
alternativeRhs :: Typed.TypedTerm Syntax.Alternative -> Typed.TypedTerm Syntax.CaseRhs
alternativeRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the binds field of hydra.haskell.syntax.Alternative
alternativeWithBinds :: Typed.TypedTerm Syntax.Alternative -> Typed.TypedTerm (Maybe Syntax.LocalBindings) -> Typed.TypedTerm Syntax.Alternative
alternativeWithBinds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.haskell.syntax.Alternative
alternativeWithPattern :: Typed.TypedTerm Syntax.Alternative -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Alternative
alternativeWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "binds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.Alternative
alternativeWithRhs :: Typed.TypedTerm Syntax.Alternative -> Typed.TypedTerm Syntax.CaseRhs -> Typed.TypedTerm Syntax.Alternative
alternativeWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "binds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHead :: Typed.TypedTerm Syntax.DeclarationHead -> Typed.TypedTerm Syntax.Variable -> Typed.TypedTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHead function operand =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm operand)}]}))
-- | DSL accessor for the function field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadFunction :: Typed.TypedTerm Syntax.ApplicationDeclarationHead -> Typed.TypedTerm Syntax.DeclarationHead
applicationDeclarationHeadFunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operand field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadOperand :: Typed.TypedTerm Syntax.ApplicationDeclarationHead -> Typed.TypedTerm Syntax.Variable
applicationDeclarationHeadOperand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the function field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithFunction :: Typed.TypedTerm Syntax.ApplicationDeclarationHead -> Typed.TypedTerm Syntax.DeclarationHead -> Typed.TypedTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithFunction original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operand field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithOperand :: Typed.TypedTerm Syntax.ApplicationDeclarationHead -> Typed.TypedTerm Syntax.Variable -> Typed.TypedTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithOperand original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationExpression
applicationExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ApplicationExpression
applicationExpression function argument =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionArgument :: Typed.TypedTerm Syntax.ApplicationExpression -> Typed.TypedTerm Syntax.Expression
applicationExpressionArgument x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the function field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionFunction :: Typed.TypedTerm Syntax.ApplicationExpression -> Typed.TypedTerm Syntax.Expression
applicationExpressionFunction x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the argument field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionWithArgument :: Typed.TypedTerm Syntax.ApplicationExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ApplicationExpression
applicationExpressionWithArgument original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the function field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionWithFunction :: Typed.TypedTerm Syntax.ApplicationExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ApplicationExpression
applicationExpressionWithFunction original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationPattern
applicationPattern :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.ApplicationPattern
applicationPattern name args =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm args)}]}))
-- | DSL accessor for the args field of hydra.haskell.syntax.ApplicationPattern
applicationPatternArgs :: Typed.TypedTerm Syntax.ApplicationPattern -> Typed.TypedTerm [Syntax.Pattern]
applicationPatternArgs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.ApplicationPattern
applicationPatternName :: Typed.TypedTerm Syntax.ApplicationPattern -> Typed.TypedTerm Syntax.Name
applicationPatternName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the args field of hydra.haskell.syntax.ApplicationPattern
applicationPatternWithArgs :: Typed.TypedTerm Syntax.ApplicationPattern -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.ApplicationPattern
applicationPatternWithArgs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.ApplicationPattern
applicationPatternWithName :: Typed.TypedTerm Syntax.ApplicationPattern -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ApplicationPattern
applicationPatternWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationType
applicationType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplicationType
applicationType context argument =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Typed.unTypedTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.haskell.syntax.ApplicationType
applicationTypeArgument :: Typed.TypedTerm Syntax.ApplicationType -> Typed.TypedTerm Syntax.Type
applicationTypeArgument x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the context field of hydra.haskell.syntax.ApplicationType
applicationTypeContext :: Typed.TypedTerm Syntax.ApplicationType -> Typed.TypedTerm Syntax.Type
applicationTypeContext x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
        Core.projectionFieldName = (Core.Name "context")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the argument field of hydra.haskell.syntax.ApplicationType
applicationTypeWithArgument :: Typed.TypedTerm Syntax.ApplicationType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplicationType
applicationTypeWithArgument original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the context field of hydra.haskell.syntax.ApplicationType
applicationTypeWithContext :: Typed.TypedTerm Syntax.ApplicationType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ApplicationType
applicationTypeWithContext original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.AsPattern
asPattern :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.AsPattern
asPattern name inner =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm inner)}]}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.AsPattern
asPatternInner :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.Pattern
asPatternInner x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.AsPattern
asPatternName :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.Name
asPatternName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the inner field of hydra.haskell.syntax.AsPattern
asPatternWithInner :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.AsPattern
asPatternWithInner original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.AsPattern
asPatternWithName :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.AsPattern
asPatternWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.CaseExpression
caseExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.Alternative] -> Typed.TypedTerm Syntax.CaseExpression
caseExpression case_ alternatives =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Typed.unTypedTerm alternatives)}]}))
-- | DSL accessor for the alternatives field of hydra.haskell.syntax.CaseExpression
caseExpressionAlternatives :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm [Syntax.Alternative]
caseExpressionAlternatives x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "alternatives")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the case field of hydra.haskell.syntax.CaseExpression
caseExpressionCase :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm Syntax.Expression
caseExpressionCase x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the alternatives field of hydra.haskell.syntax.CaseExpression
caseExpressionWithAlternatives :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm [Syntax.Alternative] -> Typed.TypedTerm Syntax.CaseExpression
caseExpressionWithAlternatives original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the case field of hydra.haskell.syntax.CaseExpression
caseExpressionWithCase :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CaseExpression
caseExpressionWithCase original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "alternatives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.haskell.syntax.CaseRhs wrapper
caseRhs :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CaseRhs
caseRhs x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.CaseRhs"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.ClassConstraint
classConstraint :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ClassConstraint
classConstraint name types =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm types)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.ClassConstraint
classConstraintName :: Typed.TypedTerm Syntax.ClassConstraint -> Typed.TypedTerm Syntax.Name
classConstraintName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.haskell.syntax.ClassConstraint
classConstraintTypes :: Typed.TypedTerm Syntax.ClassConstraint -> Typed.TypedTerm [Syntax.Type]
classConstraintTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.ClassConstraint
classConstraintWithName :: Typed.TypedTerm Syntax.ClassConstraint -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.ClassConstraint
classConstraintWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the types field of hydra.haskell.syntax.ClassConstraint
classConstraintWithTypes :: Typed.TypedTerm Syntax.ClassConstraint -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.ClassConstraint
classConstraintWithTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.ConstrainedType
constrainedType :: Typed.TypedTerm Syntax.Constraint -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ConstrainedType
constrainedType ctx type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Typed.unTypedTerm ctx)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the ctx field of hydra.haskell.syntax.ConstrainedType
constrainedTypeCtx :: Typed.TypedTerm Syntax.ConstrainedType -> Typed.TypedTerm Syntax.Constraint
constrainedTypeCtx x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
        Core.projectionFieldName = (Core.Name "ctx")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.ConstrainedType
constrainedTypeType :: Typed.TypedTerm Syntax.ConstrainedType -> Typed.TypedTerm Syntax.Type
constrainedTypeType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ctx field of hydra.haskell.syntax.ConstrainedType
constrainedTypeWithCtx :: Typed.TypedTerm Syntax.ConstrainedType -> Typed.TypedTerm Syntax.Constraint -> Typed.TypedTerm Syntax.ConstrainedType
constrainedTypeWithCtx original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.ConstrainedType
constrainedTypeWithType :: Typed.TypedTerm Syntax.ConstrainedType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.ConstrainedType
constrainedTypeWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
              Core.projectionFieldName = (Core.Name "ctx")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the class variant of hydra.haskell.syntax.Constraint
constraintClass :: Typed.TypedTerm Syntax.ClassConstraint -> Typed.TypedTerm Syntax.Constraint
constraintClass x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Constraint
constraintTuple :: Typed.TypedTerm [Syntax.Constraint] -> Typed.TypedTerm Syntax.Constraint
constraintTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ordinary variant of hydra.haskell.syntax.Constructor
constructorOrdinary :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm Syntax.Constructor
constructorOrdinary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.haskell.syntax.Constructor
constructorRecord :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm Syntax.Constructor
constructorRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.DataDeclaration
dataDeclaration :: Typed.TypedTerm Syntax.DataKeyword -> Typed.TypedTerm [Syntax.Constraint] -> Typed.TypedTerm Syntax.DeclarationHead -> Typed.TypedTerm [Syntax.Constructor] -> Typed.TypedTerm [Syntax.DerivingClause] -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclaration keyword context head constructors deriving_ comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Typed.unTypedTerm keyword)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Typed.unTypedTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Typed.unTypedTerm constructors)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Typed.unTypedTerm deriving_)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.DataDeclaration
dataDeclarationComments :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm (Maybe String)
dataDeclarationComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constructors field of hydra.haskell.syntax.DataDeclaration
dataDeclarationConstructors :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm [Syntax.Constructor]
dataDeclarationConstructors x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "constructors")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the context field of hydra.haskell.syntax.DataDeclaration
dataDeclarationContext :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm [Syntax.Constraint]
dataDeclarationContext x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "context")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the deriving field of hydra.haskell.syntax.DataDeclaration
dataDeclarationDeriving :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm [Syntax.DerivingClause]
dataDeclarationDeriving x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "deriving")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the head field of hydra.haskell.syntax.DataDeclaration
dataDeclarationHead :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm Syntax.DeclarationHead
dataDeclarationHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the keyword field of hydra.haskell.syntax.DataDeclaration
dataDeclarationKeyword :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm Syntax.DataKeyword
dataDeclarationKeyword x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "keyword")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithComments :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclarationWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the constructors field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithConstructors :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm [Syntax.Constructor] -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclarationWithConstructors original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the context field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithContext :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm [Syntax.Constraint] -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclarationWithContext original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the deriving field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithDeriving :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm [Syntax.DerivingClause] -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclarationWithDeriving original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the head field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithHead :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm Syntax.DeclarationHead -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclarationWithHead original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the keyword field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithKeyword :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm Syntax.DataKeyword -> Typed.TypedTerm Syntax.DataDeclaration
dataDeclarationWithKeyword original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the data variant of hydra.haskell.syntax.DataKeyword
dataKeywordData :: Typed.TypedTerm Syntax.DataKeyword
dataKeywordData =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DataKeyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the newtype variant of hydra.haskell.syntax.DataKeyword
dataKeywordNewtype :: Typed.TypedTerm Syntax.DataKeyword
dataKeywordNewtype =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DataKeyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newtype"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the data variant of hydra.haskell.syntax.Declaration
declarationData :: Typed.TypedTerm Syntax.DataDeclaration -> Typed.TypedTerm Syntax.Declaration
declarationData x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.haskell.syntax.DeclarationHead
declarationHeadApplication :: Typed.TypedTerm Syntax.ApplicationDeclarationHead -> Typed.TypedTerm Syntax.DeclarationHead
declarationHeadApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the simple variant of hydra.haskell.syntax.DeclarationHead
declarationHeadSimple :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.DeclarationHead
declarationHeadSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.haskell.syntax.Declaration
declarationType :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm Syntax.Declaration
declarationType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typedBinding variant of hydra.haskell.syntax.Declaration
declarationTypedBinding :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm Syntax.Declaration
declarationTypedBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the valueBinding variant of hydra.haskell.syntax.Declaration
declarationValueBinding :: Typed.TypedTerm Syntax.ValueBinding -> Typed.TypedTerm Syntax.Declaration
declarationValueBinding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valueBinding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.DerivingClause wrapper
derivingClause :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.DerivingClause
derivingClause x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.DerivingClause"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the declaration variant of hydra.haskell.syntax.Export
exportDeclaration :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm Syntax.Export
exportDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the module variant of hydra.haskell.syntax.Export
exportModule :: Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.Export
exportModule x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.haskell.syntax.Expression
expressionApplication :: Typed.TypedTerm Syntax.ApplicationExpression -> Typed.TypedTerm Syntax.Expression
expressionApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the case variant of hydra.haskell.syntax.Expression
expressionCase :: Typed.TypedTerm Syntax.CaseExpression -> Typed.TypedTerm Syntax.Expression
expressionCase x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the constructRecord variant of hydra.haskell.syntax.Expression
expressionConstructRecord :: Typed.TypedTerm Syntax.RecordExpression -> Typed.TypedTerm Syntax.Expression
expressionConstructRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructRecord"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the do variant of hydra.haskell.syntax.Expression
expressionDo :: Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.Expression
expressionDo x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.haskell.syntax.Expression
expressionIf :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
expressionIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the infixApplication variant of hydra.haskell.syntax.Expression
expressionInfixApplication :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Expression
expressionInfixApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infixApplication"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the lambda variant of hydra.haskell.syntax.Expression
expressionLambda :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.Expression
expressionLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the leftSection variant of hydra.haskell.syntax.Expression
expressionLeftSection :: Typed.TypedTerm Syntax.SectionExpression -> Typed.TypedTerm Syntax.Expression
expressionLeftSection x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftSection"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the let variant of hydra.haskell.syntax.Expression
expressionLet :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm Syntax.Expression
expressionLet x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.Expression
expressionList :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression
expressionList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.haskell.syntax.Expression
expressionLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Expression
expressionLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rightSection variant of hydra.haskell.syntax.Expression
expressionRightSection :: Typed.TypedTerm Syntax.SectionExpression -> Typed.TypedTerm Syntax.Expression
expressionRightSection x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightSection"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Expression
expressionTuple :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression
expressionTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeSignature variant of hydra.haskell.syntax.Expression
expressionTypeSignature :: Typed.TypedTerm Syntax.TypedExpression -> Typed.TypedTerm Syntax.Expression
expressionTypeSignature x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSignature"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the updateRecord variant of hydra.haskell.syntax.Expression
expressionUpdateRecord :: Typed.TypedTerm Syntax.RecordUpdateExpression -> Typed.TypedTerm Syntax.Expression
expressionUpdateRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "updateRecord"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.haskell.syntax.Expression
expressionVariable :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression
expressionVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.Field
field :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Field
field name type_ comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.Field
fieldComments :: Typed.TypedTerm Syntax.Field -> Typed.TypedTerm (Maybe String)
fieldComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.Field
fieldName :: Typed.TypedTerm Syntax.Field -> Typed.TypedTerm Syntax.Name
fieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.Field
fieldType :: Typed.TypedTerm Syntax.Field -> Typed.TypedTerm Syntax.Type
fieldType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.FieldUpdate
fieldUpdate :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FieldUpdate
fieldUpdate name value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.FieldUpdate
fieldUpdateName :: Typed.TypedTerm Syntax.FieldUpdate -> Typed.TypedTerm Syntax.Name
fieldUpdateName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.haskell.syntax.FieldUpdate
fieldUpdateValue :: Typed.TypedTerm Syntax.FieldUpdate -> Typed.TypedTerm Syntax.Expression
fieldUpdateValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.FieldUpdate
fieldUpdateWithName :: Typed.TypedTerm Syntax.FieldUpdate -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.FieldUpdate
fieldUpdateWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.haskell.syntax.FieldUpdate
fieldUpdateWithValue :: Typed.TypedTerm Syntax.FieldUpdate -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FieldUpdate
fieldUpdateWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the comments field of hydra.haskell.syntax.Field
fieldWithComments :: Typed.TypedTerm Syntax.Field -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.Field
fieldWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.Field
fieldWithName :: Typed.TypedTerm Syntax.Field -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Field
fieldWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.Field
fieldWithType :: Typed.TypedTerm Syntax.Field -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Field
fieldWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.FunctionType
functionType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.FunctionType
functionType domain codomain =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Typed.unTypedTerm codomain)}]}))
-- | DSL accessor for the codomain field of hydra.haskell.syntax.FunctionType
functionTypeCodomain :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type
functionTypeCodomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "codomain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the domain field of hydra.haskell.syntax.FunctionType
functionTypeDomain :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type
functionTypeDomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the codomain field of hydra.haskell.syntax.FunctionType
functionTypeWithCodomain :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.FunctionType
functionTypeWithCodomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the domain field of hydra.haskell.syntax.FunctionType
functionTypeWithDomain :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.FunctionType
functionTypeWithDomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.IfExpression
ifExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfExpression
ifExpression condition then_ else_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
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
-- | DSL accessor for the condition field of hydra.haskell.syntax.IfExpression
ifExpressionCondition :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
ifExpressionCondition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the else field of hydra.haskell.syntax.IfExpression
ifExpressionElse :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
ifExpressionElse x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the then field of hydra.haskell.syntax.IfExpression
ifExpressionThen :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression
ifExpressionThen x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the condition field of hydra.haskell.syntax.IfExpression
ifExpressionWithCondition :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfExpression
ifExpressionWithCondition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the else field of hydra.haskell.syntax.IfExpression
ifExpressionWithElse :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfExpression
ifExpressionWithElse original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the then field of hydra.haskell.syntax.IfExpression
ifExpressionWithThen :: Typed.TypedTerm Syntax.IfExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfExpression
ifExpressionWithThen original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.Import
import_ :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm (Maybe Syntax.ModuleName) -> Typed.TypedTerm (Maybe Syntax.ImportSpec) -> Typed.TypedTerm Syntax.Import
import_ qualified module_ as spec =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Typed.unTypedTerm qualified)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Typed.unTypedTerm spec)}]}))
-- | DSL accessor for the as field of hydra.haskell.syntax.Import
importAs :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm (Maybe Syntax.ModuleName)
importAs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the all variant of hydra.haskell.syntax.ImportExportSubspec
importExportSubspecAll :: Typed.TypedTerm Syntax.ImportExportSubspec
importExportSubspecAll =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportExportSubspec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.ImportExportSubspec
importExportSubspecList :: Typed.TypedTerm [Syntax.Name] -> Typed.TypedTerm Syntax.ImportExportSubspec
importExportSubspecList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportExportSubspec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.haskell.syntax.ImportModifier
importModifierPattern :: Typed.TypedTerm Syntax.ImportModifier
importModifierPattern =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.haskell.syntax.ImportModifier
importModifierType :: Typed.TypedTerm Syntax.ImportModifier
importModifierType =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the module field of hydra.haskell.syntax.Import
importModule :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm Syntax.ModuleName
importModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the qualified field of hydra.haskell.syntax.Import
importQualified :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm Bool
importQualified x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "qualified")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the spec field of hydra.haskell.syntax.Import
importSpec :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm (Maybe Syntax.ImportSpec)
importSpec x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "spec")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the hiding variant of hydra.haskell.syntax.ImportSpec
importSpecHiding :: Typed.TypedTerm [Syntax.NamedImportExport] -> Typed.TypedTerm Syntax.ImportSpec
importSpecHiding x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hiding"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.ImportSpec
importSpecList :: Typed.TypedTerm [Syntax.NamedImportExport] -> Typed.TypedTerm Syntax.ImportSpec
importSpecList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the as field of hydra.haskell.syntax.Import
importWithAs :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm (Maybe Syntax.ModuleName) -> Typed.TypedTerm Syntax.Import
importWithAs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "qualified")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the module field of hydra.haskell.syntax.Import
importWithModule :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.Import
importWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "qualified")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the qualified field of hydra.haskell.syntax.Import
importWithQualified :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Import
importWithQualified original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the spec field of hydra.haskell.syntax.Import
importWithSpec :: Typed.TypedTerm Syntax.Import -> Typed.TypedTerm (Maybe Syntax.ImportSpec) -> Typed.TypedTerm Syntax.Import
importWithSpec original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "qualified")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.InfixExpression
infixExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Operator -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.InfixExpression
infixExpression lhs operator rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.haskell.syntax.InfixExpression
infixExpressionLhs :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Expression
infixExpressionLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.haskell.syntax.InfixExpression
infixExpressionOperator :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Operator
infixExpressionOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.InfixExpression
infixExpressionRhs :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Expression
infixExpressionRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.haskell.syntax.InfixExpression
infixExpressionWithLhs :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.InfixExpression
infixExpressionWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.haskell.syntax.InfixExpression
infixExpressionWithOperator :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Operator -> Typed.TypedTerm Syntax.InfixExpression
infixExpressionWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.InfixExpression
infixExpressionWithRhs :: Typed.TypedTerm Syntax.InfixExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.InfixExpression
infixExpressionWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.InfixType
infixType :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Operator -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.InfixType
infixType lhs operator rhs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.haskell.syntax.InfixType
infixTypeLhs :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Type
infixTypeLhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.haskell.syntax.InfixType
infixTypeOperator :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Operator
infixTypeOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.InfixType
infixTypeRhs :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Type
infixTypeRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the lhs field of hydra.haskell.syntax.InfixType
infixTypeWithLhs :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.InfixType
infixTypeWithLhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.haskell.syntax.InfixType
infixTypeWithOperator :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Operator -> Typed.TypedTerm Syntax.InfixType
infixTypeWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.InfixType
infixTypeWithRhs :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.InfixType
infixTypeWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.LambdaExpression
lambdaExpression :: Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.LambdaExpression
lambdaExpression bindings inner =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm inner)}]}))
-- | DSL accessor for the bindings field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionBindings :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm [Syntax.Pattern]
lambdaExpressionBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionInner :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.Expression
lambdaExpressionInner x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bindings field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionWithBindings :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.LambdaExpression
lambdaExpressionWithBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inner field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionWithInner :: Typed.TypedTerm Syntax.LambdaExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.LambdaExpression
lambdaExpressionWithInner original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.LetExpression
letExpression :: Typed.TypedTerm [Syntax.LocalBinding] -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.LetExpression
letExpression bindings inner =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm inner)}]}))
-- | DSL accessor for the bindings field of hydra.haskell.syntax.LetExpression
letExpressionBindings :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm [Syntax.LocalBinding]
letExpressionBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.LetExpression
letExpressionInner :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm Syntax.Expression
letExpressionInner x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bindings field of hydra.haskell.syntax.LetExpression
letExpressionWithBindings :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm [Syntax.LocalBinding] -> Typed.TypedTerm Syntax.LetExpression
letExpressionWithBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inner field of hydra.haskell.syntax.LetExpression
letExpressionWithInner :: Typed.TypedTerm Syntax.LetExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.LetExpression
letExpressionWithInner original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the char variant of hydra.haskell.syntax.Literal
literalChar :: Typed.TypedTerm Int -> Typed.TypedTerm Syntax.Literal
literalChar x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the double variant of hydra.haskell.syntax.Literal
literalDouble :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.Literal
literalDouble x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the float variant of hydra.haskell.syntax.Literal
literalFloat :: Typed.TypedTerm Float -> Typed.TypedTerm Syntax.Literal
literalFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the int variant of hydra.haskell.syntax.Literal
literalInt :: Typed.TypedTerm Int -> Typed.TypedTerm Syntax.Literal
literalInt x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.haskell.syntax.Literal
literalInteger :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.Literal
literalInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.haskell.syntax.Literal
literalString :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Literal
literalString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the signature variant of hydra.haskell.syntax.LocalBinding
localBindingSignature :: Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.LocalBinding
localBindingSignature x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "signature"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the value variant of hydra.haskell.syntax.LocalBinding
localBindingValue :: Typed.TypedTerm Syntax.ValueBinding -> Typed.TypedTerm Syntax.LocalBinding
localBindingValue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.LocalBindings wrapper
localBindings :: Typed.TypedTerm [Syntax.LocalBinding] -> Typed.TypedTerm Syntax.LocalBindings
localBindings x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.LocalBindings"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.Module
module_ :: Typed.TypedTerm (Maybe Syntax.ModuleHead) -> Typed.TypedTerm [Syntax.Import] -> Typed.TypedTerm [Syntax.Declaration] -> Typed.TypedTerm Syntax.Module
module_ head imports declarations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.haskell.syntax.Module
moduleDeclarations :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.Declaration]
moduleDeclarations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the head field of hydra.haskell.syntax.Module
moduleHead :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm (Maybe Syntax.ModuleHead)
moduleHead x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.ModuleHead
moduleHead2 :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm [Syntax.Export] -> Typed.TypedTerm Syntax.ModuleHead
moduleHead2 comments name exports =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Typed.unTypedTerm exports)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.ModuleHead
moduleHeadComments :: Typed.TypedTerm Syntax.ModuleHead -> Typed.TypedTerm (Maybe String)
moduleHeadComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the exports field of hydra.haskell.syntax.ModuleHead
moduleHeadExports :: Typed.TypedTerm Syntax.ModuleHead -> Typed.TypedTerm [Syntax.Export]
moduleHeadExports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
        Core.projectionFieldName = (Core.Name "exports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.ModuleHead
moduleHeadName :: Typed.TypedTerm Syntax.ModuleHead -> Typed.TypedTerm Syntax.ModuleName
moduleHeadName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.ModuleHead
moduleHeadWithComments :: Typed.TypedTerm Syntax.ModuleHead -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.ModuleHead
moduleHeadWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the exports field of hydra.haskell.syntax.ModuleHead
moduleHeadWithExports :: Typed.TypedTerm Syntax.ModuleHead -> Typed.TypedTerm [Syntax.Export] -> Typed.TypedTerm Syntax.ModuleHead
moduleHeadWithExports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.ModuleHead
moduleHeadWithName :: Typed.TypedTerm Syntax.ModuleHead -> Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm Syntax.ModuleHead
moduleHeadWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the imports field of hydra.haskell.syntax.Module
moduleImports :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.Import]
moduleImports x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.haskell.syntax.ModuleName wrapper
moduleName :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.ModuleName
moduleName x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.ModuleName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarations field of hydra.haskell.syntax.Module
moduleWithDeclarations :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.Declaration] -> Typed.TypedTerm Syntax.Module
moduleWithDeclarations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the head field of hydra.haskell.syntax.Module
moduleWithHead :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm (Maybe Syntax.ModuleHead) -> Typed.TypedTerm Syntax.Module
moduleWithHead original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the imports field of hydra.haskell.syntax.Module
moduleWithImports :: Typed.TypedTerm Syntax.Module -> Typed.TypedTerm [Syntax.Import] -> Typed.TypedTerm Syntax.Module
moduleWithImports original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the implicit variant of hydra.haskell.syntax.Name
nameImplicit :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm Syntax.Name
nameImplicit x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.haskell.syntax.Name
nameNormal :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm Syntax.Name
nameNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.NamePart wrapper
namePart :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.NamePart
namePart x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.NamePart"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.NamedImportExport
namedImportExport :: Typed.TypedTerm (Maybe Syntax.ImportModifier) -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm (Maybe Syntax.ImportExportSubspec) -> Typed.TypedTerm Syntax.NamedImportExport
namedImportExport modifier name subspec =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Typed.unTypedTerm modifier)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Typed.unTypedTerm subspec)}]}))
-- | DSL accessor for the modifier field of hydra.haskell.syntax.NamedImportExport
namedImportExportModifier :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm (Maybe Syntax.ImportModifier)
namedImportExportModifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
        Core.projectionFieldName = (Core.Name "modifier")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.NamedImportExport
namedImportExportName :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm Syntax.Name
namedImportExportName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subspec field of hydra.haskell.syntax.NamedImportExport
namedImportExportSubspec :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm (Maybe Syntax.ImportExportSubspec)
namedImportExportSubspec x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
        Core.projectionFieldName = (Core.Name "subspec")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the modifier field of hydra.haskell.syntax.NamedImportExport
namedImportExportWithModifier :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm (Maybe Syntax.ImportModifier) -> Typed.TypedTerm Syntax.NamedImportExport
namedImportExportWithModifier original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "subspec")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.NamedImportExport
namedImportExportWithName :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.NamedImportExport
namedImportExportWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "modifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "subspec")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the subspec field of hydra.haskell.syntax.NamedImportExport
namedImportExportWithSubspec :: Typed.TypedTerm Syntax.NamedImportExport -> Typed.TypedTerm (Maybe Syntax.ImportExportSubspec) -> Typed.TypedTerm Syntax.NamedImportExport
namedImportExportWithSubspec original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "modifier")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the backtick variant of hydra.haskell.syntax.Operator
operatorBacktick :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm Syntax.Operator
operatorBacktick x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "backtick"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the normal variant of hydra.haskell.syntax.Operator
operatorNormal :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm Syntax.Operator
operatorNormal x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the application variant of hydra.haskell.syntax.Pattern
patternApplication :: Typed.TypedTerm Syntax.ApplicationPattern -> Typed.TypedTerm Syntax.Pattern
patternApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the as variant of hydra.haskell.syntax.Pattern
patternAs :: Typed.TypedTerm Syntax.AsPattern -> Typed.TypedTerm Syntax.Pattern
patternAs x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.PatternField
patternField :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.PatternField
patternField name pattern =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.PatternField
patternFieldName :: Typed.TypedTerm Syntax.PatternField -> Typed.TypedTerm Syntax.Name
patternFieldName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.haskell.syntax.PatternField
patternFieldPattern :: Typed.TypedTerm Syntax.PatternField -> Typed.TypedTerm Syntax.Pattern
patternFieldPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.PatternField
patternFieldWithName :: Typed.TypedTerm Syntax.PatternField -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.PatternField
patternFieldWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pattern field of hydra.haskell.syntax.PatternField
patternFieldWithPattern :: Typed.TypedTerm Syntax.PatternField -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.PatternField
patternFieldWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the list variant of hydra.haskell.syntax.Pattern
patternList :: Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.Pattern
patternList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.haskell.syntax.Pattern
patternLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Pattern
patternLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the name variant of hydra.haskell.syntax.Pattern
patternName :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Pattern
patternName x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the record variant of hydra.haskell.syntax.Pattern
patternRecord :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm Syntax.Pattern
patternRecord x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Pattern
patternTuple :: Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.Pattern
patternTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typed variant of hydra.haskell.syntax.Pattern
patternTyped :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Pattern
patternTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.haskell.syntax.Pattern
patternWildcard :: Typed.TypedTerm Syntax.Pattern
patternWildcard =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.haskell.syntax.PositionalConstructor
positionalConstructor :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.PositionalConstructor
positionalConstructor name fields comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorComments :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm (Maybe String)
positionalConstructorComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorFields :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm [Syntax.Type]
positionalConstructorFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorName :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm Syntax.Name
positionalConstructorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorWithComments :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.PositionalConstructor
positionalConstructorWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the fields field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorWithFields :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.PositionalConstructor
positionalConstructorWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorWithName :: Typed.TypedTerm Syntax.PositionalConstructor -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.PositionalConstructor
positionalConstructorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.QualifiedName
qualifiedName :: Typed.TypedTerm [Syntax.NamePart] -> Typed.TypedTerm Syntax.NamePart -> Typed.TypedTerm Syntax.QualifiedName
qualifiedName qualifiers unqualified =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Typed.unTypedTerm qualifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Typed.unTypedTerm unqualified)}]}))
-- | DSL accessor for the qualifiers field of hydra.haskell.syntax.QualifiedName
qualifiedNameQualifiers :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm [Syntax.NamePart]
qualifiedNameQualifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
        Core.projectionFieldName = (Core.Name "qualifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the unqualified field of hydra.haskell.syntax.QualifiedName
qualifiedNameUnqualified :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm Syntax.NamePart
qualifiedNameUnqualified x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
        Core.projectionFieldName = (Core.Name "unqualified")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the qualifiers field of hydra.haskell.syntax.QualifiedName
qualifiedNameWithQualifiers :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm [Syntax.NamePart] -> Typed.TypedTerm Syntax.QualifiedName
qualifiedNameWithQualifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
              Core.projectionFieldName = (Core.Name "unqualified")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the unqualified field of hydra.haskell.syntax.QualifiedName
qualifiedNameWithUnqualified :: Typed.TypedTerm Syntax.QualifiedName -> Typed.TypedTerm Syntax.NamePart -> Typed.TypedTerm Syntax.QualifiedName
qualifiedNameWithUnqualified original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
              Core.projectionFieldName = (Core.Name "qualifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordConstructor
recordConstructor :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.Field] -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.RecordConstructor
recordConstructor name fields comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.RecordConstructor
recordConstructorComments :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm (Maybe String)
recordConstructorComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordConstructor
recordConstructorFields :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm [Syntax.Field]
recordConstructorFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.RecordConstructor
recordConstructorName :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm Syntax.Name
recordConstructorName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.RecordConstructor
recordConstructorWithComments :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.RecordConstructor
recordConstructorWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordConstructor
recordConstructorWithFields :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm [Syntax.Field] -> Typed.TypedTerm Syntax.RecordConstructor
recordConstructorWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.RecordConstructor
recordConstructorWithName :: Typed.TypedTerm Syntax.RecordConstructor -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.RecordConstructor
recordConstructorWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordExpression
recordExpression :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.FieldUpdate] -> Typed.TypedTerm Syntax.RecordExpression
recordExpression name fields =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordExpression
recordExpressionFields :: Typed.TypedTerm Syntax.RecordExpression -> Typed.TypedTerm [Syntax.FieldUpdate]
recordExpressionFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.RecordExpression
recordExpressionName :: Typed.TypedTerm Syntax.RecordExpression -> Typed.TypedTerm Syntax.Name
recordExpressionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordExpression
recordExpressionWithFields :: Typed.TypedTerm Syntax.RecordExpression -> Typed.TypedTerm [Syntax.FieldUpdate] -> Typed.TypedTerm Syntax.RecordExpression
recordExpressionWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.RecordExpression
recordExpressionWithName :: Typed.TypedTerm Syntax.RecordExpression -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.RecordExpression
recordExpressionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordPattern
recordPattern :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm [Syntax.PatternField] -> Typed.TypedTerm Syntax.RecordPattern
recordPattern name fields =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordPattern
recordPatternFields :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm [Syntax.PatternField]
recordPatternFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.RecordPattern
recordPatternName :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm Syntax.Name
recordPatternName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordPattern
recordPatternWithFields :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm [Syntax.PatternField] -> Typed.TypedTerm Syntax.RecordPattern
recordPatternWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.RecordPattern
recordPatternWithName :: Typed.TypedTerm Syntax.RecordPattern -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.RecordPattern
recordPatternWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.FieldUpdate] -> Typed.TypedTerm Syntax.RecordUpdateExpression
recordUpdateExpression inner fields =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionFields :: Typed.TypedTerm Syntax.RecordUpdateExpression -> Typed.TypedTerm [Syntax.FieldUpdate]
recordUpdateExpressionFields x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionInner :: Typed.TypedTerm Syntax.RecordUpdateExpression -> Typed.TypedTerm Syntax.Expression
recordUpdateExpressionInner x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionWithFields :: Typed.TypedTerm Syntax.RecordUpdateExpression -> Typed.TypedTerm [Syntax.FieldUpdate] -> Typed.TypedTerm Syntax.RecordUpdateExpression
recordUpdateExpressionWithFields original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the inner field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionWithInner :: Typed.TypedTerm Syntax.RecordUpdateExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.RecordUpdateExpression
recordUpdateExpressionWithInner original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.haskell.syntax.RightHandSide wrapper
rightHandSide :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.RightHandSide
rightHandSide x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.RightHandSide"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.SectionExpression
sectionExpression :: Typed.TypedTerm Syntax.Operator -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SectionExpression
sectionExpression operator expression =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.haskell.syntax.SectionExpression
sectionExpressionExpression :: Typed.TypedTerm Syntax.SectionExpression -> Typed.TypedTerm Syntax.Expression
sectionExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.haskell.syntax.SectionExpression
sectionExpressionOperator :: Typed.TypedTerm Syntax.SectionExpression -> Typed.TypedTerm Syntax.Operator
sectionExpressionOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.haskell.syntax.SectionExpression
sectionExpressionWithExpression :: Typed.TypedTerm Syntax.SectionExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SectionExpression
sectionExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the operator field of hydra.haskell.syntax.SectionExpression
sectionExpressionWithOperator :: Typed.TypedTerm Syntax.SectionExpression -> Typed.TypedTerm Syntax.Operator -> Typed.TypedTerm Syntax.SectionExpression
sectionExpressionWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.SimpleValueBinding
simpleValueBinding :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.RightHandSide -> Typed.TypedTerm (Maybe Syntax.LocalBindings) -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.SimpleValueBinding
simpleValueBinding pattern rhs localBindings comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Typed.unTypedTerm localBindings)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingComments :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm (Maybe String)
simpleValueBindingComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the localBindings field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingLocalBindings :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm (Maybe Syntax.LocalBindings)
simpleValueBindingLocalBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "localBindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the pattern field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingPattern :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm Syntax.Pattern
simpleValueBindingPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingRhs :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm Syntax.RightHandSide
simpleValueBindingRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithComments :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.SimpleValueBinding
simpleValueBindingWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "localBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the localBindings field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithLocalBindings :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm (Maybe Syntax.LocalBindings) -> Typed.TypedTerm Syntax.SimpleValueBinding
simpleValueBindingWithLocalBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the pattern field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithPattern :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.SimpleValueBinding
simpleValueBindingWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "localBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithRhs :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm Syntax.RightHandSide -> Typed.TypedTerm Syntax.SimpleValueBinding
simpleValueBindingWithRhs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "localBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.haskell.syntax.Statement wrapper
statement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
statement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.Statement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the application variant of hydra.haskell.syntax.Type
typeApplication :: Typed.TypedTerm Syntax.ApplicationType -> Typed.TypedTerm Syntax.Type
typeApplication x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the ctx variant of hydra.haskell.syntax.Type
typeCtx :: Typed.TypedTerm Syntax.ConstrainedType -> Typed.TypedTerm Syntax.Type
typeCtx x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctx"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.haskell.syntax.Type
typeFunction :: Typed.TypedTerm Syntax.FunctionType -> Typed.TypedTerm Syntax.Type
typeFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the infix variant of hydra.haskell.syntax.Type
typeInfix :: Typed.TypedTerm Syntax.InfixType -> Typed.TypedTerm Syntax.Type
typeInfix x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.Type
typeList :: Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.Type
typeList x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.TypeSignature
typeSignature :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeSignature
typeSignature name type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.TypeSignature
typeSignatureName :: Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.Name
typeSignatureName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypeSignature
typeSignatureType :: Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.Type
typeSignatureType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.TypeSignature
typeSignatureWithName :: Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.TypeSignature
typeSignatureWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypeSignature
typeSignatureWithType :: Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeSignature
typeSignatureWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclaration :: Typed.TypedTerm Syntax.DeclarationHead -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclaration name type_ comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationComments :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm (Maybe String)
typeSynonymDeclarationComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationName :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm Syntax.DeclarationHead
typeSynonymDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationType :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm Syntax.Type
typeSynonymDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithComments :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithName :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm Syntax.DeclarationHead -> Typed.TypedTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithType :: Typed.TypedTerm Syntax.TypeSynonymDeclaration -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Type
typeTuple :: Typed.TypedTerm [Syntax.Type] -> Typed.TypedTerm Syntax.Type
typeTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.haskell.syntax.Type
typeVariable :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Type
typeVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.TypedBinding
typedBinding :: Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.ValueBinding -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.TypedBinding
typedBinding typeSignature valueBinding comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Typed.unTypedTerm typeSignature)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Typed.unTypedTerm valueBinding)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.TypedBinding
typedBindingComments :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm (Maybe String)
typedBindingComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeSignature field of hydra.haskell.syntax.TypedBinding
typedBindingTypeSignature :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm Syntax.TypeSignature
typedBindingTypeSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
        Core.projectionFieldName = (Core.Name "typeSignature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the valueBinding field of hydra.haskell.syntax.TypedBinding
typedBindingValueBinding :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm Syntax.ValueBinding
typedBindingValueBinding x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
        Core.projectionFieldName = (Core.Name "valueBinding")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.TypedBinding
typedBindingWithComments :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Syntax.TypedBinding
typedBindingWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "typeSignature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "valueBinding")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeSignature field of hydra.haskell.syntax.TypedBinding
typedBindingWithTypeSignature :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm Syntax.TypeSignature -> Typed.TypedTerm Syntax.TypedBinding
typedBindingWithTypeSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "valueBinding")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the valueBinding field of hydra.haskell.syntax.TypedBinding
typedBindingWithValueBinding :: Typed.TypedTerm Syntax.TypedBinding -> Typed.TypedTerm Syntax.ValueBinding -> Typed.TypedTerm Syntax.TypedBinding
typedBindingWithValueBinding original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "typeSignature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.TypedExpression
typedExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedExpression
typedExpression inner type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.TypedExpression
typedExpressionInner :: Typed.TypedTerm Syntax.TypedExpression -> Typed.TypedTerm Syntax.Expression
typedExpressionInner x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypedExpression
typedExpressionType :: Typed.TypedTerm Syntax.TypedExpression -> Typed.TypedTerm Syntax.Type
typedExpressionType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the inner field of hydra.haskell.syntax.TypedExpression
typedExpressionWithInner :: Typed.TypedTerm Syntax.TypedExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypedExpression
typedExpressionWithInner original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypedExpression
typedExpressionWithType :: Typed.TypedTerm Syntax.TypedExpression -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedExpression
typedExpressionWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.TypedPattern
typedPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedPattern
typedPattern inner type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.TypedPattern
typedPatternInner :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Pattern
typedPatternInner x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypedPattern
typedPatternType :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Type
typedPatternType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the inner field of hydra.haskell.syntax.TypedPattern
typedPatternWithInner :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.TypedPattern
typedPatternWithInner original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypedPattern
typedPatternWithType :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Type -> Typed.TypedTerm Syntax.TypedPattern
typedPatternWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.haskell.syntax.CaseRhs
unCaseRhs :: Typed.TypedTerm Syntax.CaseRhs -> Typed.TypedTerm Syntax.Expression
unCaseRhs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.CaseRhs")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.DerivingClause
unDerivingClause :: Typed.TypedTerm Syntax.DerivingClause -> Typed.TypedTerm [Syntax.Name]
unDerivingClause x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.DerivingClause")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.LocalBindings
unLocalBindings :: Typed.TypedTerm Syntax.LocalBindings -> Typed.TypedTerm [Syntax.LocalBinding]
unLocalBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.LocalBindings")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.ModuleName
unModuleName :: Typed.TypedTerm Syntax.ModuleName -> Typed.TypedTerm String
unModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.ModuleName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.NamePart
unNamePart :: Typed.TypedTerm Syntax.NamePart -> Typed.TypedTerm String
unNamePart x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.NamePart")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.RightHandSide
unRightHandSide :: Typed.TypedTerm Syntax.RightHandSide -> Typed.TypedTerm Syntax.Expression
unRightHandSide x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.RightHandSide")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.Statement
unStatement :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.Expression
unStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.Statement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.Variable
unVariable :: Typed.TypedTerm Syntax.Variable -> Typed.TypedTerm Syntax.Name
unVariable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.Variable")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the simple variant of hydra.haskell.syntax.ValueBinding
valueBindingSimple :: Typed.TypedTerm Syntax.SimpleValueBinding -> Typed.TypedTerm Syntax.ValueBinding
valueBindingSimple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ValueBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.Variable wrapper
variable :: Typed.TypedTerm Syntax.Name -> Typed.TypedTerm Syntax.Variable
variable x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.Variable"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
