-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.haskell.syntax

module Hydra.Dsl.Haskell.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.haskell.syntax.Alternative
alternative :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.CaseRhs -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.Alternative
alternative pattern rhs binds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Phantoms.unTTerm binds)}]}))
-- | DSL accessor for the binds field of hydra.haskell.syntax.Alternative
alternativeBinds :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm (Maybe Syntax.LocalBindings)
alternativeBinds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
        Core.projectionFieldName = (Core.Name "binds")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pattern field of hydra.haskell.syntax.Alternative
alternativePattern :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.Pattern
alternativePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.Alternative
alternativeRhs :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.CaseRhs
alternativeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the binds field of hydra.haskell.syntax.Alternative
alternativeWithBinds :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.Alternative
alternativeWithBinds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the pattern field of hydra.haskell.syntax.Alternative
alternativeWithPattern :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Alternative
alternativeWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "binds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.Alternative
alternativeWithRhs :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.CaseRhs -> Phantoms.TTerm Syntax.Alternative
alternativeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
              Core.projectionFieldName = (Core.Name "binds")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHead :: Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHead function operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))
-- | DSL accessor for the function field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadFunction :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead
applicationDeclarationHeadFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operand field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadOperand :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.Variable
applicationDeclarationHeadOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the function field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithFunction :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operand field of hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithOperand :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationExpression
applicationExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ApplicationExpression
applicationExpression function argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionArgument :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression
applicationExpressionArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the function field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionFunction :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression
applicationExpressionFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the argument field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionWithArgument :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ApplicationExpression
applicationExpressionWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the function field of hydra.haskell.syntax.ApplicationExpression
applicationExpressionWithFunction :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ApplicationExpression
applicationExpressionWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationPattern
applicationPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ApplicationPattern
applicationPattern name args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))
-- | DSL accessor for the args field of hydra.haskell.syntax.ApplicationPattern
applicationPatternArgs :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm [Syntax.Pattern]
applicationPatternArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
        Core.projectionFieldName = (Core.Name "args")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.ApplicationPattern
applicationPatternName :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm Syntax.Name
applicationPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the args field of hydra.haskell.syntax.ApplicationPattern
applicationPatternWithArgs :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ApplicationPattern
applicationPatternWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.ApplicationPattern
applicationPatternWithName :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ApplicationPattern
applicationPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
              Core.projectionFieldName = (Core.Name "args")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.ApplicationType
applicationType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplicationType
applicationType context argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))
-- | DSL accessor for the argument field of hydra.haskell.syntax.ApplicationType
applicationTypeArgument :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type
applicationTypeArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the context field of hydra.haskell.syntax.ApplicationType
applicationTypeContext :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type
applicationTypeContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
        Core.projectionFieldName = (Core.Name "context")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the argument field of hydra.haskell.syntax.ApplicationType
applicationTypeWithArgument :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplicationType
applicationTypeWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the context field of hydra.haskell.syntax.ApplicationType
applicationTypeWithContext :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplicationType
applicationTypeWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.AsPattern
asPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.AsPattern
asPattern name inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.AsPattern
asPatternInner :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern
asPatternInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.AsPattern
asPatternName :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Name
asPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the inner field of hydra.haskell.syntax.AsPattern
asPatternWithInner :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.AsPattern
asPatternWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.AsPattern
asPatternWithName :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.AsPattern
asPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.CaseExpression
caseExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.Alternative] -> Phantoms.TTerm Syntax.CaseExpression
caseExpression case_ alternatives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm alternatives)}]}))
-- | DSL accessor for the alternatives field of hydra.haskell.syntax.CaseExpression
caseExpressionAlternatives :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.Alternative]
caseExpressionAlternatives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "alternatives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the case field of hydra.haskell.syntax.CaseExpression
caseExpressionCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
caseExpressionCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
        Core.projectionFieldName = (Core.Name "case")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the alternatives field of hydra.haskell.syntax.CaseExpression
caseExpressionWithAlternatives :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.Alternative] -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithAlternatives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "case")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the case field of hydra.haskell.syntax.CaseExpression
caseExpressionWithCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
              Core.projectionFieldName = (Core.Name "alternatives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.haskell.syntax.CaseRhs wrapper
caseRhs :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseRhs
caseRhs x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.CaseRhs"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.ClassConstraint
classConstraint :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ClassConstraint
classConstraint name types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.ClassConstraint
classConstraintName :: Phantoms.TTerm Syntax.ClassConstraint -> Phantoms.TTerm Syntax.Name
classConstraintName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the types field of hydra.haskell.syntax.ClassConstraint
classConstraintTypes :: Phantoms.TTerm Syntax.ClassConstraint -> Phantoms.TTerm [Syntax.Type]
classConstraintTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.ClassConstraint
classConstraintWithName :: Phantoms.TTerm Syntax.ClassConstraint -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ClassConstraint
classConstraintWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the types field of hydra.haskell.syntax.ClassConstraint
classConstraintWithTypes :: Phantoms.TTerm Syntax.ClassConstraint -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ClassConstraint
classConstraintWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.ConstrainedType
constrainedType :: Phantoms.TTerm Syntax.Constraint -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ConstrainedType
constrainedType ctx type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Phantoms.unTTerm ctx)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the ctx field of hydra.haskell.syntax.ConstrainedType
constrainedTypeCtx :: Phantoms.TTerm Syntax.ConstrainedType -> Phantoms.TTerm Syntax.Constraint
constrainedTypeCtx x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
        Core.projectionFieldName = (Core.Name "ctx")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.ConstrainedType
constrainedTypeType :: Phantoms.TTerm Syntax.ConstrainedType -> Phantoms.TTerm Syntax.Type
constrainedTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ctx field of hydra.haskell.syntax.ConstrainedType
constrainedTypeWithCtx :: Phantoms.TTerm Syntax.ConstrainedType -> Phantoms.TTerm Syntax.Constraint -> Phantoms.TTerm Syntax.ConstrainedType
constrainedTypeWithCtx original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.ConstrainedType
constrainedTypeWithType :: Phantoms.TTerm Syntax.ConstrainedType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ConstrainedType
constrainedTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
              Core.projectionFieldName = (Core.Name "ctx")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the class variant of hydra.haskell.syntax.Constraint
constraintClass :: Phantoms.TTerm Syntax.ClassConstraint -> Phantoms.TTerm Syntax.Constraint
constraintClass x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Constraint
constraintTuple :: Phantoms.TTerm [Syntax.Constraint] -> Phantoms.TTerm Syntax.Constraint
constraintTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constraint"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ordinary variant of hydra.haskell.syntax.Constructor
constructorOrdinary :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm Syntax.Constructor
constructorOrdinary x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the record variant of hydra.haskell.syntax.Constructor
constructorRecord :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm Syntax.Constructor
constructorRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.DataDeclaration
dataDeclaration :: Phantoms.TTerm Syntax.DataKeyword -> Phantoms.TTerm [Syntax.Constraint] -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm [Syntax.Constructor] -> Phantoms.TTerm [Syntax.DerivingClause] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclaration keyword context head constructors deriving_ comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Phantoms.unTTerm keyword)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm constructors)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Phantoms.unTTerm deriving_)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.DataDeclaration
dataDeclarationComments :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm (Maybe String)
dataDeclarationComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the constructors field of hydra.haskell.syntax.DataDeclaration
dataDeclarationConstructors :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Constructor]
dataDeclarationConstructors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "constructors")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the context field of hydra.haskell.syntax.DataDeclaration
dataDeclarationContext :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Constraint]
dataDeclarationContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "context")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the deriving field of hydra.haskell.syntax.DataDeclaration
dataDeclarationDeriving :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.DerivingClause]
dataDeclarationDeriving x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "deriving")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the head field of hydra.haskell.syntax.DataDeclaration
dataDeclarationHead :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DeclarationHead
dataDeclarationHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the keyword field of hydra.haskell.syntax.DataDeclaration
dataDeclarationKeyword :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DataKeyword
dataDeclarationKeyword x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
        Core.projectionFieldName = (Core.Name "keyword")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithComments :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the constructors field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithConstructors :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Constructor] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithConstructors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the context field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithContext :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Constraint] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the deriving field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithDeriving :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.DerivingClause] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithDeriving original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the head field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithHead :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "keyword")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the keyword field of hydra.haskell.syntax.DataDeclaration
dataDeclarationWithKeyword :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DataKeyword -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithKeyword original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "constructors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "deriving")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the data variant of hydra.haskell.syntax.DataKeyword
dataKeywordData :: Phantoms.TTerm Syntax.DataKeyword
dataKeywordData =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DataKeyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the newtype variant of hydra.haskell.syntax.DataKeyword
dataKeywordNewtype :: Phantoms.TTerm Syntax.DataKeyword
dataKeywordNewtype =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DataKeyword"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newtype"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the data variant of hydra.haskell.syntax.Declaration
declarationData :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationData x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the application variant of hydra.haskell.syntax.DeclarationHead
declarationHeadApplication :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead
declarationHeadApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the simple variant of hydra.haskell.syntax.DeclarationHead
declarationHeadSimple :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DeclarationHead
declarationHeadSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.haskell.syntax.Declaration
declarationType :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typedBinding variant of hydra.haskell.syntax.Declaration
declarationTypedBinding :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.Declaration
declarationTypedBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the valueBinding variant of hydra.haskell.syntax.Declaration
declarationValueBinding :: Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.Declaration
declarationValueBinding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valueBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.DerivingClause wrapper
derivingClause :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.DerivingClause
derivingClause x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.DerivingClause"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the declaration variant of hydra.haskell.syntax.Export
exportDeclaration :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm Syntax.Export
exportDeclaration x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the module variant of hydra.haskell.syntax.Export
exportModule :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Export
exportModule x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the application variant of hydra.haskell.syntax.Expression
expressionApplication :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression
expressionApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the case variant of hydra.haskell.syntax.Expression
expressionCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
expressionCase x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the constructRecord variant of hydra.haskell.syntax.Expression
expressionConstructRecord :: Phantoms.TTerm Syntax.RecordExpression -> Phantoms.TTerm Syntax.Expression
expressionConstructRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructRecord"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the do variant of hydra.haskell.syntax.Expression
expressionDo :: Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.Expression
expressionDo x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the if variant of hydra.haskell.syntax.Expression
expressionIf :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
expressionIf x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the infixApplication variant of hydra.haskell.syntax.Expression
expressionInfixApplication :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Expression
expressionInfixApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infixApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the lambda variant of hydra.haskell.syntax.Expression
expressionLambda :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the leftSection variant of hydra.haskell.syntax.Expression
expressionLeftSection :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression
expressionLeftSection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftSection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the let variant of hydra.haskell.syntax.Expression
expressionLet :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression
expressionLet x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.Expression
expressionList :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression
expressionList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.haskell.syntax.Expression
expressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the rightSection variant of hydra.haskell.syntax.Expression
expressionRightSection :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression
expressionRightSection x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightSection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Expression
expressionTuple :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression
expressionTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typeSignature variant of hydra.haskell.syntax.Expression
expressionTypeSignature :: Phantoms.TTerm Syntax.TypedExpression -> Phantoms.TTerm Syntax.Expression
expressionTypeSignature x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSignature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the updateRecord variant of hydra.haskell.syntax.Expression
expressionUpdateRecord :: Phantoms.TTerm Syntax.RecordUpdateExpression -> Phantoms.TTerm Syntax.Expression
expressionUpdateRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "updateRecord"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.haskell.syntax.Expression
expressionVariable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression
expressionVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.Field
field :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Field
field name type_ comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.Field
fieldComments :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe String)
fieldComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.Field
fieldName :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Name
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.Field
fieldType :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Type
fieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.FieldUpdate
fieldUpdate :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldUpdate
fieldUpdate name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.FieldUpdate
fieldUpdateName :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Name
fieldUpdateName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.haskell.syntax.FieldUpdate
fieldUpdateValue :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Expression
fieldUpdateValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.FieldUpdate
fieldUpdateWithName :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.FieldUpdate
fieldUpdateWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.haskell.syntax.FieldUpdate
fieldUpdateWithValue :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldUpdate
fieldUpdateWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the comments field of hydra.haskell.syntax.Field
fieldWithComments :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.Field
fieldWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.Field
fieldWithName :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.Field
fieldWithType :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Field
fieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Field"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.FunctionType
functionType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionType domain codomain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm codomain)}]}))
-- | DSL accessor for the codomain field of hydra.haskell.syntax.FunctionType
functionTypeCodomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
functionTypeCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "codomain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the domain field of hydra.haskell.syntax.FunctionType
functionTypeDomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
functionTypeDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
        Core.projectionFieldName = (Core.Name "domain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the codomain field of hydra.haskell.syntax.FunctionType
functionTypeWithCodomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
              Core.projectionFieldName = (Core.Name "domain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the domain field of hydra.haskell.syntax.FunctionType
functionTypeWithDomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.IfExpression
ifExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpression condition then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
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
-- | DSL accessor for the condition field of hydra.haskell.syntax.IfExpression
ifExpressionCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the else field of hydra.haskell.syntax.IfExpression
ifExpressionElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the then field of hydra.haskell.syntax.IfExpression
ifExpressionThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
        Core.projectionFieldName = (Core.Name "then")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the condition field of hydra.haskell.syntax.IfExpression
ifExpressionWithCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the else field of hydra.haskell.syntax.IfExpression
ifExpressionWithElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "then")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the then field of hydra.haskell.syntax.IfExpression
ifExpressionWithThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.Import
import_ :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm (Maybe Syntax.ImportSpec) -> Phantoms.TTerm Syntax.Import
import_ qualified module_ as spec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Phantoms.unTTerm qualified)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm spec)}]}))
-- | DSL accessor for the as field of hydra.haskell.syntax.Import
importAs :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.ModuleName)
importAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the all variant of hydra.haskell.syntax.ImportExportSubspec
importExportSubspecAll :: Phantoms.TTerm Syntax.ImportExportSubspec
importExportSubspecAll =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportExportSubspec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.ImportExportSubspec
importExportSubspecList :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.ImportExportSubspec
importExportSubspecList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportExportSubspec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the pattern variant of hydra.haskell.syntax.ImportModifier
importModifierPattern :: Phantoms.TTerm Syntax.ImportModifier
importModifierPattern =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the type variant of hydra.haskell.syntax.ImportModifier
importModifierType :: Phantoms.TTerm Syntax.ImportModifier
importModifierType =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the module field of hydra.haskell.syntax.Import
importModule :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ModuleName
importModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the qualified field of hydra.haskell.syntax.Import
importQualified :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Bool
importQualified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "qualified")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the spec field of hydra.haskell.syntax.Import
importSpec :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.ImportSpec)
importSpec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
        Core.projectionFieldName = (Core.Name "spec")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the hiding variant of hydra.haskell.syntax.ImportSpec
importSpecHiding :: Phantoms.TTerm [Syntax.NamedImportExport] -> Phantoms.TTerm Syntax.ImportSpec
importSpecHiding x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hiding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.ImportSpec
importSpecList :: Phantoms.TTerm [Syntax.NamedImportExport] -> Phantoms.TTerm Syntax.ImportSpec
importSpecList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL updater for the as field of hydra.haskell.syntax.Import
importWithAs :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm Syntax.Import
importWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "qualified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the module field of hydra.haskell.syntax.Import
importWithModule :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Import
importWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "qualified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the qualified field of hydra.haskell.syntax.Import
importWithQualified :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Import
importWithQualified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "spec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the spec field of hydra.haskell.syntax.Import
importWithSpec :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.ImportSpec) -> Phantoms.TTerm Syntax.Import
importWithSpec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "qualified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Import"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.InfixExpression
infixExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.InfixExpression
infixExpression lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.haskell.syntax.InfixExpression
infixExpressionLhs :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Expression
infixExpressionLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operator field of hydra.haskell.syntax.InfixExpression
infixExpressionOperator :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Operator
infixExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.InfixExpression
infixExpressionRhs :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Expression
infixExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.haskell.syntax.InfixExpression
infixExpressionWithLhs :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.InfixExpression
infixExpressionWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.haskell.syntax.InfixExpression
infixExpressionWithOperator :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.InfixExpression
infixExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.InfixExpression
infixExpressionWithRhs :: Phantoms.TTerm Syntax.InfixExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.InfixExpression
infixExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.InfixType
infixType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.InfixType
infixType lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))
-- | DSL accessor for the lhs field of hydra.haskell.syntax.InfixType
infixTypeLhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type
infixTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operator field of hydra.haskell.syntax.InfixType
infixTypeOperator :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Operator
infixTypeOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.InfixType
infixTypeRhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type
infixTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the lhs field of hydra.haskell.syntax.InfixType
infixTypeWithLhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.InfixType
infixTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.haskell.syntax.InfixType
infixTypeWithOperator :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.InfixType
infixTypeWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.InfixType
infixTypeWithRhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.InfixType
infixTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.LambdaExpression
lambdaExpression :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpression bindings inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))
-- | DSL accessor for the bindings field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionBindings :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm [Syntax.Pattern]
lambdaExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionInner :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression
lambdaExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bindings field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionWithBindings :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the inner field of hydra.haskell.syntax.LambdaExpression
lambdaExpressionWithInner :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.LetExpression
letExpression :: Phantoms.TTerm [Syntax.LocalBinding] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LetExpression
letExpression bindings inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))
-- | DSL accessor for the bindings field of hydra.haskell.syntax.LetExpression
letExpressionBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LocalBinding]
letExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.LetExpression
letExpressionInner :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression
letExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bindings field of hydra.haskell.syntax.LetExpression
letExpressionWithBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LocalBinding] -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the inner field of hydra.haskell.syntax.LetExpression
letExpressionWithInner :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the char variant of hydra.haskell.syntax.Literal
literalChar :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Literal
literalChar x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the double variant of hydra.haskell.syntax.Literal
literalDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Literal
literalDouble x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the float variant of hydra.haskell.syntax.Literal
literalFloat :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the int variant of hydra.haskell.syntax.Literal
literalInt :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Literal
literalInt x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the integer variant of hydra.haskell.syntax.Literal
literalInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the string variant of hydra.haskell.syntax.Literal
literalString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the signature variant of hydra.haskell.syntax.LocalBinding
localBindingSignature :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.LocalBinding
localBindingSignature x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "signature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the value variant of hydra.haskell.syntax.LocalBinding
localBindingValue :: Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.LocalBinding
localBindingValue x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.LocalBindings wrapper
localBindings :: Phantoms.TTerm [Syntax.LocalBinding] -> Phantoms.TTerm Syntax.LocalBindings
localBindings x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.LocalBindings"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.Module
module_ :: Phantoms.TTerm (Maybe Syntax.ModuleHead) -> Phantoms.TTerm [Syntax.Import] -> Phantoms.TTerm [Syntax.Declaration] -> Phantoms.TTerm Syntax.Module
module_ head imports declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm imports)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.haskell.syntax.Module
moduleDeclarations :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.Declaration]
moduleDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the head field of hydra.haskell.syntax.Module
moduleHead :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm (Maybe Syntax.ModuleHead)
moduleHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.ModuleHead
moduleHead2 :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm [Syntax.Export] -> Phantoms.TTerm Syntax.ModuleHead
moduleHead2 comments name exports =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm exports)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.ModuleHead
moduleHeadComments :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm (Maybe String)
moduleHeadComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the exports field of hydra.haskell.syntax.ModuleHead
moduleHeadExports :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm [Syntax.Export]
moduleHeadExports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
        Core.projectionFieldName = (Core.Name "exports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.ModuleHead
moduleHeadName :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm Syntax.ModuleName
moduleHeadName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.ModuleHead
moduleHeadWithComments :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ModuleHead
moduleHeadWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the exports field of hydra.haskell.syntax.ModuleHead
moduleHeadWithExports :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm [Syntax.Export] -> Phantoms.TTerm Syntax.ModuleHead
moduleHeadWithExports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.ModuleHead
moduleHeadWithName :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.ModuleHead
moduleHeadWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
              Core.projectionFieldName = (Core.Name "exports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the imports field of hydra.haskell.syntax.Module
moduleImports :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.Import]
moduleImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
        Core.projectionFieldName = (Core.Name "imports")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.haskell.syntax.ModuleName wrapper
moduleName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ModuleName
moduleName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.ModuleName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL updater for the declarations field of hydra.haskell.syntax.Module
moduleWithDeclarations :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.Declaration] -> Phantoms.TTerm Syntax.Module
moduleWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the head field of hydra.haskell.syntax.Module
moduleWithHead :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm (Maybe Syntax.ModuleHead) -> Phantoms.TTerm Syntax.Module
moduleWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "imports")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the imports field of hydra.haskell.syntax.Module
moduleWithImports :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.Import] -> Phantoms.TTerm Syntax.Module
moduleWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.Module"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the implicit variant of hydra.haskell.syntax.Name
nameImplicit :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Name
nameImplicit x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the normal variant of hydra.haskell.syntax.Name
nameNormal :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Name
nameNormal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.NamePart wrapper
namePart :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.NamePart
namePart x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.NamePart"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.NamedImportExport
namedImportExport :: Phantoms.TTerm (Maybe Syntax.ImportModifier) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.ImportExportSubspec) -> Phantoms.TTerm Syntax.NamedImportExport
namedImportExport modifier name subspec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Phantoms.unTTerm modifier)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Phantoms.unTTerm subspec)}]}))
-- | DSL accessor for the modifier field of hydra.haskell.syntax.NamedImportExport
namedImportExportModifier :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm (Maybe Syntax.ImportModifier)
namedImportExportModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
        Core.projectionFieldName = (Core.Name "modifier")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.NamedImportExport
namedImportExportName :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm Syntax.Name
namedImportExportName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subspec field of hydra.haskell.syntax.NamedImportExport
namedImportExportSubspec :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm (Maybe Syntax.ImportExportSubspec)
namedImportExportSubspec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
        Core.projectionFieldName = (Core.Name "subspec")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the modifier field of hydra.haskell.syntax.NamedImportExport
namedImportExportWithModifier :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm (Maybe Syntax.ImportModifier) -> Phantoms.TTerm Syntax.NamedImportExport
namedImportExportWithModifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "subspec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.NamedImportExport
namedImportExportWithName :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.NamedImportExport
namedImportExportWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "modifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "subspec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the subspec field of hydra.haskell.syntax.NamedImportExport
namedImportExportWithSubspec :: Phantoms.TTerm Syntax.NamedImportExport -> Phantoms.TTerm (Maybe Syntax.ImportExportSubspec) -> Phantoms.TTerm Syntax.NamedImportExport
namedImportExportWithSubspec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "modifier")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the backtick variant of hydra.haskell.syntax.Operator
operatorBacktick :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Operator
operatorBacktick x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "backtick"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the normal variant of hydra.haskell.syntax.Operator
operatorNormal :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Operator
operatorNormal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the application variant of hydra.haskell.syntax.Pattern
patternApplication :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm Syntax.Pattern
patternApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the as variant of hydra.haskell.syntax.Pattern
patternAs :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern
patternAs x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.PatternField
patternField :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.PatternField
patternField name pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.PatternField
patternFieldName :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Name
patternFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pattern field of hydra.haskell.syntax.PatternField
patternFieldPattern :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Pattern
patternFieldPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.PatternField
patternFieldWithName :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PatternField
patternFieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pattern field of hydra.haskell.syntax.PatternField
patternFieldWithPattern :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.PatternField
patternFieldWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the list variant of hydra.haskell.syntax.Pattern
patternList :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the literal variant of hydra.haskell.syntax.Pattern
patternLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Pattern
patternLiteral x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the name variant of hydra.haskell.syntax.Pattern
patternName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern
patternName x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the record variant of hydra.haskell.syntax.Pattern
patternRecord :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Pattern
patternRecord x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Pattern
patternTuple :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the typed variant of hydra.haskell.syntax.Pattern
patternTyped :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Pattern
patternTyped x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the wildcard variant of hydra.haskell.syntax.Pattern
patternWildcard :: Phantoms.TTerm Syntax.Pattern
patternWildcard =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.haskell.syntax.PositionalConstructor
positionalConstructor :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.PositionalConstructor
positionalConstructor name fields comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorComments :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm (Maybe String)
positionalConstructorComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorFields :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm [Syntax.Type]
positionalConstructorFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorName :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm Syntax.Name
positionalConstructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorWithComments :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.PositionalConstructor
positionalConstructorWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the fields field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorWithFields :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.PositionalConstructor
positionalConstructorWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.PositionalConstructor
positionalConstructorWithName :: Phantoms.TTerm Syntax.PositionalConstructor -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PositionalConstructor
positionalConstructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.QualifiedName
qualifiedName :: Phantoms.TTerm [Syntax.NamePart] -> Phantoms.TTerm Syntax.NamePart -> Phantoms.TTerm Syntax.QualifiedName
qualifiedName qualifiers unqualified =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Phantoms.unTTerm unqualified)}]}))
-- | DSL accessor for the qualifiers field of hydra.haskell.syntax.QualifiedName
qualifiedNameQualifiers :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm [Syntax.NamePart]
qualifiedNameQualifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
        Core.projectionFieldName = (Core.Name "qualifiers")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the unqualified field of hydra.haskell.syntax.QualifiedName
qualifiedNameUnqualified :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.NamePart
qualifiedNameUnqualified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
        Core.projectionFieldName = (Core.Name "unqualified")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the qualifiers field of hydra.haskell.syntax.QualifiedName
qualifiedNameWithQualifiers :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm [Syntax.NamePart] -> Phantoms.TTerm Syntax.QualifiedName
qualifiedNameWithQualifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
              Core.projectionFieldName = (Core.Name "unqualified")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the unqualified field of hydra.haskell.syntax.QualifiedName
qualifiedNameWithUnqualified :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.NamePart -> Phantoms.TTerm Syntax.QualifiedName
qualifiedNameWithUnqualified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
              Core.projectionFieldName = (Core.Name "qualifiers")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordConstructor
recordConstructor :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Field] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructor name fields comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.RecordConstructor
recordConstructorComments :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm (Maybe String)
recordConstructorComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordConstructor
recordConstructorFields :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm [Syntax.Field]
recordConstructorFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.RecordConstructor
recordConstructorName :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm Syntax.Name
recordConstructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.RecordConstructor
recordConstructorWithComments :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructorWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordConstructor
recordConstructorWithFields :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm [Syntax.Field] -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructorWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.RecordConstructor
recordConstructorWithName :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordExpression
recordExpression :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.RecordExpression
recordExpression name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordExpression
recordExpressionFields :: Phantoms.TTerm Syntax.RecordExpression -> Phantoms.TTerm [Syntax.FieldUpdate]
recordExpressionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.RecordExpression
recordExpressionName :: Phantoms.TTerm Syntax.RecordExpression -> Phantoms.TTerm Syntax.Name
recordExpressionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordExpression
recordExpressionWithFields :: Phantoms.TTerm Syntax.RecordExpression -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.RecordExpression
recordExpressionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.RecordExpression
recordExpressionWithName :: Phantoms.TTerm Syntax.RecordExpression -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RecordExpression
recordExpressionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordPattern
recordPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.PatternField] -> Phantoms.TTerm Syntax.RecordPattern
recordPattern name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordPattern
recordPatternFields :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm [Syntax.PatternField]
recordPatternFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.RecordPattern
recordPatternName :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Name
recordPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordPattern
recordPatternWithFields :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm [Syntax.PatternField] -> Phantoms.TTerm Syntax.RecordPattern
recordPatternWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.RecordPattern
recordPatternWithName :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RecordPattern
recordPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.RecordUpdateExpression
recordUpdateExpression inner fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))
-- | DSL accessor for the fields field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionFields :: Phantoms.TTerm Syntax.RecordUpdateExpression -> Phantoms.TTerm [Syntax.FieldUpdate]
recordUpdateExpressionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionInner :: Phantoms.TTerm Syntax.RecordUpdateExpression -> Phantoms.TTerm Syntax.Expression
recordUpdateExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the fields field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionWithFields :: Phantoms.TTerm Syntax.RecordUpdateExpression -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.RecordUpdateExpression
recordUpdateExpressionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the inner field of hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpressionWithInner :: Phantoms.TTerm Syntax.RecordUpdateExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RecordUpdateExpression
recordUpdateExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.haskell.syntax.RightHandSide wrapper
rightHandSide :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RightHandSide
rightHandSide x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.RightHandSide"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.haskell.syntax.SectionExpression
sectionExpression :: Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SectionExpression
sectionExpression operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))
-- | DSL accessor for the expression field of hydra.haskell.syntax.SectionExpression
sectionExpressionExpression :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression
sectionExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the operator field of hydra.haskell.syntax.SectionExpression
sectionExpressionOperator :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Operator
sectionExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the expression field of hydra.haskell.syntax.SectionExpression
sectionExpressionWithExpression :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SectionExpression
sectionExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the operator field of hydra.haskell.syntax.SectionExpression
sectionExpressionWithOperator :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.SectionExpression
sectionExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.SimpleValueBinding
simpleValueBinding :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.RightHandSide -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBinding pattern rhs localBindings comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Phantoms.unTTerm localBindings)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingComments :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm (Maybe String)
simpleValueBindingComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the localBindings field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingLocalBindings :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm (Maybe Syntax.LocalBindings)
simpleValueBindingLocalBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "localBindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the pattern field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingPattern :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.Pattern
simpleValueBindingPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the rhs field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingRhs :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.RightHandSide
simpleValueBindingRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithComments :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "localBindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the localBindings field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithLocalBindings :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithLocalBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the pattern field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithPattern :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "localBindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the rhs field of hydra.haskell.syntax.SimpleValueBinding
simpleValueBindingWithRhs :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.RightHandSide -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "localBindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.haskell.syntax.Statement wrapper
statement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement
statement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.Statement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the application variant of hydra.haskell.syntax.Type
typeApplication :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type
typeApplication x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the ctx variant of hydra.haskell.syntax.Type
typeCtx :: Phantoms.TTerm Syntax.ConstrainedType -> Phantoms.TTerm Syntax.Type
typeCtx x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctx"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the function variant of hydra.haskell.syntax.Type
typeFunction :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
typeFunction x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the infix variant of hydra.haskell.syntax.Type
typeInfix :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type
typeInfix x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the list variant of hydra.haskell.syntax.Type
typeList :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type
typeList x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.TypeSignature
typeSignature :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSignature
typeSignature name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the name field of hydra.haskell.syntax.TypeSignature
typeSignatureName :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Name
typeSignatureName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypeSignature
typeSignatureType :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Type
typeSignatureType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.haskell.syntax.TypeSignature
typeSignatureWithName :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TypeSignature
typeSignatureWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypeSignature
typeSignatureWithType :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSignature
typeSignatureWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclaration :: Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclaration name type_ comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationComments :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm (Maybe String)
typeSynonymDeclarationComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationName :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm Syntax.DeclarationHead
typeSynonymDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationType :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm Syntax.Type
typeSynonymDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithComments :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithName :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithType :: Phantoms.TTerm Syntax.TypeSynonymDeclaration -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSynonymDeclaration
typeSynonymDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the tuple variant of hydra.haskell.syntax.Type
typeTuple :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type
typeTuple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the variable variant of hydra.haskell.syntax.Type
typeVariable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type
typeVariable x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.haskell.syntax.TypedBinding
typedBinding :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypedBinding
typedBinding typeSignature valueBinding comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Phantoms.unTTerm typeSignature)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Phantoms.unTTerm valueBinding)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.haskell.syntax.TypedBinding
typedBindingComments :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm (Maybe String)
typedBindingComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeSignature field of hydra.haskell.syntax.TypedBinding
typedBindingTypeSignature :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.TypeSignature
typedBindingTypeSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
        Core.projectionFieldName = (Core.Name "typeSignature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the valueBinding field of hydra.haskell.syntax.TypedBinding
typedBindingValueBinding :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.ValueBinding
typedBindingValueBinding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
        Core.projectionFieldName = (Core.Name "valueBinding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comments field of hydra.haskell.syntax.TypedBinding
typedBindingWithComments :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.TypedBinding
typedBindingWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "typeSignature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "valueBinding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the typeSignature field of hydra.haskell.syntax.TypedBinding
typedBindingWithTypeSignature :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.TypedBinding
typedBindingWithTypeSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "valueBinding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the valueBinding field of hydra.haskell.syntax.TypedBinding
typedBindingWithValueBinding :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.TypedBinding
typedBindingWithValueBinding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "typeSignature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.haskell.syntax.TypedExpression
typedExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedExpression
typedExpression inner type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.TypedExpression
typedExpressionInner :: Phantoms.TTerm Syntax.TypedExpression -> Phantoms.TTerm Syntax.Expression
typedExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypedExpression
typedExpressionType :: Phantoms.TTerm Syntax.TypedExpression -> Phantoms.TTerm Syntax.Type
typedExpressionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the inner field of hydra.haskell.syntax.TypedExpression
typedExpressionWithInner :: Phantoms.TTerm Syntax.TypedExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypedExpression
typedExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypedExpression
typedExpressionWithType :: Phantoms.TTerm Syntax.TypedExpression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedExpression
typedExpressionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.haskell.syntax.TypedPattern
typedPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedPattern
typedPattern inner type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))
-- | DSL accessor for the inner field of hydra.haskell.syntax.TypedPattern
typedPatternInner :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Pattern
typedPatternInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
        Core.projectionFieldName = (Core.Name "inner")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.haskell.syntax.TypedPattern
typedPatternType :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Type
typedPatternType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the inner field of hydra.haskell.syntax.TypedPattern
typedPatternWithInner :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.TypedPattern
typedPatternWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.haskell.syntax.TypedPattern
typedPatternWithType :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedPattern
typedPatternWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
              Core.projectionFieldName = (Core.Name "inner")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the body of hydra.haskell.syntax.CaseRhs
unCaseRhs :: Phantoms.TTerm Syntax.CaseRhs -> Phantoms.TTerm Syntax.Expression
unCaseRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.CaseRhs")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.DerivingClause
unDerivingClause :: Phantoms.TTerm Syntax.DerivingClause -> Phantoms.TTerm [Syntax.Name]
unDerivingClause x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.DerivingClause")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.LocalBindings
unLocalBindings :: Phantoms.TTerm Syntax.LocalBindings -> Phantoms.TTerm [Syntax.LocalBinding]
unLocalBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.LocalBindings")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.ModuleName
unModuleName :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm String
unModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.ModuleName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.NamePart
unNamePart :: Phantoms.TTerm Syntax.NamePart -> Phantoms.TTerm String
unNamePart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.NamePart")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.RightHandSide
unRightHandSide :: Phantoms.TTerm Syntax.RightHandSide -> Phantoms.TTerm Syntax.Expression
unRightHandSide x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.RightHandSide")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.Statement
unStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Expression
unStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.Statement")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.haskell.syntax.Variable
unVariable :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.Name
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.haskell.syntax.Variable")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the simple variant of hydra.haskell.syntax.ValueBinding
valueBindingSimple :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.ValueBinding
valueBindingSimple x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ValueBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.haskell.syntax.Variable wrapper
variable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
