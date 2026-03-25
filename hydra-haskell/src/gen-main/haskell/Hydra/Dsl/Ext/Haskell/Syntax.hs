-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.haskell.syntax

module Hydra.Dsl.Ext.Haskell.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Haskell.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alternative :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.CaseRhs -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.Alternative
alternative pattern rhs binds =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
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

alternativeBinds :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm (Maybe Syntax.LocalBindings)
alternativeBinds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
        Core.projectionField = (Core.Name "binds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativePattern :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.Pattern
alternativePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativeRhs :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.CaseRhs
alternativeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternativeWithBinds :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.Alternative
alternativeWithBinds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

alternativeWithPattern :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Alternative
alternativeWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
              Core.projectionField = (Core.Name "binds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alternativeWithRhs :: Phantoms.TTerm Syntax.Alternative -> Phantoms.TTerm Syntax.CaseRhs -> Phantoms.TTerm Syntax.Alternative
alternativeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Alternative"),
              Core.projectionField = (Core.Name "binds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationDeclarationHead :: Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHead function operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))

applicationDeclarationHeadFunction :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead
applicationDeclarationHeadFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationDeclarationHeadOperand :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.Variable
applicationDeclarationHeadOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
        Core.projectionField = (Core.Name "operand")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationDeclarationHeadWithFunction :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
              Core.projectionField = (Core.Name "operand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationDeclarationHeadWithOperand :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.ApplicationDeclarationHead
applicationDeclarationHeadWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationDeclarationHead"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applicationExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ApplicationExpression
applicationExpression function argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))

applicationExpressionArgument :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression
applicationExpressionArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationExpressionFunction :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression
applicationExpressionFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
        Core.projectionField = (Core.Name "function")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationExpressionWithArgument :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ApplicationExpression
applicationExpressionWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
              Core.projectionField = (Core.Name "function")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applicationExpressionWithFunction :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ApplicationExpression
applicationExpressionWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationExpression"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ApplicationPattern
applicationPattern name args =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm args)}]}))

applicationPatternArgs :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm [Syntax.Pattern]
applicationPatternArgs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
        Core.projectionField = (Core.Name "args")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationPatternName :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm Syntax.Name
applicationPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationPatternWithArgs :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ApplicationPattern
applicationPatternWithArgs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applicationPatternWithName :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ApplicationPattern
applicationPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationPattern"),
              Core.projectionField = (Core.Name "args")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

applicationType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplicationType
applicationType context argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm context)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))

applicationTypeArgument :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type
applicationTypeArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationTypeContext :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type
applicationTypeContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
        Core.projectionField = (Core.Name "context")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

applicationTypeWithArgument :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplicationType
applicationTypeWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

applicationTypeWithContext :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ApplicationType
applicationTypeWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ApplicationType"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

asPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.AsPattern
asPattern name inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))

asPatternInner :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern
asPatternInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asPatternName :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Name
asPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

asPatternWithInner :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.AsPattern
asPatternWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

asPatternWithName :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.AsPattern
asPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.AsPattern"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assertionClass :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.Assertion
assertionClass x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "class"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

assertionTuple :: Phantoms.TTerm [Syntax.Assertion] -> Phantoms.TTerm Syntax.Assertion
assertionTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Assertion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.Alternative] -> Phantoms.TTerm Syntax.CaseExpression
caseExpression case_ alternatives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm alternatives)}]}))

caseExpressionAlternatives :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.Alternative]
caseExpressionAlternatives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
        Core.projectionField = (Core.Name "alternatives")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
caseExpressionCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
        Core.projectionField = (Core.Name "case")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionWithAlternatives :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm [Syntax.Alternative] -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithAlternatives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "case")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseExpressionWithCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseExpression
caseExpressionWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseExpression"),
              Core.projectionField = (Core.Name "alternatives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseRhs :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CaseRhs
caseRhs x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.CaseRhs"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

classAssertion :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ClassAssertion
classAssertion name types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))

classAssertionName :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.Name
classAssertionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionTypes :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm [Syntax.Type]
classAssertionTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
        Core.projectionField = (Core.Name "types")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classAssertionWithName :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ClassAssertion
classAssertionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "types")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classAssertionWithTypes :: Phantoms.TTerm Syntax.ClassAssertion -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.ClassAssertion
classAssertionWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ClassAssertion"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constructRecordExpression :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.ConstructRecordExpression
constructRecordExpression name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

constructRecordExpressionFields :: Phantoms.TTerm Syntax.ConstructRecordExpression -> Phantoms.TTerm [Syntax.FieldUpdate]
constructRecordExpressionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructRecordExpressionName :: Phantoms.TTerm Syntax.ConstructRecordExpression -> Phantoms.TTerm Syntax.Name
constructRecordExpressionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructRecordExpressionWithFields :: Phantoms.TTerm Syntax.ConstructRecordExpression -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.ConstructRecordExpression
constructRecordExpressionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

constructRecordExpressionWithName :: Phantoms.TTerm Syntax.ConstructRecordExpression -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ConstructRecordExpression
constructRecordExpressionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructRecordExpression"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorOrdinary :: Phantoms.TTerm Syntax.OrdinaryConstructor -> Phantoms.TTerm Syntax.Constructor
constructorOrdinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ordinary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constructorRecord :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm Syntax.Constructor
constructorRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Constructor"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

constructorWithComments :: Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ConstructorWithComments
constructorWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

constructorWithCommentsBody :: Phantoms.TTerm Syntax.ConstructorWithComments -> Phantoms.TTerm Syntax.Constructor
constructorWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorWithCommentsComments :: Phantoms.TTerm Syntax.ConstructorWithComments -> Phantoms.TTerm (Maybe String)
constructorWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

constructorWithCommentsWithBody :: Phantoms.TTerm Syntax.ConstructorWithComments -> Phantoms.TTerm Syntax.Constructor -> Phantoms.TTerm Syntax.ConstructorWithComments
constructorWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

constructorWithCommentsWithComments :: Phantoms.TTerm Syntax.ConstructorWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ConstructorWithComments
constructorWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ConstructorWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

contextType :: Phantoms.TTerm Syntax.Assertion -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ContextType
contextType ctx type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Phantoms.unTTerm ctx)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

contextTypeCtx :: Phantoms.TTerm Syntax.ContextType -> Phantoms.TTerm Syntax.Assertion
contextTypeCtx x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
        Core.projectionField = (Core.Name "ctx")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextTypeType :: Phantoms.TTerm Syntax.ContextType -> Phantoms.TTerm Syntax.Type
contextTypeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextTypeWithCtx :: Phantoms.TTerm Syntax.ContextType -> Phantoms.TTerm Syntax.Assertion -> Phantoms.TTerm Syntax.ContextType
contextTypeWithCtx original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

contextTypeWithType :: Phantoms.TTerm Syntax.ContextType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.ContextType
contextTypeWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ContextType"),
              Core.projectionField = (Core.Name "ctx")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataDeclaration :: Phantoms.TTerm Syntax.DataOrNewtype -> Phantoms.TTerm [Syntax.Assertion] -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm [Syntax.ConstructorWithComments] -> Phantoms.TTerm [Syntax.Deriving] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclaration keyword context head constructors deriving_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
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
          Core.fieldTerm = (Phantoms.unTTerm deriving_)}]}))

dataDeclarationConstructors :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.ConstructorWithComments]
dataDeclarationConstructors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
        Core.projectionField = (Core.Name "constructors")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationContext :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Assertion]
dataDeclarationContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
        Core.projectionField = (Core.Name "context")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationDeriving :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Deriving]
dataDeclarationDeriving x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
        Core.projectionField = (Core.Name "deriving")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationHead :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DeclarationHead
dataDeclarationHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
        Core.projectionField = (Core.Name "head")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationKeyword :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DataOrNewtype
dataDeclarationKeyword x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
        Core.projectionField = (Core.Name "keyword")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataDeclarationWithConstructors :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.ConstructorWithComments] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithConstructors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithContext :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Assertion] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithDeriving :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm [Syntax.Deriving] -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithDeriving original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataDeclarationWithHead :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "keyword")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataDeclarationWithKeyword :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.DataOrNewtype -> Phantoms.TTerm Syntax.DataDeclaration
dataDeclarationWithKeyword original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "constructors")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataDeclaration"),
              Core.projectionField = (Core.Name "deriving")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataOrNewtypeData :: Phantoms.TTerm Syntax.DataOrNewtype
dataOrNewtypeData =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataOrNewtype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = Core.TermUnit}}))

dataOrNewtypeNewtype :: Phantoms.TTerm Syntax.DataOrNewtype
dataOrNewtypeNewtype =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DataOrNewtype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "newtype"),
        Core.fieldTerm = Core.TermUnit}}))

declarationData :: Phantoms.TTerm Syntax.DataDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationData x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationHeadApplication :: Phantoms.TTerm Syntax.ApplicationDeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead
declarationHeadApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationHeadParens :: Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.DeclarationHead
declarationHeadParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationHeadSimple :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.DeclarationHead
declarationHeadSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationHead"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationType :: Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm Syntax.Declaration
declarationType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationTypedBinding :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.Declaration
declarationTypedBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typedBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationValueBinding :: Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.Declaration
declarationValueBinding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Declaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "valueBinding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

declarationWithComments :: Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DeclarationWithComments
declarationWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

declarationWithCommentsBody :: Phantoms.TTerm Syntax.DeclarationWithComments -> Phantoms.TTerm Syntax.Declaration
declarationWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationWithCommentsComments :: Phantoms.TTerm Syntax.DeclarationWithComments -> Phantoms.TTerm (Maybe String)
declarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

declarationWithCommentsWithBody :: Phantoms.TTerm Syntax.DeclarationWithComments -> Phantoms.TTerm Syntax.Declaration -> Phantoms.TTerm Syntax.DeclarationWithComments
declarationWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

declarationWithCommentsWithComments :: Phantoms.TTerm Syntax.DeclarationWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.DeclarationWithComments
declarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.DeclarationWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

deriving_ :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.Deriving
deriving_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.Deriving"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

exportDeclaration :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm Syntax.Export
exportDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportModule :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Export
exportModule x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Export"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionApplication :: Phantoms.TTerm Syntax.ApplicationExpression -> Phantoms.TTerm Syntax.Expression
expressionApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCase :: Phantoms.TTerm Syntax.CaseExpression -> Phantoms.TTerm Syntax.Expression
expressionCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionConstructRecord :: Phantoms.TTerm Syntax.ConstructRecordExpression -> Phantoms.TTerm Syntax.Expression
expressionConstructRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructRecord"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionDo :: Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.Expression
expressionDo x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "do"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIf :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
expressionIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionInfixApplication :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Expression
expressionInfixApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infixApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLambda :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression
expressionLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLeftSection :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression
expressionLeftSection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftSection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLet :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression
expressionLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionList :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression
expressionList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionParens :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionPrefixApplication :: Phantoms.TTerm Syntax.PrefixApplicationExpression -> Phantoms.TTerm Syntax.Expression
expressionPrefixApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "prefixApplication"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionRightSection :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression
expressionRightSection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightSection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTuple :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression
expressionTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionTypeSignature :: Phantoms.TTerm Syntax.TypeSignatureExpression -> Phantoms.TTerm Syntax.Expression
expressionTypeSignature x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeSignature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionUpdateRecord :: Phantoms.TTerm Syntax.UpdateRecordExpression -> Phantoms.TTerm Syntax.Expression
expressionUpdateRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "updateRecord"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionVariable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression
expressionVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

field :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Field
field name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

fieldName :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Name
fieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldType :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Type
fieldType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldUpdate :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldUpdate
fieldUpdate name value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

fieldUpdateName :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Name
fieldUpdateName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldUpdateValue :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Expression
fieldUpdateValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldUpdateWithName :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.FieldUpdate
fieldUpdateWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldUpdateWithValue :: Phantoms.TTerm Syntax.FieldUpdate -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FieldUpdate
fieldUpdateWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldUpdate"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldWithComments :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FieldWithComments
fieldWithComments field comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

fieldWithCommentsComments :: Phantoms.TTerm Syntax.FieldWithComments -> Phantoms.TTerm (Maybe String)
fieldWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithCommentsField :: Phantoms.TTerm Syntax.FieldWithComments -> Phantoms.TTerm Syntax.Field
fieldWithCommentsField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
        Core.projectionField = (Core.Name "field")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fieldWithCommentsWithComments :: Phantoms.TTerm Syntax.FieldWithComments -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.FieldWithComments
fieldWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
              Core.projectionField = (Core.Name "field")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fieldWithCommentsWithField :: Phantoms.TTerm Syntax.FieldWithComments -> Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.FieldWithComments
fieldWithCommentsWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FieldWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithName :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Field
fieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fieldWithType :: Phantoms.TTerm Syntax.Field -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Field
fieldWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Field"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionType domain codomain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm domain)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm codomain)}]}))

functionTypeCodomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
functionTypeCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
        Core.projectionField = (Core.Name "codomain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeDomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
functionTypeDomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
        Core.projectionField = (Core.Name "domain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeWithCodomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
              Core.projectionField = (Core.Name "domain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionTypeWithDomain :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.FunctionType
functionTypeWithDomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.FunctionType"),
              Core.projectionField = (Core.Name "codomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpression condition then_ else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
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

ifExpressionCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
        Core.projectionField = (Core.Name "else")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression
ifExpressionThen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
        Core.projectionField = (Core.Name "then")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifExpressionWithCondition :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifExpressionWithElse :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
              Core.projectionField = (Core.Name "then")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifExpressionWithThen :: Phantoms.TTerm Syntax.IfExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfExpression
ifExpressionWithThen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.IfExpression"),
              Core.projectionField = (Core.Name "else")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

import_ :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm (Maybe Syntax.SpecImport) -> Phantoms.TTerm Syntax.Import
import_ qualified module_ as spec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
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

importAs :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.ModuleName)
importAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
        Core.projectionField = (Core.Name "as")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpec :: Phantoms.TTerm (Maybe Syntax.ImportModifier) -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm (Maybe Syntax.SubspecImportExportSpec) -> Phantoms.TTerm Syntax.ImportExportSpec
importExportSpec modifier name subspec =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
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

importExportSpecModifier :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm (Maybe Syntax.ImportModifier)
importExportSpecModifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
        Core.projectionField = (Core.Name "modifier")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpecName :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm Syntax.Name
importExportSpecName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpecSubspec :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm (Maybe Syntax.SubspecImportExportSpec)
importExportSpecSubspec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
        Core.projectionField = (Core.Name "subspec")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importExportSpecWithModifier :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm (Maybe Syntax.ImportModifier) -> Phantoms.TTerm Syntax.ImportExportSpec
importExportSpecWithModifier original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
              Core.projectionField = (Core.Name "subspec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importExportSpecWithName :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.ImportExportSpec
importExportSpecWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
              Core.projectionField = (Core.Name "modifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
              Core.projectionField = (Core.Name "subspec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importExportSpecWithSubspec :: Phantoms.TTerm Syntax.ImportExportSpec -> Phantoms.TTerm (Maybe Syntax.SubspecImportExportSpec) -> Phantoms.TTerm Syntax.ImportExportSpec
importExportSpecWithSubspec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
              Core.projectionField = (Core.Name "modifier")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportExportSpec"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importModifierPattern :: Phantoms.TTerm Syntax.ImportModifier
importModifierPattern =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = Core.TermUnit}}))

importModifierType :: Phantoms.TTerm Syntax.ImportModifier
importModifierType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ImportModifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = Core.TermUnit}}))

importModule :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ModuleName
importModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
        Core.projectionField = (Core.Name "module")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importQualified :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Bool
importQualified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
        Core.projectionField = (Core.Name "qualified")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importSpec :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.SpecImport)
importSpec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
        Core.projectionField = (Core.Name "spec")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importWithAs :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.ModuleName) -> Phantoms.TTerm Syntax.Import
importWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "qualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importWithModule :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.Import
importWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "qualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importWithQualified :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Import
importWithQualified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "spec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importWithSpec :: Phantoms.TTerm Syntax.Import -> Phantoms.TTerm (Maybe Syntax.SpecImport) -> Phantoms.TTerm Syntax.Import
importWithSpec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "qualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "module")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Import"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

infixApplicationExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.InfixApplicationExpression
infixApplicationExpression lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
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

infixApplicationExpressionLhs :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Expression
infixApplicationExpressionLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixApplicationExpressionOperator :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Operator
infixApplicationExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixApplicationExpressionRhs :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Expression
infixApplicationExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixApplicationExpressionWithLhs :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.InfixApplicationExpression
infixApplicationExpressionWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixApplicationExpressionWithOperator :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.InfixApplicationExpression
infixApplicationExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixApplicationExpressionWithRhs :: Phantoms.TTerm Syntax.InfixApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.InfixApplicationExpression
infixApplicationExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixApplicationExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

infixType :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.InfixType
infixType lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
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

infixTypeLhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type
infixTypeLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixTypeOperator :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Operator
infixTypeOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixTypeRhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Operator
infixTypeRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

infixTypeWithLhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.InfixType
infixTypeWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixTypeWithOperator :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.InfixType
infixTypeWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

infixTypeWithRhs :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.InfixType
infixTypeWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.InfixType"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lambdaExpression :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpression bindings inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))

lambdaExpressionBindings :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm [Syntax.Pattern]
lambdaExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionInner :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression
lambdaExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lambdaExpressionWithBindings :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

lambdaExpressionWithInner :: Phantoms.TTerm Syntax.LambdaExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LambdaExpression
lambdaExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LambdaExpression"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letExpression :: Phantoms.TTerm [Syntax.LocalBinding] -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LetExpression
letExpression bindings inner =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)}]}))

letExpressionBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LocalBinding]
letExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionInner :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression
letExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionWithBindings :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm [Syntax.LocalBinding] -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letExpressionWithInner :: Phantoms.TTerm Syntax.LetExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.LetExpression
letExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LetExpression"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalChar :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Literal
literalChar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.Literal
literalDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalFloat :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.Literal
literalFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInt :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Literal
literalInt x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Literal
literalInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localBindingSignature :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.LocalBinding
localBindingSignature x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "signature"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localBindingValue :: Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.LocalBinding
localBindingValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.LocalBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

localBindings :: Phantoms.TTerm [Syntax.LocalBinding] -> Phantoms.TTerm Syntax.LocalBindings
localBindings x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.LocalBindings"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

module_ :: Phantoms.TTerm (Maybe Syntax.ModuleHead) -> Phantoms.TTerm [Syntax.Import] -> Phantoms.TTerm [Syntax.DeclarationWithComments] -> Phantoms.TTerm Syntax.Module
module_ head imports declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
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

moduleDeclarations :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.DeclarationWithComments]
moduleDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
        Core.projectionField = (Core.Name "declarations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHead :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm (Maybe Syntax.ModuleHead)
moduleHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
        Core.projectionField = (Core.Name "head")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadComments :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm (Maybe String)
moduleHeadComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadExports :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm [Syntax.Export]
moduleHeadExports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
        Core.projectionField = (Core.Name "exports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadName :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm Syntax.ModuleName
moduleHeadName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleHeadWithComments :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ModuleHead
moduleHeadWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleHeadWithExports :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm [Syntax.Export] -> Phantoms.TTerm Syntax.ModuleHead
moduleHeadWithExports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleHeadWithName :: Phantoms.TTerm Syntax.ModuleHead -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm Syntax.ModuleHead
moduleHeadWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
              Core.projectionField = (Core.Name "exports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleHead_ :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm [Syntax.Export] -> Phantoms.TTerm Syntax.ModuleHead
moduleHead_ comments name exports =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleHead"),
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

moduleImports :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.Import]
moduleImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
        Core.projectionField = (Core.Name "imports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.ModuleName
moduleName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.ModuleName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

moduleWithDeclarations :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.DeclarationWithComments] -> Phantoms.TTerm Syntax.Module
moduleWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleWithHead :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm (Maybe Syntax.ModuleHead) -> Phantoms.TTerm Syntax.Module
moduleWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
              Core.projectionField = (Core.Name "imports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleWithImports :: Phantoms.TTerm Syntax.Module -> Phantoms.TTerm [Syntax.Import] -> Phantoms.TTerm Syntax.Module
moduleWithImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
              Core.projectionField = (Core.Name "head")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Module"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nameImplicit :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Name
nameImplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nameNormal :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Name
nameNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nameParens :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Name
nameParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Name"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

namePart :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.NamePart
namePart x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.NamePart"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

operatorBacktick :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Operator
operatorBacktick x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "backtick"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

operatorNormal :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.Operator
operatorNormal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Operator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

ordinaryConstructor :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.OrdinaryConstructor
ordinaryConstructor name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

ordinaryConstructorFields :: Phantoms.TTerm Syntax.OrdinaryConstructor -> Phantoms.TTerm [Syntax.Type]
ordinaryConstructorFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryConstructorName :: Phantoms.TTerm Syntax.OrdinaryConstructor -> Phantoms.TTerm Syntax.Name
ordinaryConstructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ordinaryConstructorWithFields :: Phantoms.TTerm Syntax.OrdinaryConstructor -> Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.OrdinaryConstructor
ordinaryConstructorWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ordinaryConstructorWithName :: Phantoms.TTerm Syntax.OrdinaryConstructor -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.OrdinaryConstructor
ordinaryConstructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.OrdinaryConstructor"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternApplication :: Phantoms.TTerm Syntax.ApplicationPattern -> Phantoms.TTerm Syntax.Pattern
patternApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternAs :: Phantoms.TTerm Syntax.AsPattern -> Phantoms.TTerm Syntax.Pattern
patternAs x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "as"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternField :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.PatternField
patternField name pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

patternFieldName :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Name
patternFieldName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternFieldPattern :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Pattern
patternFieldPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternFieldWithName :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.PatternField
patternFieldWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternFieldWithPattern :: Phantoms.TTerm Syntax.PatternField -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.PatternField
patternFieldWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PatternField"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternList :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Pattern
patternLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternName :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Pattern
patternName x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternParens :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Pattern
patternParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternRecord :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Pattern
patternRecord x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "record"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTuple :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.Pattern
patternTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternTyped :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Pattern
patternTyped x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternWildcard :: Phantoms.TTerm Syntax.Pattern
patternWildcard =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "wildcard"),
        Core.fieldTerm = Core.TermUnit}}))

prefixApplicationExpression :: Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PrefixApplicationExpression
prefixApplicationExpression operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

prefixApplicationExpressionOperator :: Phantoms.TTerm Syntax.PrefixApplicationExpression -> Phantoms.TTerm Syntax.Operator
prefixApplicationExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

prefixApplicationExpressionRhs :: Phantoms.TTerm Syntax.PrefixApplicationExpression -> Phantoms.TTerm Syntax.Expression
prefixApplicationExpressionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

prefixApplicationExpressionWithOperator :: Phantoms.TTerm Syntax.PrefixApplicationExpression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.PrefixApplicationExpression
prefixApplicationExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

prefixApplicationExpressionWithRhs :: Phantoms.TTerm Syntax.PrefixApplicationExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PrefixApplicationExpression
prefixApplicationExpressionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.PrefixApplicationExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedName :: Phantoms.TTerm [Syntax.NamePart] -> Phantoms.TTerm Syntax.NamePart -> Phantoms.TTerm Syntax.QualifiedName
qualifiedName qualifiers unqualified =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Phantoms.unTTerm qualifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Phantoms.unTTerm unqualified)}]}))

qualifiedNameQualifiers :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm [Syntax.NamePart]
qualifiedNameQualifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
        Core.projectionField = (Core.Name "qualifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameUnqualified :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.NamePart
qualifiedNameUnqualified x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
        Core.projectionField = (Core.Name "unqualified")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameWithQualifiers :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm [Syntax.NamePart] -> Phantoms.TTerm Syntax.QualifiedName
qualifiedNameWithQualifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
              Core.projectionField = (Core.Name "unqualified")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

qualifiedNameWithUnqualified :: Phantoms.TTerm Syntax.QualifiedName -> Phantoms.TTerm Syntax.NamePart -> Phantoms.TTerm Syntax.QualifiedName
qualifiedNameWithUnqualified original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.QualifiedName"),
              Core.projectionField = (Core.Name "qualifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordConstructor :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.FieldWithComments] -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructor name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

recordConstructorFields :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm [Syntax.FieldWithComments]
recordConstructorFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordConstructorName :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm Syntax.Name
recordConstructorName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordConstructorWithFields :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm [Syntax.FieldWithComments] -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructorWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordConstructorWithName :: Phantoms.TTerm Syntax.RecordConstructor -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RecordConstructor
recordConstructorWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordConstructor"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

recordPattern :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm [Syntax.PatternField] -> Phantoms.TTerm Syntax.RecordPattern
recordPattern name fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

recordPatternFields :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm [Syntax.PatternField]
recordPatternFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordPatternName :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Name
recordPatternName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recordPatternWithFields :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm [Syntax.PatternField] -> Phantoms.TTerm Syntax.RecordPattern
recordPatternWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recordPatternWithName :: Phantoms.TTerm Syntax.RecordPattern -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.RecordPattern
recordPatternWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.RecordPattern"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rightHandSide :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.RightHandSide
rightHandSide x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.RightHandSide"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

sectionExpression :: Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SectionExpression
sectionExpression operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

sectionExpressionExpression :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression
sectionExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sectionExpressionOperator :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Operator
sectionExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sectionExpressionWithExpression :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SectionExpression
sectionExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sectionExpressionWithOperator :: Phantoms.TTerm Syntax.SectionExpression -> Phantoms.TTerm Syntax.Operator -> Phantoms.TTerm Syntax.SectionExpression
sectionExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SectionExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

simpleValueBinding :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.RightHandSide -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBinding pattern rhs localBindings =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Phantoms.unTTerm localBindings)}]}))

simpleValueBindingLocalBindings :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm (Maybe Syntax.LocalBindings)
simpleValueBindingLocalBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
        Core.projectionField = (Core.Name "localBindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleValueBindingPattern :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.Pattern
simpleValueBindingPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleValueBindingRhs :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.RightHandSide
simpleValueBindingRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleValueBindingWithLocalBindings :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm (Maybe Syntax.LocalBindings) -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithLocalBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

simpleValueBindingWithPattern :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
              Core.projectionField = (Core.Name "localBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

simpleValueBindingWithRhs :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.RightHandSide -> Phantoms.TTerm Syntax.SimpleValueBinding
simpleValueBindingWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SimpleValueBinding"),
              Core.projectionField = (Core.Name "localBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

specImportHiding :: Phantoms.TTerm [Syntax.ImportExportSpec] -> Phantoms.TTerm Syntax.SpecImport
specImportHiding x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SpecImport"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hiding"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

specImportList :: Phantoms.TTerm [Syntax.ImportExportSpec] -> Phantoms.TTerm Syntax.SpecImport
specImportList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SpecImport"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement
statement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.Statement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

subspecImportExportSpecAll :: Phantoms.TTerm Syntax.SubspecImportExportSpec
subspecImportExportSpecAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SubspecImportExportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

subspecImportExportSpecList :: Phantoms.TTerm [Syntax.Name] -> Phantoms.TTerm Syntax.SubspecImportExportSpec
subspecImportExportSpecList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.SubspecImportExportSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeApplication :: Phantoms.TTerm Syntax.ApplicationType -> Phantoms.TTerm Syntax.Type
typeApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeCtx :: Phantoms.TTerm Syntax.ContextType -> Phantoms.TTerm Syntax.Type
typeCtx x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ctx"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeDeclaration :: Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDeclaration
typeDeclaration name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeDeclarationName :: Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm Syntax.DeclarationHead
typeDeclarationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationType :: Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm Syntax.Type
typeDeclarationType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDeclarationWithName :: Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm Syntax.DeclarationHead -> Phantoms.TTerm Syntax.TypeDeclaration
typeDeclarationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDeclarationWithType :: Phantoms.TTerm Syntax.TypeDeclaration -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeDeclaration
typeDeclarationWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeDeclaration"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeFunction :: Phantoms.TTerm Syntax.FunctionType -> Phantoms.TTerm Syntax.Type
typeFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeInfix :: Phantoms.TTerm Syntax.InfixType -> Phantoms.TTerm Syntax.Type
typeInfix x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infix"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeList :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type
typeList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeParens :: Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.Type
typeParens x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parens"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSignature :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSignature
typeSignature name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeSignatureExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSignatureExpression
typeSignatureExpression inner type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeSignatureExpressionInner :: Phantoms.TTerm Syntax.TypeSignatureExpression -> Phantoms.TTerm Syntax.Expression
typeSignatureExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureExpressionType :: Phantoms.TTerm Syntax.TypeSignatureExpression -> Phantoms.TTerm Syntax.Type
typeSignatureExpressionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureExpressionWithInner :: Phantoms.TTerm Syntax.TypeSignatureExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.TypeSignatureExpression
typeSignatureExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeSignatureExpressionWithType :: Phantoms.TTerm Syntax.TypeSignatureExpression -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSignatureExpression
typeSignatureExpressionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignatureExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeSignatureName :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Name
typeSignatureName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureType :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Type
typeSignatureType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeSignatureWithName :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.TypeSignature
typeSignatureWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeSignatureWithType :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypeSignature
typeSignatureWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypeSignature"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeTuple :: Phantoms.TTerm [Syntax.Type] -> Phantoms.TTerm Syntax.Type
typeTuple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeVariable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Type
typeVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typedBinding :: Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.TypedBinding
typedBinding typeSignature valueBinding =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Phantoms.unTTerm typeSignature)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Phantoms.unTTerm valueBinding)}]}))

typedBindingTypeSignature :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.TypeSignature
typedBindingTypeSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
        Core.projectionField = (Core.Name "typeSignature")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedBindingValueBinding :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.ValueBinding
typedBindingValueBinding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
        Core.projectionField = (Core.Name "valueBinding")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedBindingWithTypeSignature :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.TypeSignature -> Phantoms.TTerm Syntax.TypedBinding
typedBindingWithTypeSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
              Core.projectionField = (Core.Name "valueBinding")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedBindingWithValueBinding :: Phantoms.TTerm Syntax.TypedBinding -> Phantoms.TTerm Syntax.ValueBinding -> Phantoms.TTerm Syntax.TypedBinding
typedBindingWithValueBinding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedBinding"),
              Core.projectionField = (Core.Name "typeSignature")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typedPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedPattern
typedPattern inner type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typedPatternInner :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Pattern
typedPatternInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedPatternType :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Type
typedPatternType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typedPatternWithInner :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.TypedPattern
typedPatternWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typedPatternWithType :: Phantoms.TTerm Syntax.TypedPattern -> Phantoms.TTerm Syntax.Type -> Phantoms.TTerm Syntax.TypedPattern
typedPatternWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.TypedPattern"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unCaseRhs :: Phantoms.TTerm Syntax.CaseRhs -> Phantoms.TTerm Syntax.Expression
unCaseRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.CaseRhs")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDeriving :: Phantoms.TTerm Syntax.Deriving -> Phantoms.TTerm [Syntax.Name]
unDeriving x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.Deriving")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unLocalBindings :: Phantoms.TTerm Syntax.LocalBindings -> Phantoms.TTerm [Syntax.LocalBinding]
unLocalBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.LocalBindings")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unModuleName :: Phantoms.TTerm Syntax.ModuleName -> Phantoms.TTerm String
unModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.ModuleName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNamePart :: Phantoms.TTerm Syntax.NamePart -> Phantoms.TTerm String
unNamePart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.NamePart")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRightHandSide :: Phantoms.TTerm Syntax.RightHandSide -> Phantoms.TTerm Syntax.Expression
unRightHandSide x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.RightHandSide")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Expression
unStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.Statement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVariable :: Phantoms.TTerm Syntax.Variable -> Phantoms.TTerm Syntax.Name
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.haskell.syntax.Variable")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

updateRecordExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.UpdateRecordExpression
updateRecordExpression inner fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm inner)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

updateRecordExpressionFields :: Phantoms.TTerm Syntax.UpdateRecordExpression -> Phantoms.TTerm [Syntax.FieldUpdate]
updateRecordExpressionFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

updateRecordExpressionInner :: Phantoms.TTerm Syntax.UpdateRecordExpression -> Phantoms.TTerm Syntax.Expression
updateRecordExpressionInner x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
        Core.projectionField = (Core.Name "inner")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

updateRecordExpressionWithFields :: Phantoms.TTerm Syntax.UpdateRecordExpression -> Phantoms.TTerm [Syntax.FieldUpdate] -> Phantoms.TTerm Syntax.UpdateRecordExpression
updateRecordExpressionWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
              Core.projectionField = (Core.Name "inner")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

updateRecordExpressionWithInner :: Phantoms.TTerm Syntax.UpdateRecordExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UpdateRecordExpression
updateRecordExpressionWithInner original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.haskell.syntax.UpdateRecordExpression"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valueBindingSimple :: Phantoms.TTerm Syntax.SimpleValueBinding -> Phantoms.TTerm Syntax.ValueBinding
valueBindingSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.haskell.syntax.ValueBinding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

variable :: Phantoms.TTerm Syntax.Name -> Phantoms.TTerm Syntax.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.haskell.syntax.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
