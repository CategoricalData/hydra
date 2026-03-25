-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.java.environment

module Hydra.Dsl.Ext.Java.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Java.Environment as Environment
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

aliases :: Phantoms.TTerm Module.Namespace -> Phantoms.TTerm (M.Map Module.Namespace Syntax.PackageName) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliases currentNamespace packages branchVars recursiveVars inScopeTypeParams polymorphicLocals inScopeJavaVars varRenames lambdaVars typeVarSubst trustedTypeVars methodCodomain thunkedVars =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm currentNamespace)},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Phantoms.unTTerm packages)},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Phantoms.unTTerm branchVars)},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Phantoms.unTTerm recursiveVars)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Phantoms.unTTerm inScopeTypeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Phantoms.unTTerm polymorphicLocals)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Phantoms.unTTerm inScopeJavaVars)},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Phantoms.unTTerm varRenames)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Phantoms.unTTerm lambdaVars)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Phantoms.unTTerm typeVarSubst)},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Phantoms.unTTerm trustedTypeVars)},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Phantoms.unTTerm methodCodomain)},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Phantoms.unTTerm thunkedVars)}]}))

aliasesBranchVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesBranchVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "branchVars")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesCurrentNamespace :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Module.Namespace
aliasesCurrentNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "currentNamespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesInScopeJavaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesInScopeJavaVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "inScopeJavaVars")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesInScopeTypeParams :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesInScopeTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "inScopeTypeParams")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesLambdaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesLambdaVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "lambdaVars")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesMethodCodomain :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (Maybe Core.Type)
aliasesMethodCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "methodCodomain")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesPackages :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Module.Namespace Syntax.PackageName)
aliasesPackages x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "packages")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesPolymorphicLocals :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesPolymorphicLocals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "polymorphicLocals")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesRecursiveVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesRecursiveVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "recursiveVars")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesThunkedVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesThunkedVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "thunkedVars")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesTrustedTypeVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesTrustedTypeVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "trustedTypeVars")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesTypeVarSubst :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name)
aliasesTypeVarSubst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "typeVarSubst")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesVarRenames :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name)
aliasesVarRenames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
        Core.projectionField = (Core.Name "varRenames")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aliasesWithBranchVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithBranchVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithCurrentNamespace :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Module.Namespace -> Phantoms.TTerm Environment.Aliases
aliasesWithCurrentNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithInScopeJavaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithInScopeJavaVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithInScopeTypeParams :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithInScopeTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithLambdaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithLambdaVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithMethodCodomain :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm Environment.Aliases
aliasesWithMethodCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithPackages :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Module.Namespace Syntax.PackageName) -> Phantoms.TTerm Environment.Aliases
aliasesWithPackages original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithPolymorphicLocals :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithPolymorphicLocals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithRecursiveVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithRecursiveVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithThunkedVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithThunkedVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

aliasesWithTrustedTypeVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithTrustedTypeVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithTypeVarSubst :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithTypeVarSubst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "varRenames")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aliasesWithVarRenames :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithVarRenames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "currentNamespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "packages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "branchVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "recursiveVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeTypeParams")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "polymorphicLocals")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "inScopeJavaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "lambdaVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "typeVarSubst")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "trustedTypeVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "methodCodomain")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.Aliases"),
              Core.projectionField = (Core.Name "thunkedVars")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

javaEnvironment :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.JavaEnvironment
javaEnvironment aliases graph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm aliases)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)}]}))

javaEnvironmentAliases :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Environment.Aliases
javaEnvironmentAliases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
        Core.projectionField = (Core.Name "aliases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

javaEnvironmentGraph :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Graph.Graph
javaEnvironmentGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
        Core.projectionField = (Core.Name "graph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

javaEnvironmentWithAliases :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Environment.JavaEnvironment
javaEnvironmentWithAliases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

javaEnvironmentWithGraph :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.JavaEnvironment
javaEnvironmentWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.JavaEnvironment"),
              Core.projectionField = (Core.Name "aliases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

javaFeatures :: Phantoms.TTerm Bool -> Phantoms.TTerm Environment.JavaFeatures
javaFeatures supportsDiamondOperator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Phantoms.unTTerm supportsDiamondOperator)}]}))

javaFeaturesSupportsDiamondOperator :: Phantoms.TTerm Environment.JavaFeatures -> Phantoms.TTerm Bool
javaFeaturesSupportsDiamondOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.java.environment.JavaFeatures"),
        Core.projectionField = (Core.Name "supportsDiamondOperator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

javaFeaturesWithSupportsDiamondOperator :: Phantoms.TTerm Environment.JavaFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.JavaFeatures
javaFeaturesWithSupportsDiamondOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

javaSymbolClassConstant :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassConstant =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = Core.TermUnit}}))

javaSymbolClassHoistedLambda :: Phantoms.TTerm Int -> Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassHoistedLambda x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistedLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

javaSymbolClassLocalVariable :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassLocalVariable =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = Core.TermUnit}}))

javaSymbolClassNullaryFunction :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassNullaryFunction =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))

javaSymbolClassUnaryFunction :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassUnaryFunction =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
