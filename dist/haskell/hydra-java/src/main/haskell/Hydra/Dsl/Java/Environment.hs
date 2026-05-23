-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.java.environment

module Hydra.Dsl.Java.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Java.Environment as Environment
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.java.environment.Aliases
aliases :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm (M.Map Packaging.ModuleName Syntax.PackageName) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliases currentNamespace packages branchVars recursiveVars inScopeTypeParams polymorphicLocals inScopeJavaVars varRenames lambdaVars typeVarSubst trustedTypeVars methodCodomain thunkedVars =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
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
-- | DSL accessor for the branchVars field of hydra.java.environment.Aliases
aliasesBranchVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesBranchVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "branchVars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the currentNamespace field of hydra.java.environment.Aliases
aliasesCurrentNamespace :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Packaging.ModuleName
aliasesCurrentNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "currentNamespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inScopeJavaVars field of hydra.java.environment.Aliases
aliasesInScopeJavaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesInScopeJavaVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the inScopeTypeParams field of hydra.java.environment.Aliases
aliasesInScopeTypeParams :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesInScopeTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lambdaVars field of hydra.java.environment.Aliases
aliasesLambdaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesLambdaVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "lambdaVars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the methodCodomain field of hydra.java.environment.Aliases
aliasesMethodCodomain :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (Maybe Core.Type)
aliasesMethodCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "methodCodomain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the packages field of hydra.java.environment.Aliases
aliasesPackages :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Packaging.ModuleName Syntax.PackageName)
aliasesPackages x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "packages")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the polymorphicLocals field of hydra.java.environment.Aliases
aliasesPolymorphicLocals :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesPolymorphicLocals x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the recursiveVars field of hydra.java.environment.Aliases
aliasesRecursiveVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesRecursiveVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "recursiveVars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the thunkedVars field of hydra.java.environment.Aliases
aliasesThunkedVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesThunkedVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "thunkedVars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the trustedTypeVars field of hydra.java.environment.Aliases
aliasesTrustedTypeVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name)
aliasesTrustedTypeVars x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeVarSubst field of hydra.java.environment.Aliases
aliasesTypeVarSubst :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name)
aliasesTypeVarSubst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "typeVarSubst")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the varRenames field of hydra.java.environment.Aliases
aliasesVarRenames :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name)
aliasesVarRenames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "varRenames")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the branchVars field of hydra.java.environment.Aliases
aliasesWithBranchVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithBranchVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the currentNamespace field of hydra.java.environment.Aliases
aliasesWithCurrentNamespace :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Environment.Aliases
aliasesWithCurrentNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the inScopeJavaVars field of hydra.java.environment.Aliases
aliasesWithInScopeJavaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithInScopeJavaVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the inScopeTypeParams field of hydra.java.environment.Aliases
aliasesWithInScopeTypeParams :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithInScopeTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the lambdaVars field of hydra.java.environment.Aliases
aliasesWithLambdaVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithLambdaVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the methodCodomain field of hydra.java.environment.Aliases
aliasesWithMethodCodomain :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm Environment.Aliases
aliasesWithMethodCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the packages field of hydra.java.environment.Aliases
aliasesWithPackages :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Packaging.ModuleName Syntax.PackageName) -> Phantoms.TTerm Environment.Aliases
aliasesWithPackages original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the polymorphicLocals field of hydra.java.environment.Aliases
aliasesWithPolymorphicLocals :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithPolymorphicLocals original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the recursiveVars field of hydra.java.environment.Aliases
aliasesWithRecursiveVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithRecursiveVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the thunkedVars field of hydra.java.environment.Aliases
aliasesWithThunkedVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithThunkedVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the trustedTypeVars field of hydra.java.environment.Aliases
aliasesWithTrustedTypeVars :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithTrustedTypeVars original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeVarSubst field of hydra.java.environment.Aliases
aliasesWithTypeVarSubst :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithTypeVarSubst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the varRenames field of hydra.java.environment.Aliases
aliasesWithVarRenames :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm (M.Map Core.Name Core.Name) -> Phantoms.TTerm Environment.Aliases
aliasesWithVarRenames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.java.environment.JavaEnvironment
javaEnvironment :: Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.JavaEnvironment
javaEnvironment aliases graph =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm aliases)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)}]}))
-- | DSL accessor for the aliases field of hydra.java.environment.JavaEnvironment
javaEnvironmentAliases :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Environment.Aliases
javaEnvironmentAliases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
        Core.projectionFieldName = (Core.Name "aliases")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the graph field of hydra.java.environment.JavaEnvironment
javaEnvironmentGraph :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Graph.Graph
javaEnvironmentGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the aliases field of hydra.java.environment.JavaEnvironment
javaEnvironmentWithAliases :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Environment.Aliases -> Phantoms.TTerm Environment.JavaEnvironment
javaEnvironmentWithAliases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the graph field of hydra.java.environment.JavaEnvironment
javaEnvironmentWithGraph :: Phantoms.TTerm Environment.JavaEnvironment -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.JavaEnvironment
javaEnvironmentWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
              Core.projectionFieldName = (Core.Name "aliases")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.java.environment.JavaFeatures
javaFeatures :: Phantoms.TTerm Bool -> Phantoms.TTerm Environment.JavaFeatures
javaFeatures supportsDiamondOperator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Phantoms.unTTerm supportsDiamondOperator)}]}))
-- | DSL accessor for the supportsDiamondOperator field of hydra.java.environment.JavaFeatures
javaFeaturesSupportsDiamondOperator :: Phantoms.TTerm Environment.JavaFeatures -> Phantoms.TTerm Bool
javaFeaturesSupportsDiamondOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
        Core.projectionFieldName = (Core.Name "supportsDiamondOperator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the supportsDiamondOperator field of hydra.java.environment.JavaFeatures
javaFeaturesWithSupportsDiamondOperator :: Phantoms.TTerm Environment.JavaFeatures -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.JavaFeatures
javaFeaturesWithSupportsDiamondOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the constant variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassConstant :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassConstant =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the hoistedLambda variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassHoistedLambda :: Phantoms.TTerm Int -> Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassHoistedLambda x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistedLambda"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the localVariable variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassLocalVariable :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassLocalVariable =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nullaryFunction variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassNullaryFunction :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassNullaryFunction =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unaryFunction variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassUnaryFunction :: Phantoms.TTerm Environment.JavaSymbolClass
javaSymbolClassUnaryFunction =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
