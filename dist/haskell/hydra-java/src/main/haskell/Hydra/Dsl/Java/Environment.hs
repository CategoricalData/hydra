-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.java.environment

module Hydra.Dsl.Java.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Java.Syntax as JavaSyntax
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Graph as Graph
import qualified Hydra.Java.Environment as Environment
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.java.environment.Aliases
aliases :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm (M.Map Packaging.ModuleName Syntax.PackageName) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (M.Map Core.Name Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (M.Map Core.Name Core.Name) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (Maybe Core.Type) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliases currentNamespace packages branchVars recursiveVars inScopeTypeParams polymorphicLocals inScopeJavaVars varRenames lambdaVars typeVarSubst trustedTypeVars methodCodomain thunkedVars =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Typed.unTypedTerm currentNamespace)},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Typed.unTypedTerm packages)},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Typed.unTypedTerm branchVars)},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Typed.unTypedTerm recursiveVars)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Typed.unTypedTerm inScopeTypeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Typed.unTypedTerm polymorphicLocals)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Typed.unTypedTerm inScopeJavaVars)},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Typed.unTypedTerm varRenames)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Typed.unTypedTerm lambdaVars)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Typed.unTypedTerm typeVarSubst)},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Typed.unTypedTerm trustedTypeVars)},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Typed.unTypedTerm methodCodomain)},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Typed.unTypedTerm thunkedVars)}]}))
-- | DSL accessor for the branchVars field of hydra.java.environment.Aliases
aliasesBranchVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesBranchVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "branchVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the currentNamespace field of hydra.java.environment.Aliases
aliasesCurrentNamespace :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm Packaging.ModuleName
aliasesCurrentNamespace x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "currentNamespace")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inScopeJavaVars field of hydra.java.environment.Aliases
aliasesInScopeJavaVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesInScopeJavaVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inScopeTypeParams field of hydra.java.environment.Aliases
aliasesInScopeTypeParams :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesInScopeTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lambdaVars field of hydra.java.environment.Aliases
aliasesLambdaVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesLambdaVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "lambdaVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the methodCodomain field of hydra.java.environment.Aliases
aliasesMethodCodomain :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (Maybe Core.Type)
aliasesMethodCodomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "methodCodomain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the packages field of hydra.java.environment.Aliases
aliasesPackages :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (M.Map Packaging.ModuleName Syntax.PackageName)
aliasesPackages x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "packages")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the polymorphicLocals field of hydra.java.environment.Aliases
aliasesPolymorphicLocals :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesPolymorphicLocals x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the recursiveVars field of hydra.java.environment.Aliases
aliasesRecursiveVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesRecursiveVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "recursiveVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the thunkedVars field of hydra.java.environment.Aliases
aliasesThunkedVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesThunkedVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "thunkedVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the trustedTypeVars field of hydra.java.environment.Aliases
aliasesTrustedTypeVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name)
aliasesTrustedTypeVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVarSubst field of hydra.java.environment.Aliases
aliasesTypeVarSubst :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (M.Map Core.Name Core.Name)
aliasesTypeVarSubst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "typeVarSubst")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the varRenames field of hydra.java.environment.Aliases
aliasesVarRenames :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (M.Map Core.Name Core.Name)
aliasesVarRenames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "varRenames")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the branchVars field of hydra.java.environment.Aliases
aliasesWithBranchVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithBranchVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the currentNamespace field of hydra.java.environment.Aliases
aliasesWithCurrentNamespace :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Environment.Aliases
aliasesWithCurrentNamespace original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inScopeJavaVars field of hydra.java.environment.Aliases
aliasesWithInScopeJavaVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithInScopeJavaVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inScopeTypeParams field of hydra.java.environment.Aliases
aliasesWithInScopeTypeParams :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithInScopeTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the lambdaVars field of hydra.java.environment.Aliases
aliasesWithLambdaVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithLambdaVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the methodCodomain field of hydra.java.environment.Aliases
aliasesWithMethodCodomain :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (Maybe Core.Type) -> Typed.TypedTerm Environment.Aliases
aliasesWithMethodCodomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the packages field of hydra.java.environment.Aliases
aliasesWithPackages :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (M.Map Packaging.ModuleName Syntax.PackageName) -> Typed.TypedTerm Environment.Aliases
aliasesWithPackages original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the polymorphicLocals field of hydra.java.environment.Aliases
aliasesWithPolymorphicLocals :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithPolymorphicLocals original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the recursiveVars field of hydra.java.environment.Aliases
aliasesWithRecursiveVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithRecursiveVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the thunkedVars field of hydra.java.environment.Aliases
aliasesWithThunkedVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithThunkedVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the trustedTypeVars field of hydra.java.environment.Aliases
aliasesWithTrustedTypeVars :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithTrustedTypeVars original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeVarSubst field of hydra.java.environment.Aliases
aliasesWithTypeVarSubst :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (M.Map Core.Name Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithTypeVarSubst original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "varRenames")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the varRenames field of hydra.java.environment.Aliases
aliasesWithVarRenames :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm (M.Map Core.Name Core.Name) -> Typed.TypedTerm Environment.Aliases
aliasesWithVarRenames original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.Aliases"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "packages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "packages")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "branchVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "branchVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "recursiveVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "recursiveVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeTypeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "polymorphicLocals"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inScopeJavaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "varRenames"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "lambdaVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVarSubst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "typeVarSubst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trustedTypeVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "methodCodomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "methodCodomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "thunkedVars"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
              Core.projectionFieldName = (Core.Name "thunkedVars")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.java.environment.JavaEnvironment
javaEnvironment :: Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Environment.JavaEnvironment
javaEnvironment aliases graph =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Typed.unTypedTerm aliases)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)}]}))
-- | DSL accessor for the aliases field of hydra.java.environment.JavaEnvironment
javaEnvironmentAliases :: Typed.TypedTerm Environment.JavaEnvironment -> Typed.TypedTerm Environment.Aliases
javaEnvironmentAliases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
        Core.projectionFieldName = (Core.Name "aliases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the graph field of hydra.java.environment.JavaEnvironment
javaEnvironmentGraph :: Typed.TypedTerm Environment.JavaEnvironment -> Typed.TypedTerm Graph.Graph
javaEnvironmentGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the aliases field of hydra.java.environment.JavaEnvironment
javaEnvironmentWithAliases :: Typed.TypedTerm Environment.JavaEnvironment -> Typed.TypedTerm Environment.Aliases -> Typed.TypedTerm Environment.JavaEnvironment
javaEnvironmentWithAliases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the graph field of hydra.java.environment.JavaEnvironment
javaEnvironmentWithGraph :: Typed.TypedTerm Environment.JavaEnvironment -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Environment.JavaEnvironment
javaEnvironmentWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aliases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
              Core.projectionFieldName = (Core.Name "aliases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.java.environment.JavaFeatures
javaFeatures :: Typed.TypedTerm Bool -> Typed.TypedTerm Environment.JavaFeatures
javaFeatures supportsDiamondOperator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Typed.unTypedTerm supportsDiamondOperator)}]}))
-- | DSL accessor for the supportsDiamondOperator field of hydra.java.environment.JavaFeatures
javaFeaturesSupportsDiamondOperator :: Typed.TypedTerm Environment.JavaFeatures -> Typed.TypedTerm Bool
javaFeaturesSupportsDiamondOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
        Core.projectionFieldName = (Core.Name "supportsDiamondOperator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the supportsDiamondOperator field of hydra.java.environment.JavaFeatures
javaFeaturesWithSupportsDiamondOperator :: Typed.TypedTerm Environment.JavaFeatures -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.JavaFeatures
javaFeaturesWithSupportsDiamondOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the constant variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassConstant :: Typed.TypedTerm Environment.JavaSymbolClass
javaSymbolClassConstant =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the hoistedLambda variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassHoistedLambda :: Typed.TypedTerm Int -> Typed.TypedTerm Environment.JavaSymbolClass
javaSymbolClassHoistedLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistedLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the localVariable variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassLocalVariable :: Typed.TypedTerm Environment.JavaSymbolClass
javaSymbolClassLocalVariable =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nullaryFunction variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassNullaryFunction :: Typed.TypedTerm Environment.JavaSymbolClass
javaSymbolClassNullaryFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unaryFunction variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassUnaryFunction :: Typed.TypedTerm Environment.JavaSymbolClass
javaSymbolClassUnaryFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
