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
-- | DSL constructor for hydra.java.environment.Aliases
aliases :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6 -> Typed.TypedTerm t7 -> Typed.TypedTerm t8 -> Typed.TypedTerm t9 -> Typed.TypedTerm t10 -> Typed.TypedTerm t11 -> Typed.TypedTerm t12 -> Typed.TypedTerm t13
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
aliasesBranchVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesBranchVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "branchVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the currentNamespace field of hydra.java.environment.Aliases
aliasesCurrentNamespace :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesCurrentNamespace x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "currentNamespace")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inScopeJavaVars field of hydra.java.environment.Aliases
aliasesInScopeJavaVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesInScopeJavaVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "inScopeJavaVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inScopeTypeParams field of hydra.java.environment.Aliases
aliasesInScopeTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesInScopeTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "inScopeTypeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lambdaVars field of hydra.java.environment.Aliases
aliasesLambdaVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesLambdaVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "lambdaVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the methodCodomain field of hydra.java.environment.Aliases
aliasesMethodCodomain :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesMethodCodomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "methodCodomain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the packages field of hydra.java.environment.Aliases
aliasesPackages :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesPackages x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "packages")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the polymorphicLocals field of hydra.java.environment.Aliases
aliasesPolymorphicLocals :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesPolymorphicLocals x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "polymorphicLocals")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the recursiveVars field of hydra.java.environment.Aliases
aliasesRecursiveVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesRecursiveVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "recursiveVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the thunkedVars field of hydra.java.environment.Aliases
aliasesThunkedVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesThunkedVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "thunkedVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the trustedTypeVars field of hydra.java.environment.Aliases
aliasesTrustedTypeVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesTrustedTypeVars x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "trustedTypeVars")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVarSubst field of hydra.java.environment.Aliases
aliasesTypeVarSubst :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesTypeVarSubst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "typeVarSubst")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the varRenames field of hydra.java.environment.Aliases
aliasesVarRenames :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
aliasesVarRenames x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.Aliases"),
        Core.projectionFieldName = (Core.Name "varRenames")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the branchVars field of hydra.java.environment.Aliases
aliasesWithBranchVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithCurrentNamespace :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithInScopeJavaVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithInScopeTypeParams :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithLambdaVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithMethodCodomain :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithPackages :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithPolymorphicLocals :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithRecursiveVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithThunkedVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithTrustedTypeVars :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithTypeVarSubst :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
aliasesWithVarRenames :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
javaEnvironment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
javaEnvironmentAliases :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
javaEnvironmentAliases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
        Core.projectionFieldName = (Core.Name "aliases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the graph field of hydra.java.environment.JavaEnvironment
javaEnvironmentGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
javaEnvironmentGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaEnvironment"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the aliases field of hydra.java.environment.JavaEnvironment
javaEnvironmentWithAliases :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
javaEnvironmentWithGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
javaFeatures :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
javaFeatures supportsDiamondOperator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Typed.unTypedTerm supportsDiamondOperator)}]}))
-- | DSL accessor for the supportsDiamondOperator field of hydra.java.environment.JavaFeatures
javaFeaturesSupportsDiamondOperator :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
javaFeaturesSupportsDiamondOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
        Core.projectionFieldName = (Core.Name "supportsDiamondOperator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the supportsDiamondOperator field of hydra.java.environment.JavaFeatures
javaFeaturesWithSupportsDiamondOperator :: t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
javaFeaturesWithSupportsDiamondOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.java.environment.JavaFeatures"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "supportsDiamondOperator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the constant variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassConstant :: Typed.TypedTerm t0
javaSymbolClassConstant =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constant"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the hoistedLambda variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassHoistedLambda :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
javaSymbolClassHoistedLambda x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistedLambda"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the localVariable variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassLocalVariable :: Typed.TypedTerm t0
javaSymbolClassLocalVariable =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "localVariable"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nullaryFunction variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassNullaryFunction :: Typed.TypedTerm t0
javaSymbolClassNullaryFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unaryFunction variant of hydra.java.environment.JavaSymbolClass
javaSymbolClassUnaryFunction :: Typed.TypedTerm t0
javaSymbolClassUnaryFunction =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.java.environment.JavaSymbolClass"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unaryFunction"),
        Core.fieldTerm = Core.TermUnit}}))
