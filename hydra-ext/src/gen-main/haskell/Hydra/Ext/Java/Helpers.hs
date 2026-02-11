-- Note: this is an automatically generated file. Do not edit.

-- | Helper types for Java code generation

module Hydra.Ext.Java.Helpers where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Module as Module
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Classification of a Java symbol for code generation
data JavaSymbolClass = 
  -- | A constant value
  JavaSymbolClassConstant  |
  -- | A nullary function (no arguments)
  JavaSymbolClassNullaryFunction  |
  -- | A hoisted lambda wrapped in type lambdas. The Int is the number of curried lambda parameters.
  JavaSymbolClassHoistedLambda Int |
  -- | A unary function (single argument)
  JavaSymbolClassUnaryFunction  |
  -- | A local variable
  JavaSymbolClassLocalVariable 
  deriving (Eq, Ord, Read, Show)

_JavaSymbolClass = (Core.Name "hydra.ext.java.helpers.JavaSymbolClass")

_JavaSymbolClass_constant = (Core.Name "constant")

_JavaSymbolClass_nullaryFunction = (Core.Name "nullaryFunction")

_JavaSymbolClass_hoistedLambda = (Core.Name "hoistedLambda")

_JavaSymbolClass_unaryFunction = (Core.Name "unaryFunction")

_JavaSymbolClass_localVariable = (Core.Name "localVariable")

-- | Feature flags for the target Java version
data JavaFeatures = 
  JavaFeatures {
    -- | Whether the diamond operator (<>) is supported (Java 7+)
    javaFeaturesSupportsDiamondOperator :: Bool}
  deriving (Eq, Ord, Read, Show)

_JavaFeatures = (Core.Name "hydra.ext.java.helpers.JavaFeatures")

_JavaFeatures_supportsDiamondOperator = (Core.Name "supportsDiamondOperator")

-- | Aliases and context for Java code generation
data Aliases = 
  Aliases {
    -- | Current module namespace context
    aliasesCurrentNamespace :: Module.Namespace,
    -- | Maps namespaces to Java package names
    aliasesPackages :: (M.Map Module.Namespace Syntax.PackageName),
    -- | Variables bound in pattern matching branches
    aliasesBranchVars :: (S.Set Core.Name),
    -- | Variables that are self-recursive
    aliasesRecursiveVars :: (S.Set Core.Name),
    -- | Type parameters that are in scope (from method-level type parameters)
    aliasesInScopeTypeParams :: (S.Set Core.Name),
    -- | Local variables that have polymorphic types (declared with raw types)
    aliasesPolymorphicLocals :: (S.Set Core.Name),
    -- | All in-scope Java variable names (for avoiding lambda parameter shadowing)
    aliasesInScopeJavaVars :: (S.Set Core.Name),
    -- | Variable renames for avoiding shadowing (maps Hydra name to Java name)
    aliasesVarRenames :: (M.Map Core.Name Core.Name),
    -- | Lambda-bound variables (including hoisted captures with qualified names)
    aliasesLambdaVars :: (S.Set Core.Name),
    -- | Type variable substitution: maps fresh inference variable names to canonical scheme variable names
    aliasesTypeVarSubst :: (M.Map Core.Name Core.Name),
    -- | Type variables that actually appear in the method's formal parameter types
    aliasesTrustedTypeVars :: (S.Set Core.Name),
    -- | The enclosing method's codomain (return type), used for casting pair expressions
    aliasesMethodCodomain :: (Maybe Core.Type),
    -- | Variables that have been thunked (wrapped in Supplier) for lazy evaluation
    aliasesThunkedVars :: (S.Set Core.Name)}
  deriving (Eq, Ord, Read, Show)

_Aliases = (Core.Name "hydra.ext.java.helpers.Aliases")

_Aliases_currentNamespace = (Core.Name "currentNamespace")

_Aliases_packages = (Core.Name "packages")

_Aliases_branchVars = (Core.Name "branchVars")

_Aliases_recursiveVars = (Core.Name "recursiveVars")

_Aliases_inScopeTypeParams = (Core.Name "inScopeTypeParams")

_Aliases_polymorphicLocals = (Core.Name "polymorphicLocals")

_Aliases_inScopeJavaVars = (Core.Name "inScopeJavaVars")

_Aliases_varRenames = (Core.Name "varRenames")

_Aliases_lambdaVars = (Core.Name "lambdaVars")

_Aliases_typeVarSubst = (Core.Name "typeVarSubst")

_Aliases_trustedTypeVars = (Core.Name "trustedTypeVars")

_Aliases_methodCodomain = (Core.Name "methodCodomain")

_Aliases_thunkedVars = (Core.Name "thunkedVars")

-- | Environment for Java code generation
data JavaEnvironment = 
  JavaEnvironment {
    -- | Aliases and context state
    javaEnvironmentAliases :: Aliases,
    -- | Type context for type inference
    javaEnvironmentTypeContext :: Typing.TypeContext}
  deriving (Eq, Ord, Read, Show)

_JavaEnvironment = (Core.Name "hydra.ext.java.helpers.JavaEnvironment")

_JavaEnvironment_aliases = (Core.Name "aliases")

_JavaEnvironment_typeContext = (Core.Name "typeContext")
