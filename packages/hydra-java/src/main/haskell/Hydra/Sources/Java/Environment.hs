-- | Environment types for Java code generation.
-- These types support the Java coder and are used to track code generation state.

module Hydra.Sources.Java.Environment where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                           ((>:), (@@))
import qualified Hydra.Dsl.Types                           as T
import qualified Hydra.Sources.Kernel.Types.Core           as Core
-- Additional imports
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Packaging as Module
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Java.Syntax as JavaSyntax


ns :: ModuleName
ns = ModuleName "hydra.java.environment"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [JavaSyntax.ns, Core.ns, Graph.ns, Module.ns, Typing.ns],
            moduleMetadata = descriptionMetadata (Just "Environment types for Java code generation")}
  where
    definitions = [
      javaSymbolClass,
      javaFeatures,
      aliases,
      javaEnvironment]

-- | Aliases and context for Java code generation.
-- Tracks namespace mapping, variable scoping, type parameters, and other state
-- needed during the encoding of Hydra terms to Java syntax.
aliases :: TypeDefinition
aliases = def "Aliases" $
  doc "Aliases and context for Java code generation" $
  T.record [
    "currentNamespace">:
      doc "Current module namespace context" $
      modul "ModuleName",
    "packages">:
      doc "Maps namespaces to Java package names" $
      T.map (modul "ModuleName") (syntax "PackageName"),
    "branchVars">:
      doc "Variables bound in pattern matching branches" $
      T.set (core "Name"),
    "recursiveVars">:
      doc "Variables that are self-recursive" $
      T.set (core "Name"),
    "inScopeTypeParams">:
      doc "Type parameters that are in scope (from method-level type parameters)" $
      T.set (core "Name"),
    "polymorphicLocals">:
      doc "Local variables that have polymorphic types (declared with raw types)" $
      T.set (core "Name"),
    "inScopeJavaVars">:
      doc "All in-scope Java variable names (for avoiding lambda parameter shadowing)" $
      T.set (core "Name"),
    "varRenames">:
      doc "Variable renames for avoiding shadowing (maps Hydra name to Java name)" $
      T.map (core "Name") (core "Name"),
    "lambdaVars">:
      doc "Lambda-bound variables (including hoisted captures with qualified names)" $
      T.set (core "Name"),
    "typeVarSubst">:
      doc "Type variable substitution: maps fresh inference variable names to canonical scheme variable names" $
      T.map (core "Name") (core "Name"),
    "trustedTypeVars">:
      doc "Type variables that actually appear in the method's formal parameter types" $
      T.set (core "Name"),
    "methodCodomain">:
      doc "The enclosing method's codomain (return type), used for casting pair expressions" $
      T.optional (core "Type"),
    "thunkedVars">:
      doc "Variables that have been thunked (wrapped in Supplier) for lazy evaluation" $
      T.set (core "Name")]

core :: String -> Type
core = typeref Core.ns

def :: String -> Type -> TypeDefinition
def = datatype ns

environment :: String -> Type
environment = typeref ns

graph :: String -> Type
graph = typeref Graph.ns

-- | Environment for Java code generation.
javaEnvironment :: TypeDefinition
javaEnvironment = def "JavaEnvironment" $
  doc "Environment for Java code generation" $
  T.record [
    "aliases">:
      doc "Aliases and context state" $
      environment "Aliases",
    "graph">:
      doc "Graph context for type inference" $
      graph "Graph"]

-- | Feature flags for the target Java version.
javaFeatures :: TypeDefinition
javaFeatures = def "JavaFeatures" $
  doc "Feature flags for the target Java version" $
  T.record [
    "supportsDiamondOperator">:
      doc "Whether the diamond operator (<>) is supported (Java 7+)" $
      T.boolean]

-- | Classification of a Java symbol for code generation.
javaSymbolClass :: TypeDefinition
javaSymbolClass = def "JavaSymbolClass" $
  doc "Classification of a Java symbol for code generation" $
  T.union [
    "constant">:
      doc "A constant value" $
      T.unit,
    "nullaryFunction">:
      doc "A nullary function (no arguments)" $
      T.unit,
    "hoistedLambda">:
      doc "A hoisted lambda wrapped in type lambdas. The Int is the number of curried lambda parameters." $
      T.int32,
    "unaryFunction">:
      doc "A unary function (single argument)" $
      T.unit,
    "localVariable">:
      doc "A local variable" $
      T.unit]

modul :: String -> Type
modul = typeref Module.ns

syntax :: String -> Type
syntax = typeref JavaSyntax.ns

typing :: String -> Type
typing = typeref Typing.ns
