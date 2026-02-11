-- | Helper types for Java code generation.
-- These types support the Java coder and are used to track code generation state.

module Hydra.Ext.Sources.Java.Helpers where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                           ((>:), (@@))
import qualified Hydra.Dsl.Types                           as T
import qualified Hydra.Sources.Kernel.Types.Core           as Core
-- Additional imports
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Module as Module
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Ext.Sources.Java.Syntax as Syntax


ns :: Namespace
ns = Namespace "hydra.ext.java.helpers"

def :: String -> Type -> Binding
def = datatype ns

helpers :: String -> Type
helpers = typeref ns

syntax :: String -> Type
syntax = typeref Syntax.ns

core :: String -> Type
core = typeref Core.ns

graph :: String -> Type
graph = typeref Graph.ns

modul :: String -> Type
modul = typeref Module.ns

typing :: String -> Type
typing = typeref Typing.ns

module_ :: Module
module_ = Module ns elements [] [Syntax.ns, Core.ns, Graph.ns, Module.ns, Typing.ns] $
    Just "Helper types for Java code generation"
  where
    elements = [
      javaSymbolClass,
      javaFeatures,
      aliases,
      javaEnvironment]

-- | Classification of a Java symbol for code generation.
javaSymbolClass :: Binding
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

-- | Feature flags for the target Java version.
javaFeatures :: Binding
javaFeatures = def "JavaFeatures" $
  doc "Feature flags for the target Java version" $
  T.record [
    "supportsDiamondOperator">:
      doc "Whether the diamond operator (<>) is supported (Java 7+)" $
      T.boolean]

-- | Aliases and context for Java code generation.
-- Tracks namespace mapping, variable scoping, type parameters, and other state
-- needed during the encoding of Hydra terms to Java syntax.
aliases :: Binding
aliases = def "Aliases" $
  doc "Aliases and context for Java code generation" $
  T.record [
    "currentNamespace">:
      doc "Current module namespace context" $
      modul "Namespace",
    "packages">:
      doc "Maps namespaces to Java package names" $
      T.map (modul "Namespace") (syntax "PackageName"),
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

-- | Environment for Java code generation.
javaEnvironment :: Binding
javaEnvironment = def "JavaEnvironment" $
  doc "Environment for Java code generation" $
  T.record [
    "aliases">:
      doc "Aliases and context state" $
      helpers "Aliases",
    "typeContext">:
      doc "Type context for type inference" $
      typing "TypeContext"]
