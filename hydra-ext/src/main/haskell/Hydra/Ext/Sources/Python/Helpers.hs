-- | Helper types for Python code generation.
-- These types support the Python coder and are used to track code generation state.

module Hydra.Ext.Sources.Python.Helpers where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                           ((>:), (@@))
import qualified Hydra.Dsl.Types                           as T
import qualified Hydra.Sources.Kernel.Types.Core           as Core
-- Additional imports
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Module as Module
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Ext.Sources.Python.Syntax as Syntax


ns :: Namespace
ns = Namespace "hydra.ext.python.helpers"

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
module_ = Module ns elements [] [Syntax.ns, Compute.ns, Core.ns, Graph.ns, Module.ns, Typing.ns] $
    Just "Helper types for Python code generation"
  where
    elements = [
      pythonVersion,
      pythonEnvironment,
      pythonModuleMetadata,
      pyGraph]

-- | Target Python version for code generation.
pythonVersion :: Binding
pythonVersion = def "PythonVersion" $
  doc "Target Python version for code generation" $
  T.enum [
    "python310",  -- Python 3.10+ compatible (e.g., for PyPy)
    "python312"]  -- Python 3.12+ with PEP 695 type alias syntax

-- | Environment for Python code generation.
pythonEnvironment :: Binding
pythonEnvironment = def "PythonEnvironment" $
  doc "Environment for Python code generation" $
  T.record [
    "namespaces">:
      doc "Namespace mapping for imports" $
      modul "Namespaces" @@ syntax "DottedName",
    "boundTypeVariables">:
      doc "Type variables in scope, with their Python names" $
      T.pair (T.list (core "Name")) (T.map (core "Name") (syntax "Name")),
    "typeContext">:
      doc "Type context for type inference" $
      typing "TypeContext",
    "nullaryBindings">:
      doc "Set of nullary bindings (need call syntax)" $
      T.set (core "Name"),
    "version">:
      doc "Target Python version" $
      helpers "PythonVersion",
    "skipCasts">:
      doc "When True, skip generating cast() calls for reduced memory usage" $
      T.boolean,
    "inlineVariables">:
      doc "Variables that are inline let bindings (walrus operators)" $
      T.set (core "Name")]

-- | Metadata for Python module generation.
pythonModuleMetadata :: Binding
pythonModuleMetadata = def "PythonModuleMetadata" $
  doc "Temporary metadata used to create the header section of a Python file" $
  T.record [
    "namespaces">:
      doc "Namespace mapping for imports" $
      modul "Namespaces" @@ syntax "DottedName",
    "typeVariables">:
      doc "Type variables used in the module" $
      T.set (core "Name"),
    "usesAnnotated">: T.boolean,
    "usesCallable">: T.boolean,
    "usesCast">: T.boolean,
    "usesLruCache">: T.boolean,
    "usesTypeAlias">: T.boolean,
    "usesDataclass">: T.boolean,
    "usesDecimal">: T.boolean,
    "usesEither">: T.boolean,
    "usesEnum">: T.boolean,
    "usesFrozenDict">: T.boolean,
    "usesFrozenList">: T.boolean,
    "usesGeneric">: T.boolean,
    "usesJust">: T.boolean,
    "usesLeft">: T.boolean,
    "usesMaybe">: T.boolean,
    "usesName">: T.boolean,
    "usesNode">: T.boolean,
    "usesNothing">: T.boolean,
    "usesRight">: T.boolean,
    "usesTypeVar">: T.boolean]

-- | Combined graph and metadata state for Python code generation.
pyGraph :: Binding
pyGraph = def "PyGraph" $
  doc "Combined graph and metadata state for Python code generation" $
  T.record [
    "graph">:
      doc "The Hydra graph being processed" $
      graph "Graph",
    "metadata">:
      doc "Accumulated module metadata" $
      helpers "PythonModuleMetadata"]
