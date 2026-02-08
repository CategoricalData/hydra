-- Note: this is an automatically generated file. Do not edit.

-- | Helper types for Python code generation

module Hydra.Ext.Python.Helpers where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Target Python version for code generation
data PythonVersion = 
  PythonVersionPython310  |
  PythonVersionPython312 
  deriving (Eq, Ord, Read, Show)

_PythonVersion = (Core.Name "hydra.ext.python.helpers.PythonVersion")

_PythonVersion_python310 = (Core.Name "python310")

_PythonVersion_python312 = (Core.Name "python312")

-- | Environment for Python code generation
data PythonEnvironment = 
  PythonEnvironment {
    -- | Namespace mapping for imports
    pythonEnvironmentNamespaces :: (Module.Namespaces Syntax.DottedName),
    -- | Type variables in scope, with their Python names
    pythonEnvironmentBoundTypeVariables :: ([Core.Name], (M.Map Core.Name Syntax.Name)),
    -- | Type context for type inference
    pythonEnvironmentTypeContext :: Typing.TypeContext,
    -- | Set of nullary bindings (need call syntax)
    pythonEnvironmentNullaryBindings :: (S.Set Core.Name),
    -- | Target Python version
    pythonEnvironmentVersion :: PythonVersion,
    -- | When True, skip generating cast() calls for reduced memory usage
    pythonEnvironmentSkipCasts :: Bool,
    -- | Variables that are inline let bindings (walrus operators)
    pythonEnvironmentInlineVariables :: (S.Set Core.Name)}
  deriving (Eq, Ord, Read, Show)

_PythonEnvironment = (Core.Name "hydra.ext.python.helpers.PythonEnvironment")

_PythonEnvironment_namespaces = (Core.Name "namespaces")

_PythonEnvironment_boundTypeVariables = (Core.Name "boundTypeVariables")

_PythonEnvironment_typeContext = (Core.Name "typeContext")

_PythonEnvironment_nullaryBindings = (Core.Name "nullaryBindings")

_PythonEnvironment_version = (Core.Name "version")

_PythonEnvironment_skipCasts = (Core.Name "skipCasts")

_PythonEnvironment_inlineVariables = (Core.Name "inlineVariables")

-- | Temporary metadata used to create the header section of a Python file
data PythonModuleMetadata = 
  PythonModuleMetadata {
    -- | Namespace mapping for imports
    pythonModuleMetadataNamespaces :: (Module.Namespaces Syntax.DottedName),
    -- | Type variables used in the module
    pythonModuleMetadataTypeVariables :: (S.Set Core.Name),
    pythonModuleMetadataUsesAnnotated :: Bool,
    pythonModuleMetadataUsesCallable :: Bool,
    pythonModuleMetadataUsesCast :: Bool,
    pythonModuleMetadataUsesLruCache :: Bool,
    pythonModuleMetadataUsesTypeAlias :: Bool,
    pythonModuleMetadataUsesDataclass :: Bool,
    pythonModuleMetadataUsesDecimal :: Bool,
    pythonModuleMetadataUsesEither :: Bool,
    pythonModuleMetadataUsesEnum :: Bool,
    pythonModuleMetadataUsesFrozenDict :: Bool,
    pythonModuleMetadataUsesFrozenList :: Bool,
    pythonModuleMetadataUsesGeneric :: Bool,
    pythonModuleMetadataUsesJust :: Bool,
    pythonModuleMetadataUsesLeft :: Bool,
    pythonModuleMetadataUsesMaybe :: Bool,
    pythonModuleMetadataUsesName :: Bool,
    pythonModuleMetadataUsesNode :: Bool,
    pythonModuleMetadataUsesNothing :: Bool,
    pythonModuleMetadataUsesRight :: Bool,
    pythonModuleMetadataUsesTypeVar :: Bool}
  deriving (Eq, Ord, Read, Show)

_PythonModuleMetadata = (Core.Name "hydra.ext.python.helpers.PythonModuleMetadata")

_PythonModuleMetadata_namespaces = (Core.Name "namespaces")

_PythonModuleMetadata_typeVariables = (Core.Name "typeVariables")

_PythonModuleMetadata_usesAnnotated = (Core.Name "usesAnnotated")

_PythonModuleMetadata_usesCallable = (Core.Name "usesCallable")

_PythonModuleMetadata_usesCast = (Core.Name "usesCast")

_PythonModuleMetadata_usesLruCache = (Core.Name "usesLruCache")

_PythonModuleMetadata_usesTypeAlias = (Core.Name "usesTypeAlias")

_PythonModuleMetadata_usesDataclass = (Core.Name "usesDataclass")

_PythonModuleMetadata_usesDecimal = (Core.Name "usesDecimal")

_PythonModuleMetadata_usesEither = (Core.Name "usesEither")

_PythonModuleMetadata_usesEnum = (Core.Name "usesEnum")

_PythonModuleMetadata_usesFrozenDict = (Core.Name "usesFrozenDict")

_PythonModuleMetadata_usesFrozenList = (Core.Name "usesFrozenList")

_PythonModuleMetadata_usesGeneric = (Core.Name "usesGeneric")

_PythonModuleMetadata_usesJust = (Core.Name "usesJust")

_PythonModuleMetadata_usesLeft = (Core.Name "usesLeft")

_PythonModuleMetadata_usesMaybe = (Core.Name "usesMaybe")

_PythonModuleMetadata_usesName = (Core.Name "usesName")

_PythonModuleMetadata_usesNode = (Core.Name "usesNode")

_PythonModuleMetadata_usesNothing = (Core.Name "usesNothing")

_PythonModuleMetadata_usesRight = (Core.Name "usesRight")

_PythonModuleMetadata_usesTypeVar = (Core.Name "usesTypeVar")

-- | Combined graph and metadata state for Python code generation
data PyGraph = 
  PyGraph {
    -- | The Hydra graph being processed
    pyGraphGraph :: Graph.Graph,
    -- | Accumulated module metadata
    pyGraphMetadata :: PythonModuleMetadata}

_PyGraph = (Core.Name "hydra.ext.python.helpers.PyGraph")

_PyGraph_graph = (Core.Name "graph")

_PyGraph_metadata = (Core.Name "metadata")
