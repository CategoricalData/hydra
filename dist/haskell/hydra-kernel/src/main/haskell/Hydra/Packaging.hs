-- Note: this is an automatically generated file. Do not edit.

-- | A model for Hydra namespaces, modules, and packages

module Hydra.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | A definition, which may be either a term or type definition
data Definition =
  -- | A term definition
  DefinitionTerm TermDefinition |
  -- | A type definition
  DefinitionType TypeDefinition
  deriving (Eq, Ord, Read, Show)

_Definition = Core.Name "hydra.packaging.Definition"

_Definition_term = Core.Name "term"

_Definition_type = Core.Name "type"

-- | A file extension (without the dot), e.g. "json" or "py"
newtype FileExtension =
  FileExtension {
    unFileExtension :: String}
  deriving (Eq, Ord, Read, Show)

_FileExtension = Core.Name "hydra.packaging.FileExtension"

-- | A library of primitive functions
data Library =
  Library {
    -- | A common prefix for all primitive function names in the library
    libraryNamespace :: Namespace,
    -- | A preferred namespace prefix for function names in the library
    libraryPrefix :: String,
    -- | The primitives defined in this library
    libraryPrimitives :: [Graph.Primitive]}

_Library = Core.Name "hydra.packaging.Library"

_Library_namespace = Core.Name "namespace"

_Library_prefix = Core.Name "prefix"

_Library_primitives = Core.Name "primitives"

-- | A logical collection of elements in the same namespace, having dependencies on zero or more other modules
data Module =
  Module {
    -- | A common prefix for all element names in the module
    moduleNamespace :: Namespace,
    -- | The definitions in this module
    moduleDefinitions :: [Definition],
    -- | Any modules which the term expressions of this module directly depend upon
    moduleTermDependencies :: [Namespace],
    -- | Any modules which the type expressions of this module directly depend upon
    moduleTypeDependencies :: [Namespace],
    -- | An optional human-readable description of the module
    moduleDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Module = Core.Name "hydra.packaging.Module"

_Module_namespace = Core.Name "namespace"

_Module_definitions = Core.Name "definitions"

_Module_termDependencies = Core.Name "termDependencies"

_Module_typeDependencies = Core.Name "typeDependencies"

_Module_description = Core.Name "description"

-- | A prefix for element names
newtype Namespace =
  Namespace {
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = Core.Name "hydra.packaging.Namespace"

-- | A mapping from namespaces to values of type n, with a focus on one namespace
data Namespaces n =
  Namespaces {
    -- | The namespace in focus, together with its associated value
    namespacesFocus :: (Namespace, n),
    -- | A mapping of namespaces to values
    namespacesMapping :: (M.Map Namespace n)}
  deriving (Eq, Ord, Read, Show)

_Namespaces = Core.Name "hydra.packaging.Namespaces"

_Namespaces_focus = Core.Name "focus"

_Namespaces_mapping = Core.Name "mapping"

-- | A package, which is a named collection of modules with metadata and dependencies
data Package =
  Package {
    -- | The name of the package
    packageName :: PackageName,
    -- | The modules in this package
    packageModules :: [Module],
    -- | The packages which this package depends on
    packageDependencies :: [PackageName],
    -- | An optional human-readable description of the package
    packageDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Package = Core.Name "hydra.packaging.Package"

_Package_name = Core.Name "name"

_Package_modules = Core.Name "modules"

_Package_dependencies = Core.Name "dependencies"

_Package_description = Core.Name "description"

-- | The unique name of a package, e.g. "hydra-kernel" or "hydra-python"
newtype PackageName =
  PackageName {
    unPackageName :: String}
  deriving (Eq, Ord, Read, Show)

_PackageName = Core.Name "hydra.packaging.PackageName"

-- | A qualified name consisting of an optional namespace together with a mandatory local name
data QualifiedName =
  QualifiedName {
    -- | The optional namespace
    qualifiedNameNamespace :: (Maybe Namespace),
    -- | The local name
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = Core.Name "hydra.packaging.QualifiedName"

_QualifiedName_namespace = Core.Name "namespace"

_QualifiedName_local = Core.Name "local"

-- | A term-level definition, including a name, a term, and the type scheme of the term
data TermDefinition =
  TermDefinition {
    -- | The name of the term
    termDefinitionName :: Core.Name,
    -- | The term being defined
    termDefinitionTerm :: Core.Term,
    -- | The type scheme of the term, including any class constraints
    termDefinitionType :: (Maybe Core.TypeScheme)}
  deriving (Eq, Ord, Read, Show)

_TermDefinition = Core.Name "hydra.packaging.TermDefinition"

_TermDefinition_name = Core.Name "name"

_TermDefinition_term = Core.Name "term"

_TermDefinition_type = Core.Name "type"

-- | A type-level definition, including a name and the type scheme
data TypeDefinition =
  TypeDefinition {
    -- | The name of the type
    typeDefinitionName :: Core.Name,
    -- | The type scheme being defined
    typeDefinitionType :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)

_TypeDefinition = Core.Name "hydra.packaging.TypeDefinition"

_TypeDefinition_name = Core.Name "name"

_TypeDefinition_type = Core.Name "type"
