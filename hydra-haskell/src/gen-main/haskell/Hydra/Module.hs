-- | A model for Hydra namespaces and modules

module Hydra.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A definition, which may be either a term or type definition
data Definition = 
  DefinitionTerm TermDefinition |
  DefinitionType TypeDefinition
  deriving (Eq, Ord, Read, Show)

_Definition = (Core.Name "hydra.module.Definition")

_Definition_term = (Core.Name "term")

_Definition_type = (Core.Name "type")

-- | A file extension (without the dot), e.g. "json" or "py"
newtype FileExtension = 
  FileExtension {
    unFileExtension :: String}
  deriving (Eq, Ord, Read, Show)

_FileExtension = (Core.Name "hydra.module.FileExtension")

-- | A library of primitive functions
data Library = 
  Library {
    -- | A common prefix for all primitive function names in the library
    libraryNamespace :: Namespace,
    -- | A preferred namespace prefix for function names in the library
    libraryPrefix :: String,
    -- | The primitives defined in this library
    libraryPrimitives :: [Graph.Primitive]}

_Library = (Core.Name "hydra.module.Library")

_Library_namespace = (Core.Name "namespace")

_Library_prefix = (Core.Name "prefix")

_Library_primitives = (Core.Name "primitives")

-- | A logical collection of elements in the same namespace, having dependencies on zero or more other modules
data Module = 
  Module {
    -- | A common prefix for all element names in the module
    moduleNamespace :: Namespace,
    -- | The elements defined in this module
    moduleElements :: [Graph.Element],
    -- | Any modules which the term expressions of this module directly depend upon
    moduleTermDependencies :: [Module],
    -- | Any modules which the type expressions of this module directly depend upon
    moduleTypeDependencies :: [Module],
    -- | An optional human-readable description of the module
    moduleDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra.module.Module")

_Module_namespace = (Core.Name "namespace")

_Module_elements = (Core.Name "elements")

_Module_termDependencies = (Core.Name "termDependencies")

_Module_typeDependencies = (Core.Name "typeDependencies")

_Module_description = (Core.Name "description")

-- | A prefix for element names
newtype Namespace = 
  Namespace {
    unNamespace :: String}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra.module.Namespace")

-- | A mapping from namespaces to values of type n, with a focus on one namespace
data Namespaces n = 
  Namespaces {
    namespacesFocus :: (Namespace, n),
    namespacesMapping :: (M.Map Namespace n)}
  deriving (Eq, Ord, Read, Show)

_Namespaces = (Core.Name "hydra.module.Namespaces")

_Namespaces_focus = (Core.Name "focus")

_Namespaces_mapping = (Core.Name "mapping")

-- | A qualified name consisting of an optional namespace together with a mandatory local name
data QualifiedName = 
  QualifiedName {
    qualifiedNameNamespace :: (Maybe Namespace),
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra.module.QualifiedName")

_QualifiedName_namespace = (Core.Name "namespace")

_QualifiedName_local = (Core.Name "local")

-- | A term-level definition, including a name, a term, and the type of the term
data TermDefinition = 
  TermDefinition {
    termDefinitionName :: Core.Name,
    termDefinitionTerm :: Core.Term,
    termDefinitionType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TermDefinition = (Core.Name "hydra.module.TermDefinition")

_TermDefinition_name = (Core.Name "name")

_TermDefinition_term = (Core.Name "term")

_TermDefinition_type = (Core.Name "type")

-- | A type-level definition, including a name and the type
data TypeDefinition = 
  TypeDefinition {
    typeDefinitionName :: Core.Name,
    typeDefinitionType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeDefinition = (Core.Name "hydra.module.TypeDefinition")

_TypeDefinition_name = (Core.Name "name")

_TypeDefinition_type = (Core.Name "type")
