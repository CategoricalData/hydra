-- Note: this is an automatically generated file. Do not edit.
-- | A model for Hydra namespaces, modules, and packages

module Hydra.Packaging where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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
-- | A logical collection of elements in the same namespace, having dependencies on zero or more other modules
data Module =
  Module {
    -- | An optional human-readable description of the module
    moduleDescription :: (Maybe String),
    -- | The name of the module, which is also the common prefix for all element names in the module
    moduleName :: ModuleName,
    -- | Any modules which this module directly depends on
    moduleDependencies :: [ModuleDependency],
    -- | The definitions in this module
    moduleDefinitions :: [Definition]}
  deriving (Eq, Ord, Read, Show)
_Module = Core.Name "hydra.packaging.Module"
_Module_description = Core.Name "description"
_Module_name = Core.Name "name"
_Module_dependencies = Core.Name "dependencies"
_Module_definitions = Core.Name "definitions"
-- | A dependency on another module, identified by its name and (optionally) the package which provides it. When the package is omitted, the resolver searches all packages in scope; a duplicate module name across packages is a resolution error which can be disambiguated by naming the intended package explicitly.
data ModuleDependency =
  ModuleDependency {
    -- | The name of the depended-on module
    moduleDependencyModule :: ModuleName,
    -- | The package providing the depended-on module, if disambiguation is required
    moduleDependencyPackage :: (Maybe PackageName)}
  deriving (Eq, Ord, Read, Show)
_ModuleDependency = Core.Name "hydra.packaging.ModuleDependency"
_ModuleDependency_module = Core.Name "module"
_ModuleDependency_package = Core.Name "package"
-- | The unique name of a module; a prefix for the names of elements defined in the module.
newtype ModuleName =
  ModuleName {
    unModuleName :: String}
  deriving (Eq, Ord, Read, Show)
_ModuleName = Core.Name "hydra.packaging.ModuleName"
-- | A package, which is a named collection of modules with metadata and dependencies
data Package =
  Package {
    -- | The name of the package
    packageName :: PackageName,
    -- | The modules in this package
    packageModules :: [Module],
    -- | The packages which this package depends on
    packageDependencies :: [PackageDependency],
    -- | An optional human-readable description of the package
    packageDescription :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_Package = Core.Name "hydra.packaging.Package"
_Package_name = Core.Name "name"
_Package_modules = Core.Name "modules"
_Package_dependencies = Core.Name "dependencies"
_Package_description = Core.Name "description"
-- | A dependency on another package, identified by name and constrained by an optional version specifier
data PackageDependency =
  PackageDependency {
    -- | The name of the depended-on package
    packageDependencyName :: PackageName,
    -- | The version-range constraint on the depended-on package
    packageDependencyVersion :: PackageVersionSpecifier}
  deriving (Eq, Ord, Read, Show)
_PackageDependency = Core.Name "hydra.packaging.PackageDependency"
_PackageDependency_name = Core.Name "name"
_PackageDependency_version = Core.Name "version"
-- | The unique name of a package, e.g. "hydra-kernel" or "hydra-python"
newtype PackageName =
  PackageName {
    unPackageName :: String}
  deriving (Eq, Ord, Read, Show)
_PackageName = Core.Name "hydra.packaging.PackageName"
-- | A specifier constraining acceptable versions of a depended-on package. Currently only the `any` (unit) specifier is defined; future variants such as `exact`, `caret`, and `range` may be added without breaking consumers of the `any` form.
data PackageVersionSpecifier =
  -- | Any version of the package satisfies the dependency
  PackageVersionSpecifierAny
  deriving (Eq, Ord, Read, Show)
_PackageVersionSpecifier = Core.Name "hydra.packaging.PackageVersionSpecifier"
_PackageVersionSpecifier_any = Core.Name "any"
-- | A qualified name consisting of an optional module name together with a mandatory local name
data QualifiedName =
  QualifiedName {
    -- | The optional module name
    qualifiedNameModuleName :: (Maybe ModuleName),
    -- | The local name
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)
_QualifiedName = Core.Name "hydra.packaging.QualifiedName"
_QualifiedName_moduleName = Core.Name "moduleName"
_QualifiedName_local = Core.Name "local"
-- | A term-level definition, including a name, a term, and the type scheme of the term
data TermDefinition =
  TermDefinition {
    -- | The name of the term
    termDefinitionName :: Core.Name,
    -- | The term being defined
    termDefinitionTerm :: Core.Term,
    -- | The type scheme of the term, including any class constraints
    termDefinitionTypeScheme :: (Maybe Core.TypeScheme)}
  deriving (Eq, Ord, Read, Show)
_TermDefinition = Core.Name "hydra.packaging.TermDefinition"
_TermDefinition_name = Core.Name "name"
_TermDefinition_term = Core.Name "term"
_TermDefinition_typeScheme = Core.Name "typeScheme"
-- | A type-level definition, including a name and the type scheme
data TypeDefinition =
  TypeDefinition {
    -- | The name of the type
    typeDefinitionName :: Core.Name,
    -- | The type scheme being defined
    typeDefinitionTypeScheme :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)
_TypeDefinition = Core.Name "hydra.packaging.TypeDefinition"
_TypeDefinition_name = Core.Name "name"
_TypeDefinition_typeScheme = Core.Name "typeScheme"
