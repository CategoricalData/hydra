-- Note: this is an automatically generated file. Do not edit.
-- | A model for Hydra module names, modules, and packages

module Hydra.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A definition, which may be either a term, type, or primitive definition
data Definition =
  -- | A term definition
  DefinitionTerm TermDefinition |
  -- | A type definition
  DefinitionType TypeDefinition |
  -- | A primitive definition
  DefinitionPrimitive PrimitiveDefinition
  deriving (Eq, Ord, Read, Show)
_Definition = Core.Name "hydra.packaging.Definition"
_Definition_term = Core.Name "term"
_Definition_type = Core.Name "type"
_Definition_primitive = Core.Name "primitive"
-- | A typed reference to a definition: a type, a term, or a primitive, identified by name
data DefinitionReference =
  -- | A reference to a type definition, by name
  DefinitionReferenceType Core.Name |
  -- | A reference to a term definition, by name
  DefinitionReferenceTerm Core.Name |
  -- | A reference to a primitive definition, by name
  DefinitionReferencePrimitive Core.Name
  deriving (Eq, Ord, Read, Show)
_DefinitionReference = Core.Name "hydra.packaging.DefinitionReference"
_DefinitionReference_type = Core.Name "type"
_DefinitionReference_term = Core.Name "term"
_DefinitionReference_primitive = Core.Name "primitive"
-- | The scope in which a package dependency is required. Relevant mainly to dependencies on third-party (non-Hydra) artifacts, where build systems distinguish compile-time, runtime, test-only, and build-tool-only dependencies. Hydra inter-package dependencies normally leave the scope unspecified.
data DependencyScope =
  -- | A dependency exported transitively to consumers of this package (e.g. Gradle `api`, Cabal `build-depends`): present at both compile time and runtime, and visible downstream
  DependencyScopeApi |
  -- | A dependency required at runtime but not at compile time
  DependencyScopeRuntime |
  -- | A dependency required only to compile and run the package's tests
  DependencyScopeTest |
  -- | A build-tool-only dependency, used to generate or process sources at build time but not present in the compiled artifact (e.g. an ANTLR tool jar or an annotation processor)
  DependencyScopeTool
  deriving (Eq, Ord, Read, Show)
_DependencyScope = Core.Name "hydra.packaging.DependencyScope"
_DependencyScope_api = Core.Name "api"
_DependencyScope_runtime = Core.Name "runtime"
_DependencyScope_test = Core.Name "test"
_DependencyScope_tool = Core.Name "tool"
-- | Documentation and lifecycle metadata attachable to a packaging entity (package, module, or definition). Bundling these fields in one type lets future metadata be added without changing the field shape of the entities that carry it.
data EntityMetadata =
  EntityMetadata {
    -- | An optional, concise one-line human-readable summary of the entity.
    entityMetadataDescription :: (Maybe String),
    -- | Zero or more long-form prose paragraphs: cross-cutting semantic conventions, caveats, and references that would otherwise be repeated across the entity's constituents.
    entityMetadataComments :: [String],
    -- | Typed cross-references to related entities, for navigation and documentation.
    entityMetadataSeeAlso :: [EntityReference],
    -- | Optional version-lifecycle milestones for the entity.
    entityMetadataLifecycle :: (Maybe LifecycleInfo)}
  deriving (Eq, Ord, Read, Show)
_EntityMetadata = Core.Name "hydra.packaging.EntityMetadata"
_EntityMetadata_description = Core.Name "description"
_EntityMetadata_comments = Core.Name "comments"
_EntityMetadata_seeAlso = Core.Name "seeAlso"
_EntityMetadata_lifecycle = Core.Name "lifecycle"
-- | A typed reference to a packaging entity: a package, a module, or a definition
data EntityReference =
  -- | A reference to a package, by name
  EntityReferencePackage PackageName |
  -- | A reference to a module, by name
  EntityReferenceModule ModuleName |
  -- | A reference to a definition (type, term, or primitive)
  EntityReferenceDefinition DefinitionReference
  deriving (Eq, Ord, Read, Show)
_EntityReference = Core.Name "hydra.packaging.EntityReference"
_EntityReference_package = Core.Name "package"
_EntityReference_module = Core.Name "module"
_EntityReference_definition = Core.Name "definition"
-- | Version-lifecycle milestones for a packaging entity. Each milestone is independently optional; further milestones (e.g. stableSince, removedSince) may be added without changing dependent types.
data LifecycleInfo =
  LifecycleInfo {
    -- | The version in which the entity was introduced, if known.
    lifecycleInfoAvailableSince :: (Maybe Version),
    -- | The version in which the entity was deprecated, if applicable.
    lifecycleInfoDeprecatedSince :: (Maybe Version)}
  deriving (Eq, Ord, Read, Show)
_LifecycleInfo = Core.Name "hydra.packaging.LifecycleInfo"
_LifecycleInfo_availableSince = Core.Name "availableSince"
_LifecycleInfo_deprecatedSince = Core.Name "deprecatedSince"
-- | A logical collection of elements sharing a common module name, having dependencies on zero or more other modules
data Module =
  Module {
    -- | The name of the module, which is also the common prefix for all element names in the module
    moduleName :: ModuleName,
    -- | Optional documentation and lifecycle metadata for the module
    moduleMetadata :: (Maybe EntityMetadata),
    -- | Any modules which this module directly depends on
    moduleDependencies :: [ModuleDependency],
    -- | The definitions in this module
    moduleDefinitions :: [Definition]}
  deriving (Eq, Ord, Read, Show)
_Module = Core.Name "hydra.packaging.Module"
_Module_name = Core.Name "name"
_Module_metadata = Core.Name "metadata"
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
    -- | Optional documentation and lifecycle metadata for the package
    packageMetadata :: (Maybe EntityMetadata),
    -- | The packages which this package depends on
    packageDependencies :: [PackageDependency],
    -- | The modules in this package
    packageModules :: [Module]}
  deriving (Eq, Ord, Read, Show)
_Package = Core.Name "hydra.packaging.Package"
_Package_name = Core.Name "name"
_Package_metadata = Core.Name "metadata"
_Package_dependencies = Core.Name "dependencies"
_Package_modules = Core.Name "modules"
-- | A dependency on another package, identified by name and constrained by an optional version specifier
data PackageDependency =
  PackageDependency {
    -- | The name of the depended-on package
    packageDependencyName :: PackageName,
    -- | The version-range constraint on the depended-on package
    packageDependencyVersion :: VersionSpecifier,
    -- | The scope in which the dependency is required, if specified. Normally absent for Hydra inter-package dependencies; specified for third-party dependencies whose build scope (compile/runtime/test/tool) is significant.
    packageDependencyScope :: (Maybe DependencyScope)}
  deriving (Eq, Ord, Read, Show)
_PackageDependency = Core.Name "hydra.packaging.PackageDependency"
_PackageDependency_name = Core.Name "name"
_PackageDependency_version = Core.Name "version"
_PackageDependency_scope = Core.Name "scope"
-- | The unique name of a package, e.g. "hydra-kernel" or "hydra-python". For dependencies on third-party artifacts in ecosystems with a group/namespace component (notably Maven), the group and artifact are carried in a single name separated by a colon, e.g. "org.eclipse.rdf4j:rdf4j-rio-ntriples"; the consuming host splits on the colon to recover the group when emitting build configuration. Hydra package names contain no colon.
newtype PackageName =
  PackageName {
    unPackageName :: String}
  deriving (Eq, Ord, Read, Show)
_PackageName = Core.Name "hydra.packaging.PackageName"
-- | A primitive definition: the universal, host-independent declarative metadata for a primitive, including name, signature, documentation and lifecycle metadata, totality and purity flags, and an optional default implementation expressed as a Hydra term.
data PrimitiveDefinition =
  PrimitiveDefinition {
    -- | The name of the primitive
    primitiveDefinitionName :: Core.Name,
    -- | Optional documentation and lifecycle metadata for the primitive.
    primitiveDefinitionMetadata :: (Maybe EntityMetadata),
    -- | The signature of the primitive. Always explicit, never inferred.
    primitiveDefinitionSignature :: Typing.TermSignature,
    -- | Whether the primitive is pure (referentially transparent, no observable side effects). Normally true.
    primitiveDefinitionIsPure :: Bool,
    -- | Whether the primitive is total (terminates on every input of its declared type). Normally true.
    primitiveDefinitionIsTotal :: Bool,
    -- | An optional cross-compilable reference implementation of the primitive, expressed as a Hydra term. Used by interpreters lacking a native implementation and as a proof-friendly reference. Distinct from the per-host Primitive.implementation.
    primitiveDefinitionDefaultImplementation :: (Maybe Core.Term)}
  deriving (Eq, Ord, Read, Show)
_PrimitiveDefinition = Core.Name "hydra.packaging.PrimitiveDefinition"
_PrimitiveDefinition_name = Core.Name "name"
_PrimitiveDefinition_metadata = Core.Name "metadata"
_PrimitiveDefinition_signature = Core.Name "signature"
_PrimitiveDefinition_isPure = Core.Name "isPure"
_PrimitiveDefinition_isTotal = Core.Name "isTotal"
_PrimitiveDefinition_defaultImplementation = Core.Name "defaultImplementation"
-- | A term-level definition, including a name, an optional signature, and a term
data TermDefinition =
  TermDefinition {
    -- | The name of the term
    termDefinitionName :: Core.Name,
    -- | Optional documentation and lifecycle metadata for the term definition
    termDefinitionMetadata :: (Maybe EntityMetadata),
    -- | The optional signature of the term. When absent, the signature has yet to be inferred.
    termDefinitionSignature :: (Maybe Typing.TermSignature),
    -- | The term being defined
    termDefinitionBody :: Core.Term}
  deriving (Eq, Ord, Read, Show)
_TermDefinition = Core.Name "hydra.packaging.TermDefinition"
_TermDefinition_name = Core.Name "name"
_TermDefinition_metadata = Core.Name "metadata"
_TermDefinition_signature = Core.Name "signature"
_TermDefinition_body = Core.Name "body"
-- | A type-level definition, including a name and the type scheme
data TypeDefinition =
  TypeDefinition {
    -- | The name of the type
    typeDefinitionName :: Core.Name,
    -- | Optional documentation and lifecycle metadata for the type definition
    typeDefinitionMetadata :: (Maybe EntityMetadata),
    -- | The type scheme being defined
    typeDefinitionBody :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)
_TypeDefinition = Core.Name "hydra.packaging.TypeDefinition"
_TypeDefinition_name = Core.Name "name"
_TypeDefinition_metadata = Core.Name "metadata"
_TypeDefinition_body = Core.Name "body"
-- | A version string, e.g. "0.15" or "1.0.0".
newtype Version =
  Version {
    unVersion :: String}
  deriving (Eq, Ord, Read, Show)
_Version = Core.Name "hydra.packaging.Version"
-- | A specifier constraining acceptable versions of a dependency. The `any` and `exact` variants are defined; future variants such as `caret` and `range` may be added without breaking consumers of the existing forms.
data VersionSpecifier =
  -- | Any version satisfies the dependency
  VersionSpecifierAny |
  -- | Exactly the given version satisfies the dependency; used to pin a specific release
  VersionSpecifierExact Version
  deriving (Eq, Ord, Read, Show)
_VersionSpecifier = Core.Name "hydra.packaging.VersionSpecifier"
_VersionSpecifier_any = Core.Name "any"
_VersionSpecifier_exact = Core.Name "exact"
