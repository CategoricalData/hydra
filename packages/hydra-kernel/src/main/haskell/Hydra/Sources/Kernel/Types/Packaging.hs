module Hydra.Sources.Kernel.Types.Packaging where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (packageName, packageModules, packageDependencies, packageDescription, primitiveDefinition)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Typing as Typing


ns :: ModuleName
ns = ModuleName "hydra.packaging"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, Typing.ns],
            moduleMetadata = descriptionMetadata (Just "A model for Hydra module names, modules, and packages")}
  where
    definitions = [
      definition,
      definitionReference,
      entityMetadata,
      entityReference,
      lifecycleInfo,
      module',
      moduleDependency,
      moduleNameDef,
      package,
      packageDependency,
      packageName,
      primitiveDefinition,
      termDefinition,
      typeDefinition,
      version,
      versionSpecifier]

definition :: TypeDefinition
definition = define "Definition" $
  doc "A definition, which may be either a term, type, or primitive definition" $
  T.union [
    "term">:
      doc "A term definition"
      termDefinition,
    "type">:
      doc "A type definition"
      typeDefinition,
    "primitive">:
      doc "A primitive definition"
      primitiveDefinition]

definitionReference :: TypeDefinition
definitionReference = define "DefinitionReference" $
  doc "A typed reference to a definition: a type, a term, or a primitive, identified by name" $
  T.union [
    "type">:
      doc "A reference to a type definition, by name"
      Core.name,
    "term">:
      doc "A reference to a term definition, by name"
      Core.name,
    "primitive">:
      doc "A reference to a primitive definition, by name"
      Core.name]

entityMetadata :: TypeDefinition
entityMetadata = define "EntityMetadata" $
  doc ("Documentation and lifecycle metadata attachable to a packaging entity (package, module, or definition)."
    ++ " Bundling these fields in one type lets future metadata be added without changing the field shape of"
    ++ " the entities that carry it.") $
  T.record [
    "description">:
      doc "An optional, concise one-line human-readable summary of the entity." $
      T.optional T.string,
    "comments">:
      doc ("Zero or more long-form prose paragraphs: cross-cutting semantic conventions, caveats, and"
        ++ " references that would otherwise be repeated across the entity's constituents.") $
      T.list T.string,
    "seeAlso">:
      doc "Typed cross-references to related entities, for navigation and documentation." $
      T.list entityReference,
    "lifecycle">:
      doc "Optional version-lifecycle milestones for the entity." $
      T.optional lifecycleInfo]

entityReference :: TypeDefinition
entityReference = define "EntityReference" $
  doc "A typed reference to a packaging entity: a package, a module, or a definition" $
  T.union [
    "package">:
      doc "A reference to a package, by name"
      packageName,
    "module">:
      doc "A reference to a module, by name"
      moduleNameDef,
    "definition">:
      doc "A reference to a definition (type, term, or primitive)"
      definitionReference]

lifecycleInfo :: TypeDefinition
lifecycleInfo = define "LifecycleInfo" $
  doc ("Version-lifecycle milestones for a packaging entity. Each milestone is independently optional;"
    ++ " further milestones (e.g. stableSince, removedSince) may be added without changing dependent types.") $
  T.record [
    "availableSince">:
      doc "The version in which the entity was introduced, if known." $
      T.optional version,
    "deprecatedSince">:
      doc "The version in which the entity was deprecated, if applicable." $
      T.optional version]

module' :: TypeDefinition
module' = define "Module" $
  doc "A logical collection of elements sharing a common module name, having dependencies on zero or more other modules" $
  T.record [
    "name">:
      doc "The name of the module, which is also the common prefix for all element names in the module"
      moduleNameDef,
    "metadata">:
      doc "Optional documentation and lifecycle metadata for the module" $
      T.optional entityMetadata,
    "dependencies">:
      doc "Any modules which this module directly depends on" $
      T.list moduleDependency,
    "definitions">:
      doc "The definitions in this module" $
      T.list definition]

moduleDependency :: TypeDefinition
moduleDependency = define "ModuleDependency" $
  doc ("A dependency on another module, identified by its name and"
    ++ " (optionally) the package which provides it. When the package is omitted,"
    ++ " the resolver searches all packages in scope; a duplicate module name"
    ++ " across packages is a resolution error which can be disambiguated by"
    ++ " naming the intended package explicitly.") $
  T.record [
    "module">:
      doc "The name of the depended-on module"
      moduleNameDef,
    "package">:
      doc "The package providing the depended-on module, if disambiguation is required" $
      T.optional packageName]

moduleNameDef :: TypeDefinition
moduleNameDef = define "ModuleName" $
  doc "The unique name of a module; a prefix for the names of elements defined in the module." $
  T.wrap T.string

package :: TypeDefinition
package = define "Package" $
  doc "A package, which is a named collection of modules with metadata and dependencies" $
  T.record [
    "name">:
      doc "The name of the package"
      packageName,
    "metadata">:
      doc "Optional documentation and lifecycle metadata for the package" $
      T.optional entityMetadata,
    "dependencies">:
      doc "The packages which this package depends on" $
      T.list packageDependency,
    "modules">:
      doc "The modules in this package" $
      T.list module']

packageDependency :: TypeDefinition
packageDependency = define "PackageDependency" $
  doc "A dependency on another package, identified by name and constrained by an optional version specifier" $
  T.record [
    "name">:
      doc "The name of the depended-on package"
      packageName,
    "version">:
      doc "The version-range constraint on the depended-on package"
      versionSpecifier]

packageName :: TypeDefinition
packageName = define "PackageName" $
  doc "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"" $
  T.wrap T.string

primitiveDefinition :: TypeDefinition
primitiveDefinition = define "PrimitiveDefinition" $
  doc "A primitive definition: the universal, host-independent declarative metadata for a primitive, including name, signature, documentation and lifecycle metadata, totality and purity flags, and an optional default implementation expressed as a Hydra term." $
  T.record [
    "name">:
      doc "The name of the primitive"
      Core.name,
    "metadata">:
      doc "Optional documentation and lifecycle metadata for the primitive." $
      T.optional entityMetadata,
    "signature">:
      doc "The signature of the primitive. Always explicit, never inferred."
      Typing.termSignature,
    "isPure">:
      doc "Whether the primitive is pure (referentially transparent, no observable side effects). Normally true."
      T.boolean,
    "isTotal">:
      doc "Whether the primitive is total (terminates on every input of its declared type). Normally true."
      T.boolean,
    "defaultImplementation">:
      doc "An optional cross-compilable reference implementation of the primitive, expressed as a Hydra term. Used by interpreters lacking a native implementation and as a proof-friendly reference. Distinct from the per-host Primitive.implementation." $
      T.optional Core.term]

termDefinition :: TypeDefinition
termDefinition = define "TermDefinition" $
  doc "A term-level definition, including a name, an optional signature, and a term" $
  T.record [
    "name">:
      doc "The name of the term"
      Core.name,
    "metadata">:
      doc "Optional documentation and lifecycle metadata for the term definition" $
      T.optional entityMetadata,
    "signature">:
      doc "The optional signature of the term. When absent, the signature has yet to be inferred." $
      T.optional Typing.termSignature,
    "body">:
      doc "The term being defined"
      Core.term]

typeDefinition :: TypeDefinition
typeDefinition = define "TypeDefinition" $
  doc "A type-level definition, including a name and the type scheme" $
  T.record [
    "name">:
      doc "The name of the type"
      Core.name,
    "metadata">:
      doc "Optional documentation and lifecycle metadata for the type definition" $
      T.optional entityMetadata,
    "body">:
      doc "The type scheme being defined"
      Core.typeScheme]

version :: TypeDefinition
version = define "Version" $
  doc "A version string, e.g. \"0.15\" or \"1.0.0\"." $
  T.wrap T.string

versionSpecifier :: TypeDefinition
versionSpecifier = define "VersionSpecifier" $
  doc ("A specifier constraining acceptable versions of a dependency."
    ++ " Currently only the `any` (unit) specifier is defined; future variants"
    ++ " such as `exact`, `caret`, and `range` may be added without breaking"
    ++ " consumers of the `any` form.") $
  T.union [
    "any">:
      doc "Any version satisfies the dependency" $
      T.unit]
