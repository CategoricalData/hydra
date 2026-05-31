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
            moduleDescription = Just "A model for Hydra module names, modules, and packages"}
  where
    definitions = [
      definition,
      fileExtension,
      module',
      moduleDependency,
      moduleNameDef,
      package,
      packageDependency,
      packageName,
      packageVersionSpecifier,
      primitiveDefinition,
      qualifiedName,
      termDefinition,
      typeDefinition,
      version]

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

fileExtension :: TypeDefinition
fileExtension = define "FileExtension" $
  doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
  T.wrap T.string

module' :: TypeDefinition
module' = define "Module" $
  doc "A logical collection of elements sharing a common module name, having dependencies on zero or more other modules" $
  T.record [
    "name">:
      doc "The name of the module, which is also the common prefix for all element names in the module"
      moduleNameDef,
    "description">:
      doc "An optional human-readable description of the module" $
      T.maybe T.string,
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
      T.maybe packageName]

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
    "description">:
      doc "An optional human-readable description of the package" $
      T.maybe T.string,
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
      packageVersionSpecifier]

packageName :: TypeDefinition
packageName = define "PackageName" $
  doc "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"" $
  T.wrap T.string

packageVersionSpecifier :: TypeDefinition
packageVersionSpecifier = define "PackageVersionSpecifier" $
  doc ("A specifier constraining acceptable versions of a depended-on package."
    ++ " Currently only the `any` (unit) specifier is defined; future variants"
    ++ " such as `exact`, `caret`, and `range` may be added without breaking"
    ++ " consumers of the `any` form.") $
  T.union [
    "any">:
      doc "Any version of the package satisfies the dependency" $
      T.unit]

primitiveDefinition :: TypeDefinition
primitiveDefinition = define "PrimitiveDefinition" $
  doc "A primitive definition: the universal, host-independent declarative metadata for a primitive, including name, signature, description, long-form specification, cross-references, totality and purity flags, version metadata, and an optional default implementation expressed as a Hydra term." $
  T.record [
    "name">:
      doc "The name of the primitive"
      Core.name,
    "signature">:
      doc "The signature of the primitive (always explicit, never inferred)"
      Typing.termSignature,
    "description">:
      doc "A concise, one-sentence human-readable description of the primitive"
      T.string,
    "comments">:
      doc "A detailed, host-independent specification of the primitive's behavior. Used to capture constraints, edge cases, and semantic choices (e.g. floating-point sentinel behavior, numeric narrowing arithmetic, complexity expectations) that are not yet promoted to structured fields." $
      T.maybe T.string,
    "seeAlso">:
      doc "Names of related primitives, for navigation and documentation purposes." $
      T.list Core.name,
    "isPure">:
      doc "Whether the primitive is pure (referentially transparent, no observable side effects). Defaults to true."
      T.boolean,
    "isTotal">:
      doc "Whether the primitive is total (terminates on every input of its declared type). Defaults to true."
      T.boolean,
    "availableSince">:
      doc "The version in which the primitive was introduced, if known." $
      T.maybe version,
    "deprecatedSince">:
      doc "The version in which the primitive was deprecated, if applicable." $
      T.maybe version,
    "defaultImplementation">:
      doc "An optional cross-compilable reference implementation of the primitive, expressed as a Hydra term. Used by interpreters lacking a native implementation and as a proof-friendly reference. Distinct from the per-host Primitive.implementation." $
      T.maybe Core.term]

qualifiedName :: TypeDefinition
qualifiedName = define "QualifiedName" $
  doc "A qualified name consisting of an optional module name together with a mandatory local name" $
  T.record [
    "moduleName">:
      doc "The optional module name" $
      T.maybe moduleNameDef,
    "local">:
      doc "The local name"
      T.string]

termDefinition :: TypeDefinition
termDefinition = define "TermDefinition" $
  doc "A term-level definition, including a name, a term, and an optional signature" $
  T.record [
    "name">:
      doc "The name of the term"
      Core.name,
    "term">:
      doc "The term being defined"
      Core.term,
    "signature">:
      doc "The optional signature of the term. When absent, the signature is to be inferred." $
      T.maybe Typing.termSignature]

typeDefinition :: TypeDefinition
typeDefinition = define "TypeDefinition" $
  doc "A type-level definition, including a name and the type scheme" $
  T.record [
    "name">:
      doc "The name of the type"
      Core.name,
    "typeScheme">:
      doc "The type scheme being defined"
      Core.typeScheme]

version :: TypeDefinition
version = define "Version" $
  doc "A version string, e.g. \"0.15\" or \"1.0.0\"." $
  T.wrap T.string
