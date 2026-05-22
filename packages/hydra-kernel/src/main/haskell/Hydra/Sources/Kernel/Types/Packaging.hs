module Hydra.Sources.Kernel.Types.Packaging where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (packageName, packageModules, packageDependencies, packageDescription, primitiveDefinition)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Typing as Typing


ns :: Namespace
ns = Namespace "hydra.packaging"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = [Core.ns, Typing.ns],
            moduleDescription = Just "A model for Hydra namespaces, modules, and packages"}
  where
    definitions = [
      definition,
      fileExtension,
      module',
      namespace,
      package,
      packageName,
      primitiveDefinition,
      qualifiedName,
      termDefinition,
      typeDefinition]

definition :: Binding
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

fileExtension :: Binding
fileExtension = define "FileExtension" $
  doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
  T.wrap T.string

module' :: Binding
module' = define "Module" $
  doc "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" $
  T.record [
    "description">:
      doc "An optional human-readable description of the module" $
      T.maybe T.string,
    "namespace">:
      doc "A common prefix for all element names in the module"
      namespace,
    "dependencies">:
      doc "Any modules which this module directly depends on" $
      T.list namespace,
    "definitions">:
      doc "The definitions in this module" $
      T.list definition]

namespace :: Binding
namespace = define "Namespace" $
  doc "A prefix for element names" $
  T.wrap T.string

package :: Binding
package = define "Package" $
  doc "A package, which is a named collection of modules with metadata and dependencies" $
  T.record [
    "name">:
      doc "The name of the package"
      packageName,
    "modules">:
      doc "The modules in this package" $
      T.list module',
    "dependencies">:
      doc "The packages which this package depends on" $
      T.list packageName,
    "description">:
      doc "An optional human-readable description of the package" $
      T.maybe T.string]

packageName :: Binding
packageName = define "PackageName" $
  doc "The unique name of a package, e.g. \"hydra-kernel\" or \"hydra-python\"" $
  T.wrap T.string

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  doc "A qualified name consisting of an optional namespace together with a mandatory local name" $
  T.record [
    "namespace">:
      doc "The optional namespace" $
      T.maybe namespace,
    "local">:
      doc "The local name"
      T.string]

primitiveDefinition :: Binding
primitiveDefinition = define "PrimitiveDefinition" $
  doc "A primitive definition: the universal, host-independent declarative metadata for a primitive, including name, description, signature, totality and purity flags, and an optional default implementation expressed as a Hydra term." $
  T.record [
    "name">:
      doc "The name of the primitive"
      Core.name,
    "description">:
      doc "A human-readable description of the primitive"
      T.string,
    "signature">:
      doc "The signature of the primitive (always explicit, never inferred)"
      Typing.termSignature,
    "isPure">:
      doc "Whether the primitive is pure (referentially transparent, no observable side effects). Defaults to true."
      T.boolean,
    "isTotal">:
      doc "Whether the primitive is total (terminates on every input of its declared type). Defaults to true."
      T.boolean,
    "defaultImplementation">:
      doc "An optional cross-compilable reference implementation of the primitive, expressed as a Hydra term. Used by interpreters lacking a native implementation and as a proof-friendly reference. Distinct from the per-host Primitive.implementation." $
      T.maybe Core.term]

termDefinition :: Binding
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

typeDefinition :: Binding
typeDefinition = define "TypeDefinition" $
  doc "A type-level definition, including a name and the type scheme" $
  T.record [
    "name">:
      doc "The name of the type"
      Core.name,
    "typeScheme">:
      doc "The type scheme being defined"
      Core.typeScheme]
