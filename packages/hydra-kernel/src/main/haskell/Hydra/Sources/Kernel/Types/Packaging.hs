module Hydra.Sources.Kernel.Types.Packaging where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (packageName, packageModules, packageDependencies, packageDescription)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Graph as Graph


ns :: Namespace
ns = Namespace "hydra.packaging"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [Graph.ns],
            moduleTypeDependencies = [Core.ns],
            moduleDescription = Just "A model for Hydra namespaces, modules, and packages"}
  where
    definitions = [
      definition,
      fileExtension,
      library,
      module',
      namespace,
      namespaces,
      package,
      packageName,
      qualifiedName,
      termDefinition,
      typeDefinition]

definition :: Binding
definition = define "Definition" $
  doc "A definition, which may be either a term or type definition" $
  T.union [
    "term">:
      doc "A term definition"
      termDefinition,
    "type">:
      doc "A type definition"
      typeDefinition]

fileExtension :: Binding
fileExtension = define "FileExtension" $
  doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
  T.wrap T.string

library :: Binding
library = define "Library" $
  doc "A library of primitive functions" $
  T.record [
    "namespace">:
      doc "A common prefix for all primitive function names in the library"
      namespace,
    "prefix">:
      doc "A preferred namespace prefix for function names in the library"
      T.string,
    "primitives">:
      doc "The primitives defined in this library" $
      T.list Graph.primitive]

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
    "termDependencies">:
      doc "Any modules which the term expressions of this module directly depend upon" $
      T.list namespace,
    "typeDependencies">:
      doc "Any modules which the type expressions of this module directly depend upon" $
      T.list namespace,
    "definitions">:
      doc "The definitions in this module" $
      T.list definition]

namespace :: Binding
namespace = define "Namespace" $
  doc "A prefix for element names" $
  T.wrap T.string

namespaces :: Binding
namespaces = define "Namespaces" $
  doc "A mapping from namespaces to values of type n, with a focus on one namespace" $
  T.forAll "n" $ T.record [
    "focus">:
      doc "The namespace in focus, together with its associated value" $
      T.pair namespace "n",
    "mapping">:
      doc "A mapping of namespaces to values" $
      T.map namespace "n"]

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

termDefinition :: Binding
termDefinition = define "TermDefinition" $
  doc "A term-level definition, including a name, a term, and the type scheme of the term" $
  T.record [
    "name">:
      doc "The name of the term"
      Core.name,
    "term">:
      doc "The term being defined"
      Core.term,
    "typeScheme">:
      doc "The type scheme of the term, including any class constraints" $
      T.maybe Core.typeScheme]

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
